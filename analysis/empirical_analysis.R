#!/usr/bin/env Rscript

library(tidyverse)
library(datasets)
library(countrycode)
library(cowplot)
library(patchwork)

set.seed(77845) # for Mantel test

# Load US data --------------------------------------------------------

marketscan <- read_tsv("../data/marketscan/data.tsv")

nhsn <- read_tsv('../data/nhsn-ims/data.tsv') %>%
  mutate(
    bugdrug = 'Ec/q',
    use = rx_person_year,
    f_resistant = n_resistant / n_isolates
  ) %>%
  select(unit = state, bugdrug, use, f_resistant)

# Load European data --------------------------------------------------

# Conversion from DID (defined daily doses per 1k inhabitants per day)
# to CPY (claims per person per year)
did_cpy_map <- tibble(
  drug = c('beta_lactam', 'quinolone', 'macrolide'),
  ddd_per_tx = c(10, 10, 7),
  cpy_per_did = 365 / (1e3 * ddd_per_tx)
)

europe <- read_tsv('../data/ecdc/data.tsv') %>%
  mutate_at("country", ~ countrycode(., origin = "country.name", destination = "iso3c")) %>%
  left_join(did_cpy_map, by = 'drug') %>%
  mutate(use = cpy_per_did * did) %>%
  mutate(
    bugdrug = case_when(
      .$bug == 'Escherichia coli' & .$drug == 'quinolone' ~ 'Ec/q',
      .$bug == 'Streptococcus pneumoniae' & .$drug == 'beta_lactam' ~ 'Sp/bl',
      .$bug == 'Streptococcus pneumoniae' & .$drug == 'macrolide' ~ 'Sp/m'
    ),
    f_resistant = n_ns / n_isolates
  ) %>%
  select(unit = country, bugdrug, use, f_resistant)

eu_units <- unique(europe$unit)


# Build combined use/resistance dataset ---------------------------------------

unit_data <- tribble(
  ~setting, ~data_source, ~data,
  "US", "MarketScan/RO", marketscan,
  "US", "Xponent/NHSN", nhsn,
  "Europe", "ECDC", europe
) %>%
  unnest(cols = data) %>%
  mutate(dataset = str_c(data_source, ' ', bugdrug)) %>%
  select(dataset, setting, unit, f_resistant, use) %>%
  group_by(dataset) %>%
  nest() %>%
  ungroup() %>%
  mutate_at('dataset', fct_inorder)

# Use-resistance in different datasets ----------------------------------------

round_up <- function(x, digits) ceiling(x * 10 ** digits) / 10 ** digits
round_down <- function(x, digits) floor(x * 10 ** digits) / 10 ** digits

breaker <- function(digits) {
  function(x) {
    lower <- round_up(x[1], digits)
    upper <- round_down(x[2], digits)
    round(seq(lower, upper, length.out = 3), digits)
  }
}

obs_plot <- unit_data %>%
  unnest(cols = c(data)) %>%
  ggplot(aes(use, f_resistant)) +
  facet_wrap(~ dataset, scales = 'free') +
  geom_smooth(method = 'lm', color = 'gray50') +
  geom_point() +
  scale_x_continuous(
    expression(paste(
      "Antibiotic use ", tau, " (treatments per person per year)"
    )),
    breaks = breaker(2)
  ) +
  scale_y_continuous(
    expression(paste(
      "Resistance ", rho, " (proportion of isolates nonsusceptible)"
    ))
  ) +
  theme_cowplot() +
  theme(
    strip.background = element_blank(),
    plot.margin = margin(1, 5, 1, 1, 'mm')
  )

ggsave(
  "fig/cross_sectional.pdf", plot = obs_plot,
  width = 6.5, height = 5, unit = "in"
)

# Load adjacency and commuting data -------------------------------------------

read_adjacency_matrix <- function(fn) {
  read_tsv(fn) %>%
    rename(unit1 = from_unit) %>%
    pivot_longer(
      ., -unit1,
      names_to = "unit2", values_to = "adjacent"
    )
}

matrixify <- function(df) {
  stopifnot(all(names(df)[-1] == df$from_unit))
  
  mat <- df %>%
    select(-from_unit) %>%
    as.matrix
  
  rownames(mat) <- names(df)[-1]
  
  stopifnot(all(rownames(mat) == colnames(mat)))
  
  # make rows sum to 1
  mat <- mat %>%
    sweep(1, rowSums(.), "/")
    
  stopifnot(all(rowSums(mat) == 1))
  
  # symmetrize
  mat <- 0.5 * (mat + t(mat))
  stopifnot(all(mat == t(mat)))
    
  # set diagonal to maximum
  if (any(mat > 1)) stop("Entries over 1")
  diag(mat) <- 1
  
  mat
}

tibblify <- function(mat) {
  as_tibble(mat, rownames = "unit1") %>%
    pivot_longer(-unit1, names_to = "unit2", values_to = "interaction")
}

adjacency_db <- tribble(
  ~setting, ~fn,
  "US", "../db/us/adjacency.tsv",
  "Europe", "../db/europe/adjacency.tsv"
) %>%
  mutate(data = map(fn, read_adjacency_matrix)) %>%
  select(setting, data) %>%
  unnest(cols = data) %>%
  mutate_at("adjacent", as.logical)

# check that European adjacency has all the names we expect and no more
adjacency_db %>%
  filter(setting == "Europe") %>%
  { stopifnot(setequal(.$unit1, eu_units)) }

interactions_matrices <- tribble(
  ~setting, ~fn,
  "US", "../db/us/commuting.tsv",
  "Europe", "../db/europe/flights.tsv"
) %>%
  mutate(
    data = map(fn, read_tsv),
    matrix = map(data, matrixify)
  ) %>%
  select(setting, matrix)

interactions_tbl <- interactions_matrices %>% 
  mutate(tbl = map(matrix, tibblify)) %>%
  select(setting, tbl) %>%
  unnest(cols = tbl)

# Pairs data ------------------------------------------------------------------

cross_units <- function(df) {
  df %>%
    with(crossing(unit1 = unit, unit2 = unit)) %>%
    filter(unit1 < unit2) %>%
    left_join(rename_all(df, ~ str_c(., '1')), by = 'unit1') %>%
    left_join(rename_all(df, ~ str_c(., '2')), by = 'unit2') %>%
    left_join(adjacency_db, by = c("unit1", "unit2")) %>%
    left_join(interactions_tbl, by = c("unit1", "unit2")) %>%
    mutate(
      d_resistant = f_resistant1 - f_resistant2,
      d_use = use1 - use2,
      dr_du = d_resistant / d_use
    ) %>%
    select(unit1, unit2, dr_du, adjacent, interaction)
}

leave_one_out_from_cross <- function(df) {
  units <- unique(c(df$unit1, df$unit2))
  map(units, ~ filter(df, unit1 != ., unit2 != .))
}

cross_data <- unit_data %>%
  mutate(
    setting = map_chr(data, ~ unique(.$setting)),
    cross_data = map(data, cross_units),
    l1o_cross_data = map(cross_data, leave_one_out_from_cross)
  )

# Interactions histogram ------------------------------------------------------

histograms <- cross_data %>%
  select(setting, cross_data) %>%
  unnest(cols = cross_data) %>%
  select(setting, unit1, unit2, adjacent, interaction) %>%
  distinct() %>%
  filter(unit1 < unit2) %>%
  # convert to n-tiles
  group_by(setting) %>%
  mutate(x = ntile(interaction, 10)) %>%
  ungroup() %>%
  mutate(key = recode(
    setting,
    Europe = "European intercountry flights",
    US = "US interstate commuting"
  )) %>%
  ggplot(aes(x = factor(x), fill = adjacent)) +
  geom_bar(color = "black", width = 1.0) +
  facet_wrap(facets = vars(key), scales = "free_y") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(
    breaks = c(FALSE, TRUE),
    values = c("white", "black"),
    labels = c("no", "yes")
  ) +
  labs(
    x = "Interaction decile",
    y = "No. of population pairs",
    fill = "Adjacent?"
  ) +
  theme_cowplot() +
  theme(
    strip.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks.x = element_blank()
  )

histograms

ggsave('fig/interactions_histogram.pdf', plot = histograms)


# Adjacency analysis ----------------------------------------------------------

sigfig <- function(x, n = 2) {
  formatC(signif(x, digits = n), digits = n, format = "fg", flag = "#")
}

jackknife.sd <- function(x) {
  n <- length(x)
  sqrt((n - 1) / n * sum((x - mean(x)) ** 2))
}

test_f <- function(df) with(df, { wilcox.test(dr_du[adjacent], dr_du[!adjacent]) })
arr_estimate_f <- function(df) with(df, { median(dr_du[adjacent]) - median(dr_du[!adjacent]) })

# fractional change
fc_estimate_f <- function(df) {
  with(df, {
    (median(dr_du[adjacent]) - median(dr_du[!adjacent])) /
      median(dr_du[!adjacent])
  })
}

adjacency_results <- tibble(
  method = c("arr", "fc"),
  estimate_f = list(arr_estimate_f, fc_estimate_f)
) %>%
  crossing(cross_data) %>%
  mutate(
    estimate = map2_dbl(estimate_f, cross_data, ~ .x(.y)),
    l1o_estimates = map2(l1o_cross_data, estimate_f, ~ map_dbl(.x, .y)),
    se = map_dbl(l1o_estimates, jackknife.sd),
    lci = estimate + se * qnorm(0.05 / 2),
    uci = estimate + se * qnorm(1 - (0.05 / 2))
  ) %>%
  arrange(dataset, method) %>%
  select(dataset, method, estimate, lci, uci)

wilcoxon_results <- cross_data %>%
  mutate(
    test = map(cross_data, test_f),
    p = map_dbl(test, ~ .$p.value),
    sig = p.adjust(p, "BH") < 0.05
  ) %>%
  select(dataset, p, sig)

adjacency_table <- adjacency_results %>%
  mutate_if(is.numeric, ~ signif(., 2)) %>%
  mutate(label = as.character(str_glue("{estimate} ({lci} to {uci})"))) %>%
  select(dataset, method, label) %>%
  pivot_wider(names_from = method, values_from = label) %>%
  left_join(wilcoxon_results, by = "dataset") %>%
  mutate_if(is.numeric, ~ signif(., 2))

adjacency_table

write_tsv(adjacency_table, "results/adjacency-results.tsv")

# Commuting analysis --------------------------------------------------

long_to_matrix <- function(tbl, value) {
  nm <- sort(unique(c(tbl$unit1, tbl$unit2)))
  X <- matrix(NA, nrow = length(nm), ncol = length(nm))
  
  is <- match(tbl$unit1, nm)
  js <- match(tbl$unit2, nm)
  
  for (k in 1:nrow(tbl)) {
    X[is[k], js[k]] <- tbl[[value]][k]
    X[js[k], is[k]] <- tbl[[value]][k]
  }
  
  colnames(X) <- nm
  rownames(X) <- nm
  
  X
}

subset_by_names <- function(X, nm, rev = FALSE) {
  stopifnot(all(nm %in% rownames(X)))
  stopifnot(all(rownames(X) == colnames(X)))
  k <- match(nm, rownames(X))
  X[k, k]
}

rank_matrix <- function(X) {
  n <- dim(X)[1]
  # check that X is square
  stopifnot(n == dim(X)[2])
  
  # initialize blank matrix
  out <- matrix(NA, nrow = n, ncol = n)
  
  values <- rank(X[upper.tri(X)])
  out[upper.tri(X)] <- values
  out[lower.tri(X)] <- values
  diag(out) <- 0
  
  rownames(out) <- rownames(X)
  colnames(out) <- colnames(X)
  
  out
}

cor_f <- function(df) cor(df$dr_du, df$interaction, method = "spearman")

mantel_results <- cross_data %>%
  left_join(interactions_matrices, by = "setting") %>%
  mutate(
    # correlations
    estimate = map_dbl(cross_data, cor_f),
    l1o_estimates = map(l1o_cross_data, function(dfs) map_dbl(dfs, cor_f)),
    se = map_dbl(l1o_estimates, jackknife.sd),
    lci = estimate + se * qnorm(0.05 / 2),
    uci = estimate + se * qnorm(1 - (0.05 / 2)),
    # Mantel test
    Y = map(cross_data, ~ -rank_matrix(long_to_matrix(., "dr_du"))),
    X = map2(matrix, Y, ~ rank_matrix(subset_by_names(.x, rownames(.y)))),
    test = map2(X, Y, ~ vegan::mantel(.x, .y, method = "spearman")),
    estimate2 = map_dbl(test, ~ .$statistic),
    p = map_dbl(test, ~ .$signif),
    sig = p.adjust(p, "BH") < 0.05
  ) %>%
  { stopifnot(all(.$estimate == -.$estimate2)); . } %>%
  select(dataset, estimate, lci, uci, p, sig)

mantel_table <- mantel_results %>%
  mutate_if(is.numeric, ~ signif(., 2))

mantel_table

write_tsv(mantel_table, "results/mantel-results.tsv")

# Difference in dr/du by decile (compare least interaction=1 with most=10)
add_decile <- function(df) mutate(df, decile = ntile(interaction, 10))
decile_arr <- function(df) {
  add_decile(df) %>%
    with({
      median(dr_du[decile == 10]) - median(dr_du[decile == 1])
    })
}

decile_ratio <- function(df) {
  add_decile(df) %>%
    with({
      (median(dr_du[decile == 10]) - median(dr_du[decile == 1])) /
        median(dr_du[decile == 1])
    })
}

tile_results <- tibble(
  method = c("arr", "ratio"),
  estimate_f = list(decile_arr, decile_ratio)
) %>%
  crossing(cross_data) %>%
  mutate(
    estimate = map2_dbl(estimate_f, cross_data, ~ .x(.y)),
    l1o_estimates = map2(l1o_cross_data, estimate_f, ~ map_dbl(.x, .y)),
    se = map_dbl(l1o_estimates, jackknife.sd),
    lci = estimate + se * qnorm(0.05 / 2),
    uci = estimate + se * qnorm(1 - (0.05 / 2))
  ) %>%
  arrange(dataset, method) %>%
  select(dataset, method, estimate, lci, uci)

tile_table <- tile_results %>%
  mutate_if(is.numeric, ~ signif(., 2)) %>%
  mutate(label = as.character(str_glue("{estimate} ({lci} to {uci})"))) %>%
  select(dataset, method, label) %>%
  pivot_wider(names_from = method, values_from = label) %>%
  mutate_if(is.numeric, ~ signif(., 2))

write_tsv(tile_table, "results/tile-table.tsv")

# Plots -----------------------------------------------------------------------

dr_du_lab <- expression(
  paste('Use-resistance association ', (Delta * rho / Delta * tau))
)

boxplot_details_f <- function(df, ymin, ymax) {
  df %>%
    nest(data = c(unit1, unit2, dr_du, interaction)) %>%
    mutate(
      y = map(data, ~ .$dr_du),
      box = map(y, ~ boxplot.stats(.)$stats),
      boxplot_data = map(box, ~ tibble(
        ymin = max(.[1], ymin),
        lower = .[2],
        middle = .[3],
        upper = .[4],
        ymax = min(.[5], ymax)
      ))
    ) %>%
    select(adjacent, boxplot_data) %>%
    unnest(cols = c(boxplot_data))
}

f_to_keep <- 0.90
half_drop <- (1 - f_to_keep) / 2

boxplot_data <- cross_data %>%
  mutate(
    y = map(cross_data, ~ .$dr_du),
    ymin = map_dbl(y, ~ quantile(., half_drop)),
    ymax = map_dbl(y, ~ quantile(., 1 - half_drop)),
    boxplot_details = pmap(list(cross_data, ymin, ymax), boxplot_details_f),
    point_data = pmap(list(cross_data, ymin, ymax), ~ filter(..1, between(dr_du, ..2, ..3)))
  )

point_data <- boxplot_data %>%
  select(dataset, point_data) %>%
  unnest(cols = point_data)

boxplot_details <- boxplot_data %>%
  select(dataset, boxplot_details) %>%
  unnest(cols = boxplot_details)

adjacency_plot <- ggplot(data = NULL, aes(x = factor(adjacent))) +
  facet_wrap(~ dataset, scales = 'free_y') +
  geom_boxplot(
    data = boxplot_details,
    aes(lower = lower, upper = upper, middle = middle, ymin = ymin, ymax = ymax),
    stat = 'identity'
  ) +
  geom_jitter(
    data = point_data,
    aes(y = dr_du),
    shape = 1, size = 0.75,
    width = 0.3
  ) +
  scale_x_discrete(
    '',
    labels = c(`TRUE` = 'Adjacent', `FALSE` = 'Not adj.')
  ) +
  ylab(dr_du_lab) +
  theme_cowplot() +
  theme(strip.background = element_blank())

ggsave(
  'fig/adjacency_plot.pdf', plot = adjacency_plot,
  width = 7, height = 5, unit = "in"
)

# Interaction plot ----------------------------------------------------

sig_interactions <- mantel_results %>%
  filter(sig) %>%
  pull(dataset)

sig_labels <- levels(cross_data$dataset) %>%
  (function(x) {
    case_when(
      x %in% sig_interactions ~ str_c(x, " *"),
      TRUE ~ x
    )
  })

interaction_plot <- cross_data %>%
  select(dataset, cross_data) %>%
  # add * after names of significant interactions
  mutate_at("dataset", ~ factor(., labels = sig_labels)) %>%
  unnest(cols = cross_data) %>%
  # convert to ranks
  group_by(dataset) %>%
  mutate(x = rank(interaction)) %>%
  ungroup() %>%
  ggplot(aes(x, dr_du)) +
  facet_wrap(vars(dataset), scales = "free_x") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 1) +
  geom_smooth(method = MASS::rlm, se = FALSE, color = "red", linetype = 2) +
  theme_cowplot() +
  labs(
    x = "Interaction rank",
    y = dr_du_lab
  ) +
  coord_cartesian(ylim = 25 * c(-1, 1)) +
  theme(strip.background = element_blank())

interaction_plot

ggsave(
  "fig/interaction_plot.pdf", plot = interaction_plot,
  width = 7.5, height = 5, unit = "in"
)
