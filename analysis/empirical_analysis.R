#!/usr/bin/env Rscript

library(tidyverse)
library(datasets)
library(countrycode)
library(cowplot)
library(patchwork)
library(scales)

revalue <- function(x, from, to) to[match(x, from)]

# Load US data --------------------------------------------------------

marketscan_res <- read_tsv('../data/ms-medicare-ro/abg_state.tsv') %>%
  rename(drug = drug_group) %>%
  mutate(bugdrug = case_when(
    # don't do Sp/bl, as they have negative correlation
    .$bug == 'E. coli' & .$drug == 'quinolone' ~ 'Ec/q',
    .$bug == 'S. pneumoniae' & .$drug == 'macrolide' ~ 'Sp/m'
  )) %>%
  filter(!is.na(bugdrug)) %>%
  select(bugdrug, drug, state, f_resistant = f_ns)

marketscan_use <- read_tsv('../data/ms-medicare-ro/ineq_marketscan.tsv') %>%
  rename(drug = drug_group, use = total_use) %>%
  filter(drug %in% c('quinolone', 'macrolide'))

marketscan <- marketscan_use %>%
  inner_join(marketscan_res, by = c('drug', 'state')) %>%
  mutate_at("state", ~ revalue(., state.name, state.abb)) %>%
  select(unit = state, bugdrug, use, f_resistant)

nhsn <- read_tsv('../data/nhsn-ims/data.tsv') %>%
  filter(state %in% state.abb) %>%
  mutate(
    bugdrug = 'Ec/q',
    use = rx_1k_year / 1e3,
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
  ggplot(aes(use, f_resistant * 100)) +
  facet_wrap(~ dataset, scales = 'free') +
  geom_smooth(method = 'lm', color = 'gray50') +
  geom_point() +
  scale_x_continuous(
    'Treatments per person per year',
    breaks = breaker(2)
  ) +
  scale_y_continuous(
    name = 'Resistance (% isolates nonsusceptible)'
  ) +
  theme_cowplot() +
  theme(
    strip.background = element_blank(),
    plot.margin = margin(1, 5, 1, 1, 'mm')
  )

ggsave('fig/cross_sectional.pdf')

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
  diag(mat) <- max(mat)
  
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
    cross_data = map(data, cross_units),
    l1o_cross_data = map(cross_data, leave_one_out_from_cross)
  )

# Commuting histogram ---------------------------------------------------------

histograms <- cross_data %>%
  select(setting, cross_data) %>%
  unnest(cols = cross_data) %>%
  select(setting, unit1, unit2, adjacent, interaction) %>%
  distinct() %>%
  filter(unit1 < unit2) %>%
  mutate(key = recode(
    setting,
    Europe = "European intercountry flights",
    US = "US interstate commuting"
  )) %>%
  ggplot(aes(x = interaction, fill = adjacent)) +
  geom_histogram(na.rm = TRUE, color = "black") +
  facet_wrap(facets = vars(key), scales = "free") +
  scale_x_log10() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(
    breaks = c(FALSE, TRUE),
    values = c("white", "black"),
    labels = c("no", "yes")
  ) +
  labs(
    x = "Ad hoc interaction",
    y = "No. of unit pairs",
    fill = "Adjacent?"
  ) +
  theme_cowplot()

ggsave('fig/commuting_histogram.pdf')


# Adjacency analysis ----------------------------------------------------------

sigfig <- function(x, n = 2) {
  formatC(signif(x, digits = n), digits = n, format = "fg", flag = "#")
}

jackknife.sd <- function(x) {
  n <- length(x)
  sqrt((n - 1) / n * sum((x - mean(x)) ** 2))
}

analysis_f <- function(model_f, coef_f, ratio_f) {
  base_results <- cross_data %>%
    mutate(
      model = map(cross_data, model_f),
      coef = map_dbl(model, coef_f),
      ratio = map_dbl(model, ratio_f)
    ) %>%
    select(dataset, coef, ratio)

  l1o_results <- cross_data %>%
    select(dataset, l1o_cross_data) %>%
    unnest(cols = l1o_cross_data) %>%
    mutate(
      model = map(l1o_cross_data, model_f),
      coef = map_dbl(model, coef_f),
      ratio = map_dbl(model, ratio_f)
    ) %>%
    group_by(dataset) %>%
    summarize(
      coef_se = jackknife.sd(coef),
      ratio_se = jackknife.sd(ratio)
    )

  base_results %>%
    left_join(l1o_results, by = 'dataset')
}

adjacency_results <- analysis_f(
  function(df) with(df, {
    list(
      adj_med = median(df$dr_du[df$adjacent]),
      nonadj_med = median(df$dr_du[!df$adjacent])
    )
  }),
  function(model) with(model, { adj_med - nonadj_med }),
  function(model) with(model, { (adj_med - nonadj_med) / nonadj_med })
)

test_f <- function(df) with(df, { wilcox.test(dr_du[adjacent], dr_du[!adjacent]) })
estimate_f <- function(df) with(df, { median(dr_du[!adjacent]) - median(dr_du[adjacent]) })

wilcoxon_results <- cross_data %>%
  mutate(
    test = map(cross_data, test_f),
    estimate = map_dbl(cross_data, estimate_f),
    l1o_estimates = map(l1o_cross_data, function(dfs) map_dbl(dfs, estimate_f)),
    se = map_dbl(l1o_estimates, jackknife.sd),
    lci = estimate + se * qnorm(0.05 / 2),
    uci = estimate + se * qnorm(1 - (0.05 / 2)),
    p = map_dbl(test, ~ .$p.value),
    sig = p.adjust(p, "BH") < 0.05
  ) %>%
  select(dataset, estimate, lci, uci, p, sig)

wilcoxon_results

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
    estimate = map_dbl(cross_data, cor_f),
    l1o_ests = map(l1o_cross_data, function(dfs) map_dbl(dfs, cor_f)),
    se = map_dbl(l1o_ests, jackknife.sd),
    lci = estimate + se * qnorm(0.05 / 2),
    uci = estimate + se * qnorm(1 - (0.05 / 2)),
    Y = map(cross_data, ~ rank_matrix(long_to_matrix(., "dr_du"))),
    X = map2(matrix, Y, ~ rank_matrix(-subset_by_names(.x, rownames(.y)))),
    test = map2(X, Y, ~ vegan::mantel(.x, .y, method = "spearman")),
    estimate2 = map_dbl(test, ~ .$statistic),
    p = map_dbl(test, ~ .$signif),
    sig = p.adjust(p, "BH") < 0.05
  ) %>%
  { stopifnot(all(.$estimate2 == -.$estimate)); . } %>%
  select(dataset, estimate, estimate2, lci, uci, p, sig)

# # Tables ----------------------------------------------------------------------
# 
# show_results <- function(df, caption) {
#   df %>%
#     mutate(
#       coef_hci = 1.96 * coef_se * 0.5,
#       coef_cil = coef - coef_hci,
#       coef_ciu = coef + coef_hci,
#       coef_star = coef_cil > 0 | coef_ciu < 0,
#       ratio_hci = 1.96 * ratio_se * 0.5,
#       ratio_cil = ratio - ratio_hci,
#       ratio_ciu = ratio + ratio_hci,
#       ratio_star = ratio_cil > 0 | ratio_ciu < 0
#     ) %>%
#     mutate_if(is.numeric, sigfig) %>%
#     mutate_at(vars(ends_with('star')), ~ recode(as.numeric(.), `1` = '*', `0` = '')) %>%
#     mutate(
#       coef_display = str_glue('{coef} ({coef_cil} to {coef_ciu}){coef_star}'),
#       ratio_display = str_glue('{ratio} ({ratio_cil} to {ratio_ciu}){ratio_star}')
#     ) %>%
#     select(dataset, coef_display, ratio_display)
# }
# 
# bind_rows(
#   "adjacency" = adjacency_results,
#   "adjacency_lorru" = adjacency_lorru_results,
#   "adjacency_rlm" = adjacency_rlm_results,
#   "commuting" = commuting_results,
#   .id = "model_type"
# ) %>%
#   nest(-model_type) %>%
#   mutate(results = map(data, show_results)) %>%
#   select(model_type, results) %>%
#   unnest() %>%
#   write_tsv("results/empirical.tsv")

# show_results(
#   adjacency_results,
#   'Comparing adjacent and non-adjacent pairs. "Coef" is difference in median Δρ/Δτ between the two groups. "Ratio" is that difference divided by overall median.'
# )

# show_results(
#   adjacency_lorru_results,
#   'As above, but using LOR(ρ)/Δτ'
# )

# show_results(
#   adjacency_rlm_results,
#   'Using robust regression: Δρ/Δτ ~ Α. "Coef" is βΑ, "ratio" is βΑ/μ.'
# )

# show_results(
#   commuting_results,
#   'Robust regression on Δρ/Δτ ~ C, where C is the commuting fraction. "Coef" is β * 10^-4. "Ratio" is β * 10^-4 / μ.'
# )
# ```

# Plots -----------------------------------------------------------------------

# boxplot_data_f <- function(df, ymin, ymax) {
#   df %>%
#     nest(-adjacent) %>%
#     mutate(
#       y = map(data, ~ .$dr_du),
#       box = map(y, ~ boxplot.stats(.)$stats),
#       boxplot_data = map(box, ~ tibble(
#         ymin = max(.[1], ymin),
#         lower = .[2],
#         middle = .[3],
#         upper = .[4],
#         ymax = min(.[5], ymax)
#       ))
#     ) %>%
#     select(adjacent, boxplot_data) %>%
#     unnest()
# }
# 
# boxplot_f <- function(cross_data, f_to_keep) {
#   half_drop <- (1 - f_to_keep) / 2
# 
#   plot_data <- cross_data %>%
#     mutate(
#       y = map(cross_data, ~ .$dr_du),
#       ymin = map_dbl(y, ~ quantile(., half_drop)),
#       ymax = map_dbl(y, ~ quantile(., 1 - half_drop)),
#       boxplot_data = pmap(list(cross_data, ymin, ymax), boxplot_data_f),
#       point_data = pmap(list(cross_data, ymin, ymax), ~ filter(..1, between(dr_du, ..2, ..3)))
#     )
# 
#   point_data <- plot_data %>%
#     select(dataset, point_data) %>%
#     unnest()
# 
#   boxplot_data <- plot_data %>%
#     select(dataset, boxplot_data) %>%
#     unnest()
# 
#   plot <- ggplot(data = NULL, aes(x = factor(adjacent))) +
#     facet_wrap(~ dataset, scales = 'free_y') +
#     geom_boxplot(
#       data = boxplot_data,
#       aes(lower = lower, upper = upper, middle = middle, ymin = ymin, ymax = ymax),
#       stat = 'identity'
#     ) +
#     geom_jitter(data = point_data, aes(y = dr_du), size = 0.1, width = 0.2) +
#     scale_x_discrete(
#       '',
#       labels = c(`TRUE` = 'Adjacent', `FALSE` = 'Not adj.')
#     ) +
#     ylab(expression(paste('Use-resistance association ', (Delta * rho / Delta * tau)))) +
#     theme_cowplot() +
#     theme(strip.background = element_blank())
# 
#   plot
# }
# 
# adjacency_plot <- boxplot_f(cross_data, 0.90)
# ggsave('fig/adjacency_plot.pdf', plot = adjacency_plot)
# 
# # swo: can include the rlm on the un-logged x values, as reported in the
# # data, but they look weird and curvy when log-ing the x-vals
# 
# commute_plot <- cross_data %>%
#   select(dataset, cross_data) %>%
#   unnest() %>%
#   group_by(dataset) %>%
#   mutate(x = case_when(
#     f_commuting == 0 ~ -6,
#     TRUE ~ log10(f_commuting)
#   )) %>%
#   filter(between(ecdf(dr_du)(dr_du), 0.025, 0.975)) %>%
#   ungroup() %>%
#   ggplot(aes(x)) +
#   facet_wrap(~ dataset, scales = 'free') +
#   geom_point(aes(y = dr_du), shape = 1, size = 0.5) +
#   #geom_line(aes(y = y)) +
#   xlab('Commuting fraction (log10)') +
#   ylab(expression(paste('Use-resistance association ', (Delta * rho / Delta * tau)))) +
#   theme_cowplot() +
#   theme(strip.background = element_blank())
# 
# ggsave('fig/commute_plot.pdf', plot = commute_plot)
