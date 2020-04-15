#!/usr/bin/env Rscript --vanilla

source("whn_model.R")

# WHN two-population model --------------------------------------------

whn_2pop_sim <- function(tau1, tau2, epsilon) {
  parms <- whn_base_parms %>%
    `$<-`("transmission_matrix", epsilon_matrix(epsilon)) %>%
    `$<-`("taui", c(tau1, tau2))

  memo_whn_sim(parms)
}

whn_epsilon_values <- c(0, 1e-4, 1e-3, 0.01, 0.0175, 0.025, 0.050, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)

whn2 <- tibble(
  base_tau = 0.125,
  delta_tau = round(seq(0.0, 0.15, length.out = 7), 3),
  tau1 = base_tau - delta_tau / 2,
  tau2 = base_tau + delta_tau / 2
) %>%
  crossing(epsilon = whn_epsilon_values) %>%
  mutate(
    results = pmap(list(tau1, tau2, epsilon), whn_2pop_sim),
    delta_rho = map_dbl(results, ~ max(.$rho) - min(.$rho))
  ) %>%
  unnest(cols = c(results)) %>%
  mutate(pop = recode(pop, `1` = "intervention", `2` = "control"))

ggsave(whn2, "results/whn2.tsv")

# WHN with commuting --------------------------------------------------

shrink_square_mat <- function(mat, n) {
  # matrix must be square
  n0 <- dim(mat)[1]
  stopifnot(dim(mat)[2] == n0)
  # must be asking for fewer columns
  stopifnot(n <= n0)

  # get indices to subsample
  idx <- sample(1:n0, n)
  mat[idx, idx]
}

scale_rows <- function(mat) {
  t(scale(t(mat), center = FALSE, scale = rowSums(mat)))
}
grow_diagonal <- function(mat, f) `diag<-`(mat, diag(mat) / f)

movement_counts_to_transmission_matrix <- function(tbl, size = NULL, f_internal = NULL) {
  # check that table has the expected columns
  stopifnot(all(names(tbl) == c("unit1", "unit2", "n")))
  # check that table can be symmetrical
  stopifnot(setequal(tbl$unit1, tbl$unit2))
  # get the names
  nms <- sort(unique(tbl$unit1))

  out <- tbl %>%
    arrange(unit1, unit2) %>%
    spread(unit2, n, fill = 0) %T>%
    {
      stopifnot(all(.$unit1 == nms))
      stopifnot(all(names(.) == c("unit1", nms)))
    } %>%
    select(-unit1) %>%
    as.matrix() %>%
    `rownames<-`(nms)

  if (!is.null(size)) out %<>% shrink_square_mat(size)
  if (!is.null(f_internal)) out %<>% grow_diagonal(f_internal)

  scale_rows(out)
}

size <- 25
trans_datasets <- list(
  eu_flights = read_tsv("../db/europe/flights.tsv") %>%
    rename(unit1 = country1, unit2 = country2, n = n),
  us_commuting = read_tsv("../db/us/commuting.tsv") %>%
    rename(unit1 = from_state, unit2 = to_state, n = n_workers)
)

taui <- sample(seq(0, 3 / 12, length.out = size))

null_parms <- whn_base_parms %>%
  `$<-`("taui", taui) %>%
  `$<-`("transmission_matrix", identity_matrix(size))

whn_commuting <- crossing(
  trans_data_nm = c("eu_flights", "us_commuting"),
  internal_f = c(1.0, 0.1, 0.01, 0.001)
) %>%
  mutate(
    trans_data = map(trans_data_nm, ~ trans_datasets[[.]]),
    trans_matrix = map2(trans_data, internal_f, ~ movement_counts_to_transmission_matrix(.x, size, .y)),
    parms = map(trans_matrix, ~ `$<-`(null_parms, "transmission_matrix", .))
  ) %>%
  bind_rows(crossing(trans_data_nm = c("eu_flights", "us_commuting"), internal_f = NA, parms = list(null_parms))) %>%
  mutate(sim = map(parms, whn_sim))

write_tsv(whn_commuting, "results/whn_commuting.tsv")
