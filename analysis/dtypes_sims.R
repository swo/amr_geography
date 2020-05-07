#!/usr/bin/env Rscript --vanilla

source("utils.R")
source("dtypes_model.R")

# D-types two-population ----------------------------------------------

dtypes_2pop <- tibble(
  base_tau = 0.125,
  delta_tau = c(0.05, 0.10),
  tau1 = base_tau - delta_tau / 2,
  tau2 = base_tau + delta_tau / 2
) %>%
  crossing(epsilon = dtypes_epsilon_values) %>%
  mutate(
    simulation_id = 1:n(),
    results = pmap(list(tau1, tau2, epsilon), dtypes_2pop_sim)
  ) %>%
  select(simulation_id, everything())

write_rds(dtypes_2pop, "results/dtypes_2pop.rds")

# D-types with commuting ----------------------------------------------

tau_i <- sample(seq(0.04, 0.20, length.out = size))

null_parms <- dtypes_base_parms %>%
  `$<-`("tau_i", tau_i) %>%
  `$<-`("transmission_matrix", identity_matrix(size))

dtypes_commuting <- crossing(
  trans_data_nm = c("eu_flights", "us_commuting"),
  internal_f = c(1.0, 0.1, 0.01, 0.001)
) %>%
  mutate(
    trans_data = map(trans_data_nm, ~ trans_datasets[[.]]),
    trans_matrix = map2(trans_data, internal_f, ~ movement_counts_to_transmission_matrix(.x, size, .y)),
    parms = map(trans_matrix, ~ `$<-`(null_parms, "transmission_matrix", .))
  ) %>%
  bind_rows(crossing(trans_data_nm = c("eu_flights", "us_commuting"), internal_f = NA, parms = list(null_parms))) %>%
  mutate(
    simulation_id = 1:n(),
    sim_raw = map(parms, dtypes_sim),
    sim = map(sim_raw, dtypes_simplify_results)
  ) %>%
  select(simulation_id, everything())

write_rds(dtypes_commuting, "results/dtypes_commuting.rds")
