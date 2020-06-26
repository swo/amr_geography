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
