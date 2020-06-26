#!/usr/bin/env Rscript

source("utils.R")

whn_2pop <- read_rds("cache/whn_2pop.rds")
dtypes_2pop <- read_rds("cache/dtypes_2pop.rds")

data <- bind_rows(
  "WHN" = whn_2pop,
  "Dtypes" = dtypes_2pop,
  .id = "sim"
) %>%
  mutate(
    delta_rho = map_dbl(results, ~ max(.$rho) - min(.$rho)),
    dr_du = if_else(delta_rho == 0, 0, delta_rho / delta_tau)
  )

# Percent reduction, compared to ε=0, in 2-pop models -------------------------

twopop_reduction <- data %>%
  filter(
    delta_tau %in% c(0.05, 0.10),
    epsilon %in% c(0, 1e-4, 1e-3, 1e-2, 1e-1)
  ) %>%
  group_by(sim, delta_tau) %>%
  mutate(reduction = 1 - dr_du / dr_du[epsilon == 0]) %>%
  ungroup() %>%
  select(sim, delta_tau, epsilon, reduction)

write_tsv(twopop_reduction, "results/2pop_reduction.tsv")
