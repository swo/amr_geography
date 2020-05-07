#!/usr/bin/env Rscript --vanilla

source("whn_model.R")

# WHN two-population model --------------------------------------------

whn_2pop_sim <- function(tau1, tau2, epsilon) {
  parms <- whn_base_parms %>%
    `$<-`("transmission_matrix", epsilon_matrix(epsilon)) %>%
    `$<-`("taui", c(tau1, tau2))

  whn_sim(parms) %>%
    mutate_at("pop", ~ recode(., `1` = "intervention", `2` = "control"))
}

whn_epsilon_values <- c(0, 1e-4, 1e-3, 0.01, 0.0175, 0.025, 0.050, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)

whn_2pop <- tibble(
  base_tau = 0.125,
  delta_tau = round(seq(0.0, 0.15, length.out = 7), 3),
  tau1 = base_tau - delta_tau / 2,
  tau2 = base_tau + delta_tau / 2
) %>%
  crossing(epsilon = whn_epsilon_values) %>%
  mutate(
    simulation_id = 1:n(),
    results = pmap(list(tau1, tau2, epsilon), whn_2pop_sim)
  ) %>%
  select(simulation_id, everything())

write_rds(whn_2pop, "results/whn_2pop.rds")

# WHN with commuting --------------------------------------------------

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
  mutate(
    simulation_id = 1:n(),
    sim = map(parms, whn_sim)
  ) %>%
  select(simulation_id, everything())

write_rds(whn_commuting, "results/whn_commuting.rds")
