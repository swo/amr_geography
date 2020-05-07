#!/usr/bin/env Rscript --vanilla

library(tidyverse)

# N (no. of pairs per group) in power calculation
f <- function(dr0, eps, eps0, niso) {
  # Effect size computed from parameters
  # d = Δρ|ε=0 * (1 - e^(-ε/ε0}) sqrt(2N)
  d <- dr0 * (1 - exp(-eps / eps0)) * sqrt(2 * niso)

  power.t.test(
    n = NULL,
    delta = d,
    sd = 1,
    power = 0.8,
    type = "two.sample",
    alternative = "one.sided"
  )$n
}

# "Base" is the base value
# "Better" is an optimistic value for sensitivity calculations
all_parms <- list(
  dr0 = list(base = 0.2, better = 0.4),
  eps = list(base = 1e-4, better = 1e-2),
  eps0 = list(base = 0.05, better = 0.005),
  niso = list(base = 1e2, better = 1e3)
)

# N at base parms
base_parms <- map(all_parms, ~ .$base)
base_results <- tibble(
  name = "base", parms = list(base_parms), n_pairs_per_group = do.call(f, base_parms)
)

results <- tibble(name = names(all_parms)) %>%
  mutate(
    parms = map(name, ~ assign_in(base_parms, ., all_parms[[.]]$better)),
    n_pairs_per_group = map_dbl(parms, ~ do.call(f, .))
  ) %>%
  bind_rows(base_results) %>%
  unnest_wider(col = parms)

write_tsv(results, "results/power.tsv")
