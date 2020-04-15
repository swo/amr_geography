source("utils.R")

# Reduction in $\Delta \rho / \Delta \tau$ against $\epsilon$
# Percent reduction, compared to ε=0 ----------------------------------

whn2 <- read_tsv("results/whn2.tsv")
dtypes2 <- read_tsv("results/dtypes2.tsv")

bind_rows(
  'WHN' = whn2,
  'Dtypes' = dtypes2,
  .id = 'model'
) %>%
  mutate(
    dr_du = delta_rho / delta_tau,
    key = str_c(model, ' Δτ=', delta_tau)
  ) %>%
  filter(
    delta_tau %in% c(0.05, 0.10),
    epsilon %in% c(0, 1e-4, 1e-3, 1e-2, 1e-1)
  ) %>%
  group_by(model, delta_tau) %>%
  mutate(pct_red = scales::percent(1 - dr_du / dr_du[epsilon == 0], 0.1)) %>%
  ungroup() %>%
  select(epsilon, key, pct_red) %>%
  spread(key, pct_red) %>%
