source("utils.R")

whn2 <- read_tsv("results/whn2.tsv")
dtypes2 <- read_tsv("results/dtypes2.tsv")

data <- bind_rows(
  "WHN" = whn2,
  "Dtypes" = dtypes2,
  .id = "sim"
) %>%
  mutate(dr_du = if_else(delta_rho == 0, 0, delta_rho / delta_tau))

# Estimates of epsilon0

epsilon0 <- data %>%
  select(sim, epsilon, dr_du) %>%
  filter(is.finite(dr_du)) %>%
  nest(data = c(epsilon, dr_du)) %>%
  mutate(
    model = map(data, ~ nls(
      dr_du ~ A * exp(-epsilon / epsilon0),
      start = list(A = 1.0, epsilon0 = 1e-2),
      data = .
    )),
    estimate = map_dbl(model, ~ coef(.)["epsilon0"]),
    ci = map(model, ~ confint(.)["epsilon0", ]),
    cil = map_dbl(ci, first),
    ciu = map_dbl(ci, last)
  ) %>%
  select(sim, estimate, cil, ciu)

# Reduction in $\Delta \rho / \Delta \tau$ against $\epsilon$
# Percent reduction, compared to ε=0 ----------------------------------

    # key = str_c(model, ' Δτ=', delta_tau)
  # ) %>%
  # filter(
    # delta_tau %in% c(0.05, 0.10),
    # epsilon %in% c(0, 1e-4, 1e-3, 1e-2, 1e-1)
  # ) %>%
  # group_by(model, delta_tau) %>%
  # mutate(pct_red = scales::percent(1 - dr_du / dr_du[epsilon == 0], 0.1)) %>%
  # ungroup() %>%
  # select(epsilon, key, pct_red) %>%
  # spread(key, pct_red)
