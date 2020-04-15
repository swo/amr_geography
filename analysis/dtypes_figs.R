#!/usr/bin/env Rscript --vanilla

source("utils.R")

# Two-population ------------------------------------------------------

dtypes2 <- read_tsv("results/dtypes2.tsv")

dtypes2_plot <- dtypes2 %>%
  ggplot(aes(epsilon, dr_du, group = factor(delta_tau))) +
  geom_point(aes(shape = factor(delta_tau))) +
  geom_line() +
  geom_blank(data = tibble(epsilon = 0, dr_du = 0, delta_tau = 0.01)) +
  scale_shape_manual(
    values = c(1, 16),
    labels = c(
      expression(Delta * tau == 0.05),
      expression(Delta * tau == 0.10)
    )
  ) +
  guides(shape = guide_legend(title = "", label.hjust = 0)) +
  scale_x_continuous(
    expression(epsilon),
    limits = c(0, 0.1),
    expand = c(0.02, 0, 0.05, 0)
  ) +
  scale_y_continuous(
    expression(Delta * rho / Delta * tau),
    limits = c(0, 6.5),
    expand = c(0, 0.1)
  ) +
  theme_cowplot(font_size = 12) +
  theme(legend.position = c(0.5, 0.75))

ggsave(
  'fig/dtypes2.pdf', dtypes2_plot,
  width = 88, height = 80, unit = 'mm'
)

# Commuting -----------------------------------------------------------

dtypes_commuting <- read_tsv("results/dtypes_commuting.tsv")

dtypes_commuting_plot <- dtypes_commuting %>%
  ggplot(aes(tau, rho, color = factor(internal_f))) +
  facet_wrap(~ trans_data_nm) +
  geom_point() +
  geom_line()

dtypes_commuting_table <- dtypes_commuting %>%
  select(trans_data_nm, internal_f, tau, rho) %>%
  nest(data = c(tau, rho)) %>%
  mutate(
    model = map(data, ~ lm(rho ~ tau, data = .)),
    slope = map_dbl(model, ~ coef(.)["tau"]),
    reduction = 1 - slope / max(slope)
  ) %>%
  select(trans_data_nm, internal_f, slope, reduction) %>%
  arrange(reduction)
