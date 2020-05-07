#!/usr/bin/env Rscript --vanilla

source("utils.R")

# Two-population ------------------------------------------------------

dtypes_2pop <- read_rds("results/dtypes_2pop.rds")

dtypes_2pop_plot <- dtypes_2pop %>%
  mutate(
    delta_rho = map_dbl(results, ~ max(.$rho) - min(.$rho)),
    dr_du = delta_rho / delta_tau
  ) %>%
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
  "fig/dtypes_2pop.pdf", dtypes_2pop_plot,
  width = 88, height = 80, unit = "mm"
)
