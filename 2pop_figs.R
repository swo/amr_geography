#!/usr/bin/env Rscript

library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)
library(patchwork)

pdf.options(useDingbats = FALSE, useKerning = FALSE)

annotate_text <- function(x, y, label, parse = TRUE, ...) {
  annotate("text", x = x, y = y, label = label, parse = parse, ...)
}

# WHN -------------------------------------------------------------------------

whn_2pop <- read_rds("cache/whn_2pop.rds")

whn_2pop_barplot_data <- whn_2pop %>%
  unnest(results) %>%
  filter(epsilon %in% c(0.0, 0.01, 0.1))

whn_2pop_lineplot_data <- whn_2pop %>%
  mutate(delta_rho = map_dbl(results, ~ max(.$rho) - min(.$rho)))

whn_2pop_barplot_f <- function(df) {
  df %>%
    mutate_at("epsilon", ~ factor(
      ., levels = unique(.),
      labels = scales::percent(unique(.), accuracy = 1)
    )) %>%
    ggplot(aes(x = epsilon, y = rho, fill = pop)) +
    geom_hline(yintercept = 0.50, linetype = 2) +
    geom_col(position = "dodge", color = "black") +
    scale_fill_manual(
      "",
      values = c("white", "black")
    ) +
    scale_x_discrete(
      name = expression("interaction strength (" * epsilon * ")")
    ) +
    scale_y_continuous(
      name = expression("resistance (" * rho * ", %)"),
      labels = scales::percent_format(accuracy = 1, suffix = ""),
      limits = c(0, 1.0),
      expand = c(0, 0)
    ) +
    theme_half_open(font_size = 12) +
    theme(
      legend.key.size = unit(3, "mm"),
      legend.position = c(0.3, 1.03),
      axis.ticks.x = element_blank()
    )
}

whn_plot1 <- whn_2pop_barplot_data %>%
  filter(delta_tau == 0.05) %>%
  whn_2pop_barplot_f()

whn_plot2 <- whn_2pop_barplot_data %>%
  filter(delta_tau == 0.10) %>%
  whn_2pop_barplot_f()

whn_plot3 <- whn_2pop_lineplot_data %>%
  filter(epsilon %in% c(0.0, 0.01, 0.1)) %>%
  ggplot(aes(delta_tau, delta_rho)) +
  geom_blank(data = tibble(delta_tau = 0.155, delta_rho = 0)) +
  geom_point(aes(group = factor(epsilon))) +
  geom_line(aes(group = factor(epsilon))) +
  scale_x_continuous(
    expression(Delta * tau),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expression(Delta * rho),
    limits = c(0, 0.80),
    expand = c(0, 0.01)
  ) +
  annotate_text(0.07, 0.60, "epsilon == 0 * '%'") +
  annotate_text(0.10, 0.35, "epsilon == 1 * '%'") +
  annotate_text(0.13, 0.12, "epsilon == 10 * '%'") +
  theme_cowplot(font_size = 12) +
  theme(plot.margin = margin(0, 15, 0, 0, "pt"))

whn_plot4 <- whn_2pop_lineplot_data %>%
  filter(
    delta_tau == 0.10,
    epsilon %in% c(0, 1e-4, 0.01, 0.025, 0.050, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)
  ) %>%
  mutate(dr_du = delta_rho / delta_tau) %>%
  ggplot(aes(epsilon, dr_du, group = factor(delta_tau))) +
  geom_point() +
  geom_line() +
  geom_blank(data = tibble(epsilon = c(0, 0.51), dr_du = 0, delta_tau = 0.01)) +
  scale_x_continuous(
    expression(epsilon),
    expand = c(0, 0),
    labels = partial(scales::percent, accuracy = 1)
  ) +
  scale_y_continuous(
    expression(Delta * rho / Delta * tau),
    limits = c(0, 7),
    expand = c(0, 0.0)
  ) +
  theme_cowplot(font_size = 12) +
  theme(
    legend.position = c(0.5, 0.75),
    plot.margin = margin(0, 12, 0, 3, "pt")
  )

whn_2pop_plot <- whn_plot1 + whn_plot2 + whn_plot3 + whn_plot4 +
  plot_annotation(
    tag_levels = "a",
    theme = theme(plot.margin = margin())
  )

ggsave(
  "fig/whn_2pop.pdf", whn_2pop_plot,
  width = 190, height = 100, unit = "mm"
)

# D-types ---------------------------------------------------------------------

dtypes <- read_rds("cache/dtypes_2pop.rds")

dtypes_plot <- dtypes %>%
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
    labels = scales::percent,
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
  "fig/dtypes_2pop.pdf", dtypes_plot,
  width = 88, height = 80, unit = "mm"
)
