#!/usr/bin/env Rscript --vanilla

source("utils.R")

# WHN two-population model

whn_2pop <- read_rds("results/whn_2pop.rds")

whn_2pop_barplot_data <- whn_2pop %>%
  unnest(results) %>%
  filter(epsilon %in% c(0.0, 0.01, 0.1))

whn_2pop_lineplot_data <- whn_2pop %>%
  mutate(delta_rho = map_dbl(results, ~ max(.$rho) - min(.$rho)))

whn_2pop_barplot_f <- function(df) {
  initial_resistance <- df %>%
    filter(epsilon == 0, pop == "control") %>%
    pull(rho)

  df %>%
    ggplot(aes(x = factor(epsilon), y = rho, fill = pop)) +
    geom_hline(yintercept = initial_resistance, linetype = 2) +
    geom_col(position = "dodge", color = "black") +
    scale_fill_manual(
      "",
      values = c("white", "black")
    ) +
    scale_x_discrete(name = expression(epsilon)) +
    scale_y_continuous(
      name = "resistance (%)",
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
  ggplot(aes(delta_tau, delta_rho, group = factor(epsilon))) +
  geom_point() +
  geom_line() +
  scale_x_continuous(
    expression(Delta * tau),
    limits = c(0, 0.15),
    expand = c(0, 0.002)
  ) +
  scale_y_continuous(
    expression(Delta * rho),
    limits = c(0, 0.80),
    expand = c(0, 0.01)
  ) +
  annotate_text(0.07, 0.60, "epsilon == 0") +
  annotate_text(0.10, 0.35, "epsilon == 0.01") +
  annotate_text(0.13, 0.12, "epsilon == 0.1") +
  theme_cowplot(font_size = 12)

whn_plot4 <- whn_2pop_lineplot_data %>%
  filter(
    delta_tau %in% c(0.05, 0.10),
    epsilon %in% c(0, 1e-4, 0.01, 0.025, 0.050, 0.075, 0.1, 0.2, 0.3, 0.4, 0.5)
  ) %>%
  mutate(dr_du = delta_rho / delta_tau) %>%
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
    expand = c(0, 0.005)
  ) +
  scale_y_continuous(
    expression(Delta * rho / Delta * tau),
    limits = c(0, 7),
    expand = c(0, 0.1)
  ) +
  theme_cowplot(font_size = 12) +
  theme(legend.position = c(0.5, 0.75))

diagram <- ggdraw() +
  draw_grob(x = 0.2, circleGrob(gp = gpar(fill = "black")), scale = 0.5) +
  draw_grob(x = -0.5, circleGrob(gp = gpar(fill = "white")), scale = 0.5) +
  draw_grob(x = -0.5, y = -0.5, textGrob("control\npopulation", gp = gpar(fontsize = 11))) +
  draw_grob(x = 0.2, y = -0.5, textGrob("intervention\npopulation", gp = gpar(fontsize = 11))) +
  draw_grob(x = 0, y = 0, linesGrob(x = unit(c(0.2, 0.5), "npc"), y = 0.5)) +
  draw_grob(x = -0.15, y = 0.1, textGrob(expression(epsilon), gp = gpar(fontsize = 11))) +
  draw_grob(x = -0.15, y = 0.35, textGrob("interaction\nstrength", gp = gpar(fontsize = 11)))

whn_2pop_plot <- (diagram | whn_plot1 | whn_plot2) /
  (whn_plot3 | whn_plot4) +
  plot_annotation(
    tag_levels = "a",
    theme = theme(plot.margin = margin())
  )

ggsave(
  "fig/whn_2pop.pdf", whn_2pop_plot,
  width = 190, height = 100, unit = "mm"
)

# WHN with commuting --------------------------------------------------

whn_commuting <- read_rds("results/whn_commuting.rds")

whn_commuting_plot <- whn_commuting %>%
  ggplot(aes(tau, rho, color = factor(internal_f))) +
  facet_wrap(~ trans_data_nm) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point()

whn_commuting_table <- whn_commuting %>%
  select(trans_data_nm, internal_f, rho, tau) %>%
  filter(between(tau, 0.05, 0.20)) %>%
  nest(data = c(rho, tau)) %>%
  mutate(
    model = map(data, ~ lm(rho ~ tau, data = .)),
    slope = map_dbl(model, ~ coef(.)["tau"]),
    reduction = 1 - slope / max(slope)
  ) %>%
  select(trans_data_nm, internal_f, slope, reduction) %>%
  arrange(reduction)
