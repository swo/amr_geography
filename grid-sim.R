#!/usr/bin/env Rscript

library(tidyverse)
library(patchwork)
library(cowplot)
rlm <- MASS::rlm

set.seed(77845)

n <- 50
slope <- 1
grid_size <- 1

# Put points on a grid
unit_data <- tibble(
  id = 1:n,
  use = runif(n, 0, 1),
  x = runif(n, 0, grid_size),
  y = runif(n, 0, grid_size)
)

build_cross <- function(unit_data, d0) {
  # cross the raw unit data
  cross_data <- crossing(
    rename_all(unit_data, ~ str_c(., ".1")),
    rename_all(unit_data, ~ str_c(., ".2"))
  ) %>%
    mutate(
      dist = sqrt((x.1 - x.2) ** 2 + (y.1 - y.2) ** 2),
      weight = exp(-dist / d0),
      interaction_rank = rank(weight, ties.method = "first")
    )

  stopifnot(nrow(cross_data) == nrow(unit_data) ** 2)

  # use the crossed data to get the observed resistance in every unit
  unit_data <- cross_data %>%
    # get the resistance that every place would have, in a vacuum
    mutate(res0 = slope * use.2) %>%
    group_by(id.1) %>%
    # get actual resistance as weighted mean
    summarize(
      use = unique(use.1),
      res = weighted.mean(res0, weight)
    ) %>%
    select(id = id.1, use, res)

  # look at the observed data with respect to distance
  dist_data <- cross_data %>%
    select(id.1, id.2, dist, interaction_rank) %>%
    left_join(rename_all(unit_data, ~ str_c(., ".1")), by = "id.1") %>%
    left_join(rename_all(unit_data, ~ str_c(., ".2")), by = "id.2") %>%
    mutate(
      d_use = use.1 - use.2,
      d_res = res.1 - res.2,
      dr_du = d_res / d_use
    ) %>%
    filter(id.1 != id.2, d_use >= 0) %>%
    select(id.1, id.2, dr_du, dist, interaction_rank)

  list(
    unit_data = unit_data,
    dist_data = dist_data
  )
}

compare_deciles <- function(dist_data) {
  dist_data %>%
    mutate(decile = ntile(-dist, 10)) %>%
    filter(decile %in% c(1, 10)) %>%
    group_by(decile) %>%
    summarize_at("dr_du", median) %>%
    with({
      (dr_du[decile == 10] - dr_du[decile == 1]) /
        dr_du[decile == 1]
    })
}

d0s <- c(1e-6, 0.025, 0.075, 0.25)

results <- tibble(d0 = d0s) %>%
  mutate(
    data = map(d0, ~ build_cross(unit_data, .)),
    unit_data = map(data, ~ .$unit_data),
    dist_data = map(data, ~ .$dist_data),
    slope = map_dbl(unit_data, ~ coef(lm(res ~ use, data = .))["use"]),
    decile_fraction = map_dbl(dist_data, compare_deciles),
    cor_test = map(dist_data, ~ cor.test(.$dr_du, .$interaction_rank, method = "spearman")),
    cor = map_dbl(cor_test, ~ .$estimate)
  )

lim <- 5

unit_plot <- results %>%
  select(d0, unit_data) %>%
  mutate_at("d0", ~ fct_inorder(str_c("d[0] == ", .))) %>%
  unnest(cols = unit_data) %>%
  ggplot(aes(use, res)) +
  facet_wrap(vars(d0), nrow = 1, labeller = label_parsed) +
  geom_abline(slope = slope, linetype = 2, color = "black") +
  stat_smooth(method = "lm", color = "gray50", se = FALSE) +
  geom_point() +
  scale_x_continuous(
    name = expression("use (" * tau * ")"),
    breaks = c(0, 0.5, 1),
    labels = c("0", "0.5", "1")
  ) +
  scale_y_continuous(
    name = expression("resistance (" * rho * ")"),
    breaks = c(0, 0.5, 1)
  ) +
  theme_cowplot(font_size = 12) +
  theme(
    strip.background = element_blank(),
    plot.margin = margin(1, 5, 5, 1, "mm"),
    panel.spacing = unit(1, "lines")
  )

pair_plot <- results %>%
  select(d0, dist_data) %>%
  mutate_at("d0", ~ fct_inorder(str_c("d[0] == ", .))) %>%
  unnest(cols = dist_data) %>%
  ggplot(aes(interaction_rank, dr_du)) +
  facet_wrap(vars(d0), nrow = 1, labeller = label_parsed) +
  geom_point(shape = 1) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  stat_smooth(method = "rlm", color = "red", linetype = 2, se = FALSE) +
  coord_cartesian(ylim = c(-1, 1) * lim) +
  scale_x_continuous(
    name = "Interaction (rank)",
    breaks = c(0, n ** 2 / 2, n ** 2)
  ) +
  labs(y = expression(Delta * rho / Delta * tau)) +
  theme_cowplot(font_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.margin = margin(1, 5, 1, 1, "mm"),
    panel.spacing = unit(1, "lines")
  )

plot <- unit_plot / pair_plot +
 plot_annotation(
   tag_levels = "a",
   theme = theme(plot.margin = margin())
 )

ggsave(
  "fig/grid-sim-plot.pdf", plot,
  width = 190, height = 100, unit = "mm"
)

results %>%
  select(d0, slope, cor, decile_fraction) %>%
  write_tsv("results/grid-sim-results.tsv")
