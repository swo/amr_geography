#!/usr/bin/env Rscript

library(tidyverse)
library(patchwork)
rlm <- MASS::rlm

n <- 50
slope <- 1

# Put points on a grid
unit_data <- tibble(
  id = 1:n,
  use = runif(n, 0, 1),
  x = runif(n, 0, 1),
  y = runif(n, 0, 1)
)

build_cross <- function(unit_data, alpha) {
  # cross the raw unit data
  cross_data <- crossing(
    rename_all(unit_data, ~ str_c(., ".1")),
    rename_all(unit_data, ~ str_c(., ".2"))
  ) %>%
    mutate(
      dist = sqrt((x.1 - x.2) ** 2 + (y.1 - y.2) ** 2),
      weight = exp(-dist / alpha)
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
    select(id.1, id.2, dist) %>%
    left_join(rename_all(unit_data, ~ str_c(., ".1")), by = "id.1") %>%
    left_join(rename_all(unit_data, ~ str_c(., ".2")), by = "id.2") %>%
    mutate(
      d_use = use.1 - use.2,
      d_res = res.1 - res.2,
      dr_du = d_res / d_use
    ) %>%
    filter(id.1 != id.2, d_use >= 0) %>%
    select(id.1, id.2, dr_du, dist)

  # look at the observed data with respect to adjacency
  adj_data <- dist_data %>%
    group_by(id.1) %>%
    mutate(adjacent = dist == min(dist)) %>%
    ungroup() %>%
    select(id.1, id.2, dr_du, adjacent)

  list(
    unit_data = unit_data,
    dist_data = dist_data,
    adj_data = adj_data
  )
}

alphas <- c(1e-6, 0.01, 0.1, 0.5)
# alphas <- seq(1e-6, 1.0, length.out = 100)

results <- tibble(alpha = alphas) %>%
  mutate(
    data = map(alpha, ~ build_cross(unit_data, .)),
    unit_data = map(data, ~ .$unit_data),
    dist_data = map(data, ~ .$dist_data),
    adj_data = map(data, ~ .$adj_data),
    model = map(dist_data, ~ rlm(dr_du ~ dist, data = .)),
    slope = map_dbl(model, ~ coef(.)["dist"]),
    ci = map(model, confint.default),
    cor = map(dist_data, ~ cor.test(.$dr_du, .$dist, method = "spearman")),
    cor_p = map_dbl(cor, ~ .$p.value),
    adj_test = map(adj_data, ~ wilcox.test(dr_du ~ adjacent, data = .)),
    adj_p = map_dbl(adj_test, ~ .$p.value)
  )

lim <- 5

unit_plot <- results %>%
  select(alpha, unit_data) %>%
  unnest(cols = unit_data) %>%
  ggplot(aes(use, res)) +
  facet_wrap(vars(alpha), nrow = 1) +
  geom_point() +
  geom_abline(slope = 1, linetype = 2, color = "green") +
  stat_smooth(method = "rlm", color = "red", linetype = 2, se = FALSE) +
  labs(x = "use", y = "res", title = "Units")

pair_plot <- results %>%
  select(alpha, dist_data) %>%
  unnest(cols = dist_data) %>%
  ggplot(aes(dist, dr_du)) +
  facet_wrap(vars(alpha), nrow = 1) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = 2, color = "green") +
  stat_smooth(method = "rlm", color = "red", linetype = 2, se = FALSE) +
  coord_cartesian(ylim = c(-1, 1) * lim) +
  labs(x = "distance", y = "Δres / Δuse", title = "Pairs of units")

adj_plot <- results %>%
  select(alpha, adj_data) %>%
  unnest(cols = adj_data) %>%
  ggplot(aes(adjacent, dr_du)) +
  facet_wrap(vars(alpha), nrow = 1) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1) * lim) +
  labs(x = "adjacent", y = "Δres / Δuse", title = "Pairs of units")

plot <- unit_plot / pair_plot / adj_plot

ggsave("tmp.pdf")

results$cor_p
results$slope
results$adj_p
