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
  cross_data <- crossing(
    rename_all(unit_data, ~ str_c(., ".1")),
    rename_all(unit_data, ~ str_c(., ".2"))
  ) %>%
    mutate(dist = sqrt((x.1 - x.2) ** 2 + (y.1 - y.2) ** 2))

  neighbor_use_data <- cross_data %>%
    # filter(id.1 != id.2) %>%
    group_by(id.1) %>%
    mutate(weight = exp(-dist / alpha)) %>%
    # mutate(weight = 1 / dist) %>%
    summarize(neighbor_use = weighted.mean(use.2, weight)) %>%
    select(id = id.1, neighbor_use)

  unit_data <- unit_data %>%
    left_join(neighbor_use_data, by = "id") %>%
    mutate(res = slope * neighbor_use) %>%
    # mutate(res = slope * ((1 - alpha) * use + alpha * neighbor_use)) %>%
    select(id, use, res)

  dist_data <- cross_data %>%
    select(id.1, id.2, dist) %>%
    left_join(rename_all(unit_data, ~ str_c(., ".1")), by = "id.1") %>%
    left_join(rename_all(unit_data, ~ str_c(., ".2")), by = "id.2") %>%
    mutate(
      d_use = use.1 - use.2,
      d_res = res.1 - res.2,
      dr_du = d_use / d_res
    ) %>%
    filter(id.1 != id.2, d_use >= 0) %>%
    select(id.1, id.2, dr_du, dist)

  dist_data
}

alphas <- c(1e-6, 0.01, 0.1, 0.2)

results <- tibble(alpha = alphas) %>%
  mutate(
    data = map(alpha, ~ build_cross(unit_data, .)),
    model = map(data, ~ rlm(dr_du ~ dist, data = .)),
    summary = map(model, summary),
    ci = map(model, confint.default),
    cor = map(data, ~ with(., { cor.test(dr_du, dist, method = "spearman") }))
  )

pair_plot <- results %>%
  select(alpha, data) %>%
  unnest(cols = data) %>%
  ggplot(aes(dist, dr_du)) +
  facet_wrap(vars(alpha)) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = 2, color = "green") +
  stat_smooth(method = "rlm", color = "red", linetype = 2) +
  coord_cartesian(ylim = c(-25, 25)) +
  labs(x = "distance", y = "Δres / Δuse", title = "Pairs of units")

ggsave("tmp.pdf")

results$summary
results$ci
results$cor
