#!/usr/bin/env Rscript

source("utils.R")

# WHN with commuting --------------------------------------------------

whn_commuting <- read_rds("results/whn_commuting.rds")
dtypes_commuting <- read_rds("results/dtypes_commuting.rds")

commuting <- bind_rows(
  "WHN" = whn_commuting,
  "Dtypes" = dtypes_commuting,
  .id = "model"
)

commuting_plot <- commuting %>%
  select(model, trans_data_nm, internal_f, sim) %>%
  unnest(cols = c(sim)) %>%
  ggplot(aes(tau, rho, color = factor(internal_f))) +
  facet_grid(trans_data_nm ~ model, scales = "free_x") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme_half_open()

ggsave("fig/commuting.pdf")

slope_f <- function(df) {
  data <- filter(df, between(tau, 0.05, 0.20))
  model <- lm(rho ~ tau, data = data)
  coef(model)["tau"]
}

commuting_reduction <- commuting %>%
  select(model, trans_data_nm, internal_f, sim) %>%
  mutate(
    slope = map_dbl(sim, slope_f),
    reduction = 1 - slope / max(slope)
  ) %>%
  select(model, trans_data_nm, internal_f, slope, reduction) %>%
  arrange(model, reduction)

write_tsv(commuting_table, "results/commuting_reduction.tsv")
