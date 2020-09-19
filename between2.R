#!/usr/bin/env Rscript

library(tidyverse)

# Marc's model:
# R ~ U1 + UN
# U1 === use in that unit
# UN === use in neighboring units, weighted by interactions

source("empirical_analysis.R")

f <- function(unit_data, interactions_tbl) {
  ud1 <- unit_data %>%
    rename(unit1 = unit, res1 = f_resistant, use1 = use)
  ud2 <- unit_data %>%
    rename(unit2 = unit, res2 = f_resistant, use2 = use)

  interactions_tbl %>%
    inner_join(ud1, by = c("setting", "unit1")) %>%
    inner_join(ud2, by = c("setting", "unit2")) %>%
    group_by(unit1) %>%
    summarize(
      use1 = unique(use1),
      res1 = unique(res1),
      usen = weighted.mean(use2[unit1 != unit2], interaction[unit1 != unit2])
    )
}

res <- unit_data %>%
  mutate(
    xdata = map(data, ~ f(., interactions_tbl)),
    model1 = map(xdata, ~ lm(res1 ~ use1, data = .)),
    modeln = map(xdata, ~ lm(res1 ~ usen, data = .)),
    model2 = map(xdata, ~ lm(res1 ~ use1 + usen, data = .)),
    p1 = map_dbl(model1, ~ coefficients(summary(.))["use1", "Pr(>|t|)"]),
    pn = map_dbl(modeln, ~ coefficients(summary(.))["usen", "Pr(>|t|)"]),
    p2_1 = map_dbl(model2, ~ coefficients(summary(.))["use1", "Pr(>|t|)"]),
    p2_n = map_dbl(model2, ~ coefficients(summary(.))["usen", "Pr(>|t|)"]),
    s1 = map_dbl(model1, ~ coef(.)["use1"]),
    sn = map_dbl(modeln, ~ coef(.)["usen"]),
    s2_1 = map_dbl(model2, ~ coef(.)["use1"]),
    s2_n = map_dbl(model2, ~ coef(.)["usen"])
  )

# Typically, the sum of the 2 slopes in the multivariate model is equal to
# the slope of the usen only model, which tells me that the usen model is
# just using usen as a proxy for use1.

# But that doesn't explain why the slope s1 is less than the slope sn, except
# unless use1 is just noisier?

# And s2_n is always greater than s1_n. That fits with the picture of use1
# being just noisier.

# So I'm not sure how to interpret this in view of spillover. Does this mean
# about spillover?
