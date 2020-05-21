#!/usr/bin/env Rscript

library(tidyverse)
library(readxl)
library(datasets)

revalue <- function(x, from, to) to[match(x, from)]

raw <- read_xlsx("table1.xlsx", skip = 5)

# for each AB combination of states, how many workers go from the one to
# the other?
counts <- raw %>%
  select(
    from_unit = `State Name...3`,
    to_unit = `State Name...9`,
    n = `Workers in Commuting Flow`
  ) %>%
  filter(from_unit %in% state.name, to_unit %in% state.name) %>%
  mutate_at(c("from_unit", "to_unit"), ~ revalue(., state.name, state.abb)) %>%
  group_by(from_unit, to_unit) %>%
  summarize_at("n", sum) %>%
  ungroup() %>%
  pivot_wider(
    c(from_unit, to_unit),
    names_from = to_unit, values_from = n,
    values_fill = list(n = 0)
  ) %>%
  arrange(from_unit) %>%
  select_at(c("from_unit", .$from_unit))

write_tsv(counts, "../commuting.tsv")
