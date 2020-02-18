#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(readxl)
library(datasets)

raw <- read_xlsx('table1.xlsx', skip = 5)

# for each AB combination of states, how many workers go from the one to the other?
counts <- raw %>%
  select(from_state = `State Name...3`, to_state = `State Name...9`, n_workers = `Workers in Commuting Flow`) %>%
  filter(from_state %in% state.name, to_state %in% state.name) %>%
  group_by(from_state, to_state) %>%
  summarize_at('n_workers', sum) %>%
  ungroup()

write_tsv(counts, "../commuting.tsv")
