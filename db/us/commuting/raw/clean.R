#!/usr/bin/env Rscript

library(readxl)

raw <- read_xlsx('table1.xlsx', skip = 5)

clean <- raw %>%
  select(from_state = `State Name`, to_state = `State Name__1`, n_workers = `Workers in Commuting Flow`) %>%
  group_by(from_state, to_state) %>%
  summarize_at('n_workers', sum) %>%
  ungroup() %>%
  filter(!is.na(from_state), !is.na(to_state))

write_tsv(clean, '../commuting.tsv')
