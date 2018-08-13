#!/usr/bin/env Rscript

raw <- read_csv('uswbo19ages.csv')

state <- raw %>%
  group_by(year, st) %>%
  summarize(pop = sum(pop)) %>%
  ungroup() %>%
  rename(state_abbreviation = st, population = pop)

write_tsv(state, 'state_census.tsv')
