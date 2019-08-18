#!/usr/bin/env Rscript

library(readxl)

raw <- read_xlsx('table1.xlsx', skip = 5)

# for each AB combination of states, how many workers go from the one to the other?
counts <- raw %>%
  select(from_state = `State Name`, to_state = `State Name__1`, n_workers = `Workers in Commuting Flow`) %>%
  filter(from_state %in% state.name, to_state %in% state.name) %>%
  group_by(from_state, to_state) %>%
  summarize_at('n_workers', sum) %>%
  ungroup() %>%
  filter(!is.na(from_state), !is.na(to_state))

within_counts <- counts %>%
  filter(from_state == to_state) %>%
  select(state = from_state, n_workers)

commuting <- counts %>%
  filter(from_state != to_state) %>%
  mutate(
    state1 = pmin(from_state, to_state),
    state2 = pmax(from_state, to_state)
  ) %>%
  group_by(state1, state2) %>%
  summarize(n_across = sum(n_workers)) %>%
  ungroup() %>%
  left_join(rename(within_counts, state1 = state, n_within1 = n_workers), by = 'state1') %>%
  left_join(rename(within_counts, state2 = state, n_within2 = n_workers), by = 'state2') %>%
  mutate(
    n_within = n_within1 + n_within2,
    f_commuting = n_across / (n_within + n_across)
  ) %>%
  select(state1, state2, f_commuting)

write_tsv(commuting, '../commuting.tsv')
