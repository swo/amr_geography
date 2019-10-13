#!/usr/bin/env Rscript --vanilla

library(tidyverse)

raw <- read_csv('301624556_T_T100D_MARKET_US_CARRIER_ONLY.csv')

counts <- raw %>%
  group_by(from_state = ORIGIN_STATE_NM, to_state = DEST_STATE_NM) %>%
  summarize(n = sum(PASSENGERS)) %>%
  ungroup() %>%
  # exclude Puerto Rico, etc.
  filter(from_state %in% state.name, to_state %in% state.name)

flying_across <- counts %>%
  filter(from_state != to_state) %>%
  mutate(
    state1 = pmin(from_state, to_state),
    state2 = pmax(from_state, to_state)
  ) %>%
  group_by(state1, state2) %>%
  summarize(n_flying = sum(n))

write_tsv(flying_across, '../flying.tsv')
