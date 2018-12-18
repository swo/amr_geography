#!/usr/bin/env Rscript

raw_adjacency = read_tsv(
  'county_adjacency.txt',
  col_names = c('county1', 'state_county_fips1', 'county2', 'state_county_fips2')
)

raw_fips = read_csv(
  'national_county.txt',
  col_names = c('state', 'state_fips', 'county_fips', 'county', 'fips_class_code')
)

fips = raw_fips %>%
  select(state, state_fips) %>%
  distinct()

fips_to_state = function(x) with(fips, state[match(x, state_fips)])

adjacency = raw_adjacency %>%
  mutate(
    fips1 = zoo::na.locf(str_sub(state_county_fips1, 1, 2)),
    fips2 = str_sub(state_county_fips2, 1, 2)
  ) %>%
  select(fips1, fips2) %>%
  distinct() %>%
  mutate(
    state_abb1 = fips_to_state(fips1),
    state_abb2 = fips_to_state(fips2),
    unit_id1 = match(state_abb1, state.abb),
    unit_id2 = match(state_abb2, state.abb),
    unit1 = state.name[unit_id1],
    unit2 = state.name[unit_id2]
  ) %>%
  select(unit1, unit2)

write_tsv(adjacency, '../../state_adjacency.tsv')
