#!/usr/bin/env Rscript

# Census --------------------------------------------------------------

# STATEFP,STNAME,POPULATION,LATITUDE,LONGITUDE
# 01,Alabama,4779736,+33.008097,-086.756826
# 02,Alaska,710231,+61.399882,-148.873973

census = read_csv('census/CenPop2010_Mean_ST.txt') %>%
  select(state = STNAME, population = POPULATION) %>%
  filter(not(state %in% c('District of Columbia', 'Puerto Rico'))) %>%
  mutate(unit_id = match(state, state.name),
         state_abbreviation = state.abb[unit_id],
         region = state.region[unit_id],
         division = state.division[unit_id])

# Areas, temperature, income  ------------------------------------------

areas = read_tsv('state_areas/state_areas.tsv')
temperatures = read_tsv('temperature/state_temperature.tsv')
incomes = read_tsv('income/state_income.tsv')
  
census %>%
  left_join(areas, by = 'state') %>%
  left_join(temperatures, by = 'state') %>%
  left_join(incomes, by = 'state') %>%
  rename(unit = state) %>%
  write_tsv('../state_data.tsv')
