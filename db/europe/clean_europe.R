#!/usr/bin/env Rscript

rename_countries = function(df) {
  mutate(df, unit = recode(unit,
                           'United Kingdom of Great Britain and Northern Ireland' = 'United Kingdom',
                           'Czechia' = 'Czech Republic'))
}

known_units = read_tsv('../../data/ecdc/data.tsv') %>%
  select(unit = country) %>%
  distinct()

region_data = read_csv('un_geoscheme/UNSD — Methodology.csv') %>%
  filter(`Region Name` == 'Europe') %>%
  select(unit = `Country or Area`, region = `Sub-region Name`) %>%
  rename_countries()

area_data = read_tsv('europe_areas/europe_areas.tsv')

population_data = read_tsv('population/europe_population.tsv') %>%
  rename_countries()

temperature_data = read_tsv('temperature/europe_temperatures.tsv')

income_data = read_tsv('income/Download-GDPPCconstant-USD-countries.txt', skip = 2) %>%
  gather('year', 'income', -CountryID, -Country) %>%
  filter(between(as.integer(year), 2010, 2015)) %>%
  group_by(unit = Country) %>%
  summarize_at('income', mean) %>%
  rename_countries()

units = known_units %>%
  mutate(unit_id = 1:n()) %>%
  left_join(region_data, by = 'unit') %>%
  left_join(area_data, by = 'unit') %>%
  left_join(population_data, by = 'unit') %>%
  left_join(temperature_data, by = 'unit') %>%
  left_join(income_data, by = 'unit')

# check if there are any NA values
cat('\nAny NA rows?\n')
units %>%
  filter_all(any_vars(is.na(.)))

units %>%
  write_tsv('../europe_units.tsv')
