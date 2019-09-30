#!/usr/bin/env Rscript

codes = read_csv('UNSD — Methodology.csv') %>%
  select(ISO_3DIGIT = `ISO-alpha3 Code`, unit = `Country or Area`) %>%
  mutate(unit = recode(unit,
                       'United Kingdom of Great Britain and Northern Ireland' = 'United Kingdom',
                       'Czechia' = 'Czech Republic'))

raw = read_tsv('cckp_historical_data_0.txt')

raw %>%
  left_join(codes) %>%
  select(unit, temperature = Annual_temp) %>%
  bind_rows(data_frame(unit = 'Malta', temperature = 19.5)) %>%
  write_tsv('europe_temperatures.tsv')
