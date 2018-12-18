#!/usr/bin/env Rscript

read_tsv('TotalPopSex-20181121012936.txt', skip = 1) %>%
  filter(Sex == 'Both sexes combined') %>%
  select(unit = Location, `2010`:`2015`) %>%
  gather('year', 'population', -unit) %>%
  mutate_at('population', ~ parse_number(., locale = locale(grouping_mark = ' '))) %>%
  filter(year == 2010) %>%
  select(unit, population) %>%
  write_tsv('europe_population.tsv')
