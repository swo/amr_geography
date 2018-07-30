#!/usr/bin/env Rscript

res = read_tsv('resistance.tsv')
use = read_tsv('use.tsv')

census_divisions = read_tsv('census-regions.tsv') %>%
  select(state, division=division_name)

population = read_tsv('population.tsv')

res2 = res %>%
  rename(division=region) %>%
  filter(
    bug == 'Escherichia coli' & drug == 'Fluoroquinolones' |
    bug == 'Streptococcus pneumoniae' & drug %in% c('Penicillins', 'Macrolides')
  ) %>%
  # summarize over years
  group_by(bug, drug, division) %>%
  summarize(pct_resistant = weighted.mean(pct_resistant, w=n_isolates)) %>%
  ungroup() %>%
  mutate(drug_group = case_when(.$drug == 'Fluoroquinolones' ~ 'quinolone',
                                .$drug == 'Penicillins' ~ 'penicillin',
                                .$drug == 'Macrolides' ~ 'macrolide')) %>%
  select(-drug)

use2 = use %>%
  rename(rx_1k_year=value) %>%
  filter(drug_group %in% c('broad_spectrum_penicillin', 'narrow_spectrum_penicillin', 'macrolide', 'quinolone')) %>%
  # combine the two penicillin groups
  mutate(drug_group = case_when(.$drug_group %in% c('macrolide', 'quinolone') ~ .$drug_group,
                                str_detect(.$drug_group, 'penicillin') ~ 'penicillin')) %>%
  group_by(state, year, drug_group) %>%
  summarize_at(vars(rx_1k_year), sum) %>%
  ungroup() %>%
  # include census region information and population data
  left_join(census_divisions, by='state') %>%
  left_join(population, by=c('state', 'year')) %>%
  # summarize use in each division and year
  group_by(drug_group, division, year) %>%
  summarize(rx_1k_year = weighted.mean(rx_1k_year, w=pop)) %>%
  # then take the average use across years
  summarize(rx_1k_year = mean(rx_1k_year)) %>%
  ungroup() %>%
  mutate(division = case_when(.$division == 'Middle Atlantic' ~ 'Mid Atlantic',
                              TRUE ~ .$division))

ur = left_join(res2, use2, by=c('division', 'drug_group'))

write_tsv(ur, 'use_res.tsv')

ur %>%
  group_by(bug, drug_group) %>%
  do(tidy(lm(pct_resistant ~ rx_1k_year, data=.), conf.int=TRUE))
