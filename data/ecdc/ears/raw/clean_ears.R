#!/usr/bin/env Rscript

# clean up the ECDC resistance data

# create a hashmap for changing the metric names
key_values = c('Completeness age', 'complete_age',
               'Completeness gender', 'complete_gender',
               'Non-susceptible (I and R) isolates', 'n_ns',
               'Non-susceptible (I and R) isolates proportion', 'pct_ns',
               'Resistant (R) isolates', 'n_res',
               'Resistant (R) isolates proportion', 'pct_res',
               'Total tested isolates', 'n_isolates')

metric_hash = hashmap::hashmap(key_values[c(TRUE, FALSE)], key_values[c(FALSE, TRUE)])

raw = read_csv('ECDC_surveillance_data_Antimicrobial_resistance.csv')

# assert that TxtValue is blank
stopifnot(is.na(unique(raw$TxtValue)))

cleaned = raw %>%
  select(bug_drug=Population,
         metric=Indicator,
         year=Time,
         country_code=RegionCode,
         country=RegionName,
         value=NumValue) %>%
  filter(value != '-') %>%
  mutate(bug=str_split_fixed(bug_drug, '\\|', n=2)[,1],
         drug=str_split_fixed(bug_drug, '\\|', n=2)[,2],
         metric=metric_hash[[metric]],
         value=as.numeric(value))

# save the country codes
cleaned %>%
  count(country, country_code) %>%
  select(-n) %>%
  write_tsv('country_codes.tsv')

cleaned %>%
  select(year, country, bug, drug, metric, value) %>%
  write_tsv('ears.tsv')
