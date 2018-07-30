#!/usr/bin/env Rscript

# IMS

ims = read_csv('Data_OAU.csv') %>%
  filter(Antibiotic_Class == 'Fluoroquinolones', Location != 'National', between(Year, 2011, 2014)) %>%
  # average use over years
  group_by(Location) %>%
  summarize(Rate = mean(as.numeric(Rate))) %>%
  select(state=Location, rx_1k_year=Rate)

# NHSN

nhsn = read_csv('PSA_States.csv') %>%
  filter(Phenotype == 'E.coli R to fluoroquinolones',
         State != 'PR', # IMS doesn't have Puerto Rico
         EventType == 'CAUTI',
         EventYear == 'All Years',
         AgeCategory == 'All Ages',
         !(NumberTested %in% c('(0)', '(1-19)'))) %>%
  mutate_at(vars(starts_with('Number')), as.integer) %>%
  mutate_at(vars(PercentResistant, matches('95CI')), as.numeric) %>%
  select(state=State, n_isolates=NumberTested, n_resistant=NumberResistant,
         pct_res=PercentResistant, pct_res_lci=Lower95CI, pct_res_uci=Upper95CI)

combined = inner_join(ims, nhsn, by='state')

# check that there was simple linking
stopifnot(nrow(ims) == nrow(nhsn) && nrow(nhsn) == nrow(combined))

write_tsv(combined, '../data.tsv')
