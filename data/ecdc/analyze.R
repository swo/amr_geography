# swo 2009 Romania? Something is weird with quinolones in 2009

res_drugs = list('Macrolides'='macrolide', 'Fluoroquinolones'='quinolone', 'Penicillins'='beta_lactam')
use_drugs = list('Macrolides'='macrolide', 'Fluoroquinolones'='quinolone',
                      'Beta-lactam antibacterials, penicillins'='beta_lactam', 'Other beta-lactam antibacterials'='beta_lactam')

res = read_tsv('ears/ears.tsv') %>%
  filter(bug %in% c('Escherichia coli', 'Streptococcus pneumoniae'),
         drug %in% names(res_drugs),
         metric %in% c('n_isolates', 'n_ns', 'n_res')) %>%
  mutate(drug = unlist(res_drugs[drug])) %>%
  select(year, bug, drug, country, metric, value) %>%
  spread(metric, value) %>%
  # no. nonsusceptible is missing for some weird drugs
  # (combined resistances, high-level gentamicin, meticillin)
  # but they have no. resistant, so use that
  mutate(n_ns=if_else(is.na(n_ns), n_res, n_ns)) %>%
  # keep only records that have some data
  filter(n_ns > 0) %>%
  mutate(f_ns = n_ns / n_isolates)

use = read_tsv('esac/esac.tsv') %>%
  filter(!(country=='Romania' & year==2009)) %>%
  # keep only community (outpatient) use
  filter(toc == 'AC') %>%
  filter(abx_desc %in% names(use_drugs)) %>%
  mutate(drug = unlist(use_drugs[abx_desc])) %>%
  # remove weird data points
  group_by(year, drug, country) %>%
  summarize(did = sum(did)) %>%
  ungroup()

# average these values over 2011-2015

first_year = 2011
last_year = 2015

ares = res %>%
  filter(between(year, first_year, last_year)) %>%
  group_by(bug, drug, country) %>%
  summarize(
    n_ns = sum(n_ns),
    n_isolates = sum(n_isolates),
    first_res_year = min(year),
    last_res_year = max(year)
  ) %>%
  ungroup() %>%
  mutate(f_ns = n_ns / n_isolates)

ause = use %>%
  filter(between(year, first_year, last_year)) %>%
  group_by(drug, country) %>%
  summarize(
    did = mean(did),
    first_use_year = min(year),
    last_use_year = max(year)
  )

acomb = inner_join(ares, ause, by=c('drug', 'country'))

# say which bug-drug-countries don't meet the 2011-2015 end points

acomb %>%
  filter(
    first_res_year != first_year |
    last_res_year != last_year |
    first_use_year != first_year |
    last_use_year != last_year
  ) %>%
  select(bug, drug, country, ends_with('year')) %>%
  write_tsv('year_exceptions.tsv')

write_tsv(acomb, 'data.tsv')
