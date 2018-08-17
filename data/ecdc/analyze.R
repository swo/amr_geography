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
  filter(toc == 'ACHC') %>%
  filter(abx_desc %in% names(use_drugs)) %>%
  mutate(drug = unlist(use_drugs[abx_desc])) %>%
  # remove weird data points
  group_by(year, drug, country) %>%
  summarize(did = sum(did)) %>%
  ungroup()

comb = inner_join(res, use, by=c('country', 'drug', 'year'))

mod = comb %>%
  group_by(bug, drug, year) %>%
  filter(n() >= 3) %>%
  do(tidy(lm(f_ns ~ did, data=.), conf.int=TRUE)) %>%
  ungroup()

# average these values over all the years

ares = res %>%
  group_by(bug, drug, country) %>%
  summarize(n_ns = sum(n_ns),
            n_isolates = sum(n_isolates)) %>%
  ungroup() %>%
  mutate(f_ns = n_ns / n_isolates)

ause = use %>%
  group_by(drug, country) %>%
  summarize(did = mean(did))

acomb = inner_join(ares, ause, by=c('drug', 'country'))

amod = acomb %>%
  group_by(bug, drug) %>%
  do(tidy(lm(f_ns ~ did, data=.), conf.int=TRUE)) %>%
  ungroup()

write_tsv(acomb, 'data.tsv')

amod2 = amod %>%
  filter(term == 'did') %>%
  mutate(year = 2000)

mod %>%
  filter(term=='did') %>%
  ggplot(aes(year, estimate, ymin=conf.low, ymax=conf.high)) +
  geom_point() +
  geom_errorbar() +
  geom_point(data=amod2, color='red') +
  geom_errorbar(data=amod2, color='red') +
  facet_wrap(~bug+drug)
