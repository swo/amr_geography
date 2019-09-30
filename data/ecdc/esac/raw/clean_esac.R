#!/usr/bin/env Rscript

raw = read_tsv('esac-raw.tsv')

# years should all match; can be dropped
raw %$% stopifnot(all(year == title_year))

# contexts should match; can be dropped
toc_hash = hashmap::hashmap(c('community (primary care sector)',
                              'community and hospital sector',
                              'hospital sector'),
                               c('AC', 'ACHC', 'HC'))
raw %$% stopifnot(all(toc == toc_hash[[title_toc]]))

# abx (the download key) should be either the description or ATC code
raw %$% stopifnot(all(abx == title_abx | abx == title_atc))

# the countries with stars (i.e., only total is reoprted) either report
# "-" for the community value or a value that is really close to the total
raw %>%
  mutate(star=endsWith(country, '*'),
         country=if_else(star, substr(country, 1, str_length(country) - 1), country)) %>%
  mutate(did=as.numeric(if_else(did=='-', NA_character_, did))) %>%
  # throw out community values for starred countries
  filter(!is.na(did), did > 0, !(star & toc=='AC')) %>%
  select(year, toc, abx, abx_desc=title_abx, abx_atc=title_atc, country, did) %>%
  write_tsv('esac.tsv')
