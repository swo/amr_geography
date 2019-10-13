#!/usr/bin/env Rscript --vanilla

library(tidyverse)

raw <- read_tsv('table-64.tsv')

data <- raw %>%
  gather('country_of_work', 'value', -country_of_residence) %>%
  filter(country_of_residence != country_of_work) %>%
  mutate(
    value = as.numeric(case_when(
      # "-1" for HR/NL is, I think, just 1
      .$value == '-1' ~ '1',
      # "." will be approximated as zero
      .$value == '.' ~ '0',
      # strip the parens
      str_detect(.$value, '^\\(') ~ str_match(.$value, '^\\((\\d+)\\)$')[, 2],
      # NA is just zero
      is.na(.$value) ~ '0',
      TRUE ~ .$value
    ))
  )

# I compared the results from these row sums against the "Total" column in the
# original table. The results are so-so, but as noted in the table footer,
# they aren't expected to be very good.

# data %>%
#   group_by(country_of_residence) %>%
#   summarize_at('value', sum) %>%
#   print(n = Inf)

write_tsv(data, '../commuting.tsv')
