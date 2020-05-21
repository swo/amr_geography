#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(countrycode)

raw_commuting <- read_tsv("table-64.tsv")

commuting <- raw_commuting %>%
  rename(from_unit = country_of_residence) %>%
  pivot_longer(-from_unit, names_to = "to_unit") %>%
  mutate(
    value = as.numeric(case_when(
      # "-1" for HR/NL is, I think, just 1
      .$value == "-1" ~ "1",
      # "." will be approximated as zero
      .$value == "." ~ "0",
      # Strip the parentheses
      str_detect(.$value, "^\\(") ~ str_match(.$value, "^\\((\\d+)\\)$")[, 2],
      # NA is just zero
      is.na(.$value) ~ "0",
      TRUE ~ .$value
    ))
  ) %>%
  mutate_at(
    c("from_unit", "to_unit"),
    # For some reason, the UK is listed as "UK" rather than "GB" as per ISO alpha-2
    ~ countrycode(., "iso2c", "iso3c", custom_match = c("UK" = "GBR"))
  ) %>%
  mutate_at("value", ~ . * 1e3)

commuting <- commuting %>%
  pivot_wider(
    c(from_unit, to_unit),
    names_from = to_unit,
    values_fill = list(value = 0)
  )

write_tsv(commuting, "../commuting.tsv")
