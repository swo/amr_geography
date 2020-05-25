#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(countrycode)

year <- "2018"

flights_tbl <- read_tsv("avia_paocc.tsv.gz") %>%
  # split the first column
  separate(!!names(.)[1], c("unit", "measure", "partner", "geo"), sep = ",") %>%
  # use only passengers carried
  filter(unit == "PAS", measure == "PAS_CRD") %>%
  # use ISO 3-character country codes
  mutate_at(c("partner", "geo"), ~ countrycode(., "eurostat", "iso3c")) %>%
  filter_at(c("partner", "geo"), ~ !is.na(.)) %>%
  select(from_unit = partner, to_unit = geo, !!c("value" = year)) %>%
  mutate_at("value", as.numeric) %>%
  filter(!is.na(value))

flights <- flights_tbl %>%
  pivot_wider(
    c(from_unit, to_unit),
    names_from = to_unit,
    values_fill = list(value = 0)
  ) %>%
  arrange(from_unit) %>%
  select_at(c("from_unit", intersect(.$from_unit, names(.)))) %>%
  filter(from_unit %in% names(.))

write_tsv(flights, "../flights.tsv")
