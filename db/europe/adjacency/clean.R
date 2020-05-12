#!/usr/bin/env Rscript

library(tidyverse)
library(countrycode)

units <- read_tsv("../../../data/ecdc/data.tsv") %>%
  pull(country) %>%
  unique() %>%
  countrycode(., origin = "country.name", destination = "iso3c")

adjacency <- read_csv("contdird.csv") %>%
  # require land/river contiguity (not large water bodies)
  filter(conttype == 1, year == 2011) %>%
  select(unit1 = state1ab, unit2 = state2ab) %>%
  mutate_at(c("unit1", "unit2"), ~ countrycode(., origin = "cowc", destination = "iso3c")) %>%
  # keep only European pairs
  filter(unit1 %in% units, unit2 %in% units) %>%
  # make it look nice
  filter(unit1 < unit2) %>%
  arrange(unit1)

# make sure we got all the units, except for the islands
my_units <- adjacency %>%
  { c(.$unit1, .$unit2) } %>%
  unique()

# make sure Iceland and Malta are missing (i.e., are adjacent to nothing)
missing_units <- setdiff(units, my_units)
stopifnot(setequal(missing_units, c("ISL", "MLT")))

write_tsv(adjacency, "../adjacency.tsv")
