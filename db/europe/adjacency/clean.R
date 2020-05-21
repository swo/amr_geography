#!/usr/bin/env Rscript

library(tidyverse)
library(countrycode)

units <- read_tsv("../../../data/ecdc/data.tsv") %>%
  pull(country) %>%
  unique() %>%
  countrycode(., origin = "country.name", destination = "iso3c")

adjacency_tbl <- read_csv("contdird.csv") %>%
  # require land/river contiguity (not large water bodies)
  filter(conttype == 1, year == 2011) %>%
  select(from_unit = state1ab, to_unit = state2ab) %>%
  mutate_at(c("from_unit", "to_unit"), ~ countrycode(., origin = "cowc", destination = "iso3c")) %>%
  # keep only European pairs
  filter(from_unit %in% units, to_unit %in% units) %>%
  arrange(from_unit)

# Make sure we got all the units, except for the islands
# (Iceland and Malta are missing, i.e., are adjacent to nothing)
my_units <- adjacency_tbl %>%
  { c(.$from_unit, .$to_unit) } %>%
  unique()

missing_units <- setdiff(units, my_units)
stopifnot(setequal(missing_units, c("ISL", "MLT")))

# Make a square(ish) tibble
adjacency <- adjacency_tbl %>%
  # Each unit is adjacent to itself
  # (this also squeaks in Iceland and Malta)
  bind_rows(tibble(from_unit = units, to_unit = units)) %>%
  mutate(x = 1) %>%
  pivot_wider(
    c(from_unit, to_unit),
    names_from = to_unit, values_from = x,
    values_fill = list(x = 0)
  ) %>%
  arrange(from_unit) %>%
  select_at(c("from_unit", sort(unique(.$from_unit))))

# Check that rows and columns are the same
stopifnot(all(adjacency$from_unit == names(adjacency)[-1]))

# Result should be symmetric
mat <- as.matrix(select(adjacency, -from_unit)) %>%
  `rownames<-`(adjacency$from_unit)

stopifnot(all(mat == t(mat)))

write_tsv(adjacency, "../adjacency.tsv")
