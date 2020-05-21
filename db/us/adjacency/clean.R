#!/usr/bin/env Rscript

library(tidyverse)
library(datasets)
library(zoo)

revalue <- function(x, from, to) to[match(x, from)]

# Develop a list of FIPS codes to state abbreviations
# e.g., 01 -> AL
raw_fips <- read_csv(
  "national_county.txt",
  col_names = c(
    "state_abb", "state_fips",
    "county_fips","county",
    "fips_class_code"
  )
)

fips <- raw_fips %>%
  select(state_abb, state_fips) %>%
  filter(state_abb %in% state.abb) %>%
  distinct()

# Load list of adjacent counties
raw_adjacency <- read_tsv(
  "county_adjacency.txt",
  col_names = c(
    "county1", "state_county_fips1",
    "county2", "state_county_fips2"
  )
)


fips_to_state <- function(x) {
  x %>%
    str_sub(1, 2) %>%
    revalue(fips$state_fips, fips$state_abb)
}

# Make a tibble of adjacencies
adjacency_tbl <- raw_adjacency %>%
  # replace blanks
  mutate_at("state_county_fips1", na.locf) %>%
  select(from_unit = state_county_fips1, to_unit = state_county_fips2) %>%
  mutate_at(c("from_unit", "to_unit"), fips_to_state) %>%
  distinct() %>%
  filter(!is.na(from_unit), !is.na(to_unit))

# Make a square(ish) tibble
adjacency <- adjacency_tbl %>%
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

# The results should be a 50x50 symmetrical matrix
mat <- as.matrix(select(adjacency, -from_unit)) %>%
  `rownames<-`(adjacency$from_unit)

stopifnot(all(dim(mat) == rep(50, 2)))
stopifnot(all(mat == t(mat)))

write_tsv(adjacency, "../adjacency.tsv")
