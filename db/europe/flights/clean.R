#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(countrycode)
library(magrittr)

# Parameters
year <- "2018"

# Read in flight data
flights_raw <- read_tsv("avia_paocc.tsv.gz", na = ":")

# clean flight data
flights <- flights_raw %>%
  # split the first column
  separate(!!names(.)[1], c("unit", "measure", "partner", "geo"), sep = ",") %>%
  # use only passengers carried
  filter(unit == "PAS", measure == "PAS_CRD") %>%
  # use ISO country names
  mutate_at(c("partner", "geo"), ~ countrycode(., "eurostat", "iso3c")) %>%
  filter_at(c("partner", "geo"), ~ !is.na(.)) %>%
  # keep only the 2 country names and the number of passenders in that year
  select(country1 = partner, country2 = geo, !!c("n" = year))

# load population
population_raw <- read_tsv("demo_pjan.tsv.gz", na = ":")

remove_annotations <- function(x) {
  if (is.numeric(x)) {
    x
  } else if (is.character(x)) {
    as.numeric(str_replace(x, " p$", ""))
  }
}

population <- population_raw %>%
  separate(!!names(.)[1], c("unit", "age", "sex", "geo"), sep = ",") %>%
  filter(unit == "NR", age == "TOTAL", sex == "T") %>%
  select(-unit, -age, -sex) %>%
  mutate_at("geo", ~ countrycode(., "eurostat", "iso3c")) %>%
  select(country = geo, !!c("n_population" = year)) %>%
  mutate_at("n_population", remove_annotations)

# get "normalized" internal flights
normalization_data <- flights %>%
  filter(country1 == country2) %>%
  left_join(population, by = c("country1" = "country"))

model <- lm(n ~ n_population + 0, data = normalization_data)

# Add model predictions
normalized_data <- normalization_data %>%
  mutate(n = floor(predict(model, newdata = select(., n_population)))) %>%
  select(country1, country2, n)

new_data <- flights %>%
  filter(country1 != country2) %>%
  bind_rows(normalized_data) %>%
  arrange(country1, country2)

write_tsv(new_data, "../flights.tsv")
