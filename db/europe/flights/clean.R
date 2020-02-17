#!/usr/bin/env Rscript --vanilla

library(tidyverse)
library(countrycode)
library(magrittr)

# Parameters
year <- "2013"
f_internal <- 0.01

# Get the list of countries we care about
countries <- read_tsv("../../../data/ecdc/data.tsv") %>%
  pull(country) %>%
  unique() %>%
  countrycode("country.name", "iso.name.en")

# Read in flight data
flights_raw <- read_tsv("avia_paocc.tsv.gz", na = ":")

# clean flight data
flights <- flights_raw %>%
  # split the first column
  separate(!!names(.)[1], c("unit", "measure", "partner", "geo"), sep = ",") %>%
  # use only passengers carried
  filter(unit == "PAS", measure == "PAS_CRD") %>%
  # use ISO country names
  mutate_at(c("partner", "geo"), ~ countrycode(., "eurostat", "iso.name.en")) %>%
  # keep only the 2 country names and the number of passenders in that year
  select(country1 = partner, country2 = geo, !!c("n" = year))

# load population
population_raw <- read_tsv("demo_pjan.tsv.gz", na = ":")

population <- population_raw %>%
  separate(!!names(.)[1], c("unit", "age", "sex", "geo"), sep = ",") %>%
  filter(unit == "NR", age == "TOTAL", sex == "T") %>%
  mutate_at("geo", ~ countrycode(., "eurostat", "iso.name.en")) %>%
  select(country = geo, !!c("n_population" = year))

# get "normalized" internal flights
normalization_data <- flights %>%
  filter(country1 == country2) %>%
  left_join(population, by = c("country1" = "country"))

model <- lm(n_passengers ~ n_population + 0, data = normalization_data)

# Add model predictions
normalized_data <- normalization_data %>%
  mutate(n = (1 / f_internal) * predict(model, newdata = select(., n_population))) %>%
  select(country1, country2, n)

# Take flights A->B and B->A, and average then for a single A-B value
new_data <- left_join(
  filter(flights, country1 < country2),
  filter(flights, country1 > country2),
  by = c("country1" = "country2", "country2" = "country1")
) %>%
  mutate(n = 0.5 * (n_passengers.x + n_passengers.y)) %>%
  select(country1, country2, n) %>%
  # Add in the A-A values
  bind_rows(normalized_data) %>%
  replace_na(list(n = 0)) %>%
  arrange(country1, country2) %>%
  group_by(country1) %>%
  # Convert to fractions of contacts
  mutate(f = n / sum(n, na.rm = TRUE))

write_tsv(new_data, "../flights.tsv")
