#!/usr/bin/env Rscript

library(tidyverse)

# Antibiotic use data ---------------------------------------------------------

# Download the CDC Patient Safety Atlas antibiotic use data

download.file("https://gis.cdc.gov/grasp/PSA/Downloads/Data_OAU.csv", "Data_OAU.csv", method = "auto")

use_data <- read_csv("Data_OAU.csv") %>%
  filter(
    Antibiotic_Class == "Fluoroquinolones",
    # keep only 50 states, exclude "National" and DC
    Location %in% datasets::state.abb,
    between(Year, 2011, 2014)
  ) %>%
  # average over years
  group_by(Location) %>%
  # convert from prescriptions per 1,000 people per year to *per person*
  summarize(Rate = mean(as.numeric(Rate)) / 1e3) %>%
  select(state = Location, rx_person_year = Rate)

# Antibiotic resistance data --------------------------------------------------

# Download and unzip the CDC NHSN antibiotic resistance data

download.file("https://gis.cdc.gov/grasp/PSA/Downloads/PSASummaryDownloads.zip", "PSASummaryDownloads.zip", method = "auto")
unzip("PSASummaryDownloads.zip")

resistance_data <- read_csv("PSA_States.csv") %>%
  filter(
    Phenotype == "E.coli R to fluoroquinolones",
    State %in% datasets::state.abb,
    EventType == "CAUTI",
    EventYear == "All Years",
    AgeCategory == "All Ages"
  ) %>%
  mutate_at(vars(starts_with("Number")), as.integer) %>%
  select(state = State, n_isolates = NumberTested, n_resistant = NumberResistant)

# Combined data ---------------------------------------------------------------

# Check that the two data sets have the same number of rows (i.e., one per state)
stopifnot(nrow(use_data) == nrow(resistance_data))

# Combine the two data sets
combined_data <- inner_join(use_data, resistance_data, by = "state")

# Check that the result has the same number of rows
stopifnot(nrow(use_data) == nrow(combined_data))

# Save the combined data
write_tsv(combined_data, "data.tsv")
