# 01 - Build the per-country taxonomy indicator table.
# Reads the tidy panel (+ GDP) and writes data/tidy/taxonomy_indicators.csv,
# the single vantage point for all downstream analysis.

here::i_am("R/01_build_indicators.R")
library(here)
library(data.table)
library(dplyr)
library(tidyr)
library(countrycode)
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/indicators.R"))

base_data  <- as_tibble(fread(here("data/tidy/full_taxonomy_data.csv")))
extra_data <- as_tibble(fread(here("data/tidy/new_data.csv")))

indicators <- build_indicator_table(
  base_data, extra_data,
  first_year = REF_FIRST_YEAR, last_year = REF_LAST_YEAR
)

# Fold in green complexity (Dimension 4) once 02_complexity.R has produced it.
# Run order is therefore: 02_complexity.R -> 01_build_indicators.R -> 03_analysis.R.
green_path <- here("data/tidy/green_complexity_eu.csv")
if (file.exists(green_path)) {
  green <- as_tibble(fread(green_path)) |>
    select(country, ECI, GCI, GCP)
  indicators <- left_join(indicators, green, by = "country")
  message("Joined green complexity (ECI, GCI, GCP).")
} else {
  message("green_complexity_eu.csv not found - run 02_complexity.R, then re-run 01. ",
          "Building base indicators only.")
}

fwrite(indicators, here("data/tidy/taxonomy_indicators.csv"))
message(sprintf("Wrote taxonomy_indicators.csv: %d countries, %d indicators.",
                nrow(indicators), ncol(indicators) - 1L))
