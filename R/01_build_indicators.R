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

# NOTE (Phase 1): once green complexity (green_complexity_eu.csv) and brown
# employment are built by 02_complexity.R, left_join them here so the indicator
# table carries all four dimensions.

fwrite(indicators, here("data/tidy/taxonomy_indicators.csv"))
message(sprintf("Wrote taxonomy_indicators.csv: %d countries, %d indicators.",
                nrow(indicators), ncol(indicators) - 1L))
