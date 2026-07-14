# Central configuration for the WP1 country taxonomy pipeline.
# Sourced by the numbered pipeline scripts (01_, 02_, 03_).

# Reference window ------------------------------------------------------------
# The taxonomy uses country-level averages over a fixed window to smooth out
# year-to-year noise while staying close to the "current" structure of the
# economy. 2014-2018 is the most recent window in which all core sources
# (EXIOBASE footprints, Eurostat energy balances, PATSTAT green patents,
# WDI/Eurostat GDP) overlap with good coverage. Sensitivity to this choice is
# tested in Phase 6.
REF_FIRST_YEAR <- 2014
REF_LAST_YEAR  <- 2018

# Paths -----------------------------------------------------------------------
TIDY_DIR <- here::here("data", "tidy")
PLOT_DIR <- here::here("plots")
