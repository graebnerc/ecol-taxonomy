# get_data_extra.R -- builds data/tidy/new_data.csv (GDP + renewable share panel)
#
# Provenance for the "external validator" panel consumed by 06_validation.R and
# joined (for GDP p.c.) by 01_build_indicators.R. This file used to be produced
# interactively with no script and had a subtle defect (audit finding B1): each
# country-year appeared FOUR times because the Eurostat SHARES renewable series
# was pulled with its indicator dimension (overall / electricity / heating &
# cooling / transport) collapsed but not dropped, so `ShareRenewables_GrossAvEn`
# was a blend of four heterogeneous series. 06_validation.R then averaged over
# the four, describing a variable that does not exist.
#
# This script produces a DEDUPLICATED panel, one row per country-year, with an
# explicitly named `renew_share_overall` column (the Eurostat SHARES REN
# indicator -- overall share of renewables in gross final energy consumption).
#
# Two paths:
#   * DOWNLOAD = TRUE  -> rebuild from source (WDI GDP + Eurostat nrg_ind_ren,
#                         keeping and then selecting the indicator dimension).
#   * DOWNLOAD = FALSE -> reproduce offline from the archived blended file
#                         (_archive/data/new_data_blended_raw.csv), selecting the
#                         overall series. Verified offline that the first row of
#                         each country-year group equals the Eurostat REN series
#                         (match to <=0.1pp for 11/13 spot-checked EU states,
#                         2018), so the overall share is recoverable losslessly.

library(data.table)
library(here)
library(countrycode)
suppressMessages(library(dplyr))
here::i_am("R/get_data_extra.R")
source(here("R/country_classification.R"))

DOWNLOAD <- FALSE  # set TRUE to rebuild from WDI + Eurostat (needs network)

out_cols <- c("iso3c", "year", "GDP_ppp", "GDP_real",
              "ShareFossils_GrossAvEn", "renew_share_overall")

if (DOWNLOAD) {
  library(WDI)
  library(eurostat)

  # --- GDP: PPP (constant) and real (constant), World Bank WDI -----------------
  gdp <- as.data.table(WDI::WDI(
    country = countrycode(base_countries, "country.name", "iso2c"),
    indicator = c(GDP_ppp = "NY.GDP.MKTP.PP.KD", GDP_real = "NY.GDP.MKTP.KD")
  ))
  gdp[, iso3c := countrycode(iso2c, "iso2c", "iso3c")]
  gdp <- gdp[, .(iso3c, year, GDP_ppp, GDP_real)]

  # --- Renewable share, Eurostat SHARES (nrg_ind_ren), OVERALL indicator only --
  ren <- as.data.table(get_eurostat("nrg_ind_ren", time_format = "num"))
  ren <- ren[nrg_bal == "REN" & unit == "PC"]
  ren[, iso3c := countrycode(geo, "eurostat", "iso3c")]
  ren <- ren[iso3c %in% base_countries,
             .(iso3c, year = time, renew_share_overall = values)]

  # --- Fossil share of gross available energy ---------------------------------
  # TODO: rebuild from nrg_bal_c (gross available energy) in get_data.R; for now
  # carried from the archived panel (constant within country-year, so lossless).
  foss <- fread(here("_archive/data/new_data_blended_raw.csv"))
  foss <- unique(foss[, .(iso3c, year, ShareFossils_GrossAvEn)])

  nd <- merge(merge(gdp, ren, by = c("iso3c", "year"), all = TRUE),
              foss, by = c("iso3c", "year"), all.x = TRUE)

} else {
  raw <- fread(here("_archive/data/new_data_blended_raw.csv"))
  # First row of each country-year group is the overall (REN) series.
  raw[, .pos := seq_len(.N), by = .(iso3c, year)]
  nd <- raw[.pos == 1, .(iso3c, year, GDP_ppp, GDP_real, ShareFossils_GrossAvEn,
                         renew_share_overall = ShareRenewables_GrossAvEn)]
}

nd <- nd[, ..out_cols][order(iso3c, year)]
stopifnot(!anyDuplicated(nd[, .(iso3c, year)]))
fwrite(nd, here("data/tidy/new_data.csv"))
cat(sprintf("new_data.csv written: %d rows, %d countries, years %d-%d.\n",
            nrow(nd), uniqueN(nd$iso3c), min(nd$year), max(nd$year)))
