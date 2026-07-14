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
# explicitly named `renew_share_overall` column (Eurostat SHARES REN indicator --
# overall share of renewables in gross final energy consumption).
#
# The renewable series is taken from one of three sources (`SOURCE`):
#   "xlsx"     (default) parse data/raw/nrg_ind_ren.xlsx (official Eurostat
#              export, Sheet 1 = overall). Authoritative and offline.
#   "download" fetch Eurostat nrg_ind_ren live (needs network).
#   "archive"  recover from _archive/data/new_data_blended_raw.csv by taking the
#              first (overall) row of each country-year group. Verified against
#              the xlsx: exact (<0.001) for all 135 country-years in the 2014-2018
#              reference window; only 2022-2023 differ, by Eurostat data revision.
# GDP (PPP/real) and the fossil share are carried from the archived panel unless
# SOURCE == "download" (then GDP comes from WDI). The fossil share is unused
# downstream; TODO rebuild it from nrg_bal_c gross available energy in get_data.R.

library(data.table)
library(here)
library(countrycode)
suppressMessages(library(dplyr))
here::i_am("R/get_data_extra.R")
source(here("R/country_classification.R"))

SOURCE <- "xlsx"  # "xlsx" | "download" | "archive"

base_iso3 <- countrycode(base_countries, "country.name", "iso3c")  # names -> ISO3

out_cols <- c("iso3c", "year", "GDP_ppp", "GDP_real",
              "ShareFossils_GrossAvEn", "renew_share_overall")

# --- Renewable overall share (renew_share_overall) ---------------------------
read_ren_xlsx <- function(path) {
  suppressMessages(library(readxl))
  x <- as.data.frame(suppressMessages(
    read_excel(path, sheet = "Sheet 1", col_names = FALSE)))  # Sheet 1 = overall
  yr_row <- which(x[[1]] == "TIME")[1]
  years  <- suppressWarnings(as.integer(unlist(x[yr_row, ])))  # years in value cols
  vcols  <- which(!is.na(years)); years <- years[vcols]
  body   <- x[(yr_row + 2):nrow(x), , drop = FALSE]            # skip TIME + GEO rows
  geo    <- body[[1]]
  keep   <- !is.na(geo) & !grepl("European Union|Euro area", geo) & nzchar(geo)
  body   <- body[keep, , drop = FALSE]
  iso    <- suppressWarnings(countrycode(body[[1]], "country.name", "iso3c"))
  ren <- rbindlist(lapply(seq_along(vcols), function(j)
    data.table(iso3c = iso, year = years[j],
               renew_share_overall = suppressWarnings(as.numeric(body[[vcols[j]]])))))
  ren[!is.na(iso3c) & !is.na(renew_share_overall) & iso3c %in% base_iso3]
}

if (SOURCE == "download") {
  suppressMessages({library(WDI); library(eurostat)})
  gdp <- as.data.table(WDI::WDI(
    country = countrycode(base_countries, "country.name", "iso2c"),
    indicator = c(GDP_ppp = "NY.GDP.MKTP.PP.KD", GDP_real = "NY.GDP.MKTP.KD")))
  gdp[, iso3c := countrycode(iso2c, "iso2c", "iso3c")]
  gdp <- gdp[, .(iso3c, year, GDP_ppp, GDP_real)]
  foss <- unique(fread(here("_archive/data/new_data_blended_raw.csv"))[
    , .(iso3c, year, ShareFossils_GrossAvEn)])  # TODO: from nrg_bal_c instead
  ren <- as.data.table(get_eurostat("nrg_ind_ren", time_format = "num"))
  ren <- ren[nrg_bal == "REN" & unit == "PC"]
  ren[, iso3c := countrycode(geo, "eurostat", "iso3c")]
  ren <- ren[iso3c %in% base_iso3,
             .(iso3c, year = time, renew_share_overall = values)]
  nd <- Reduce(function(a, b) merge(a, b, by = c("iso3c", "year"), all = TRUE),
               list(gdp, foss, ren))

} else {
  gdp_foss <- unique(fread(here("_archive/data/new_data_blended_raw.csv"))[
    , .(iso3c, year, GDP_ppp, GDP_real, ShareFossils_GrossAvEn)])
  if (SOURCE == "xlsx") {
    ren <- read_ren_xlsx(here("data/raw/nrg_ind_ren.xlsx"))
  } else {  # "archive": first row of each group is the overall series
    raw <- fread(here("_archive/data/new_data_blended_raw.csv"))
    raw[, .pos := seq_len(.N), by = .(iso3c, year)]
    ren <- raw[.pos == 1, .(iso3c, year, renew_share_overall = ShareRenewables_GrossAvEn)]
  }
  nd <- merge(gdp_foss, ren, by = c("iso3c", "year"), all = TRUE)
}

nd <- nd[, ..out_cols][order(iso3c, year)]
stopifnot(!anyDuplicated(nd[, .(iso3c, year)]))
fwrite(nd, here("data/tidy/new_data.csv"))
cat(sprintf("new_data.csv written from SOURCE=%s: %d rows, %d countries, years %d-%d.\n",
            SOURCE, nrow(nd), uniqueN(nd$iso3c), min(nd$year), max(nd$year)))
