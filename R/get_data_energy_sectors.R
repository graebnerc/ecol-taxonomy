# Sectoral split of final energy consumption, EU-27, from Eurostat nrg_bal_c.
#
# WHY. info/PaperTodos.md item 2 (presentation feedback) asks, for the country
# drill-downs after the map, whether a country's energy demand is driven by
# BUSINESS or by HOUSEHOLDS. The taxonomy uses only the total
# (FinalEnergyConsumption), so the split has to be pulled separately. It is a
# descriptive layer for the narrative, NOT an axis input.
#
# Fetched via the Eurostat SDMX REST API (the `eurostat` package is not
# installed in this environment). Dimension order is
# FREQ.NRG_BAL.SIEC.UNIT.GEO -- getting it wrong returns HTTP 400 rather than
# wrong data, which is the good kind of failure.
#
# Balance codes:
#   FC_E         final consumption - energy use (the total the taxonomy uses)
#   FC_IND_E     industry
#   FC_TRA_E     transport
#   FC_OTH_HH_E  households
#   FC_OTH_CP_E  commercial & public services
#   FC_OTH_AF_E  agriculture & forestry
#
# Writes data/tidy/energy_by_sector.csv (country x year x balance, GWh).

here::i_am("R/get_data_energy_sectors.R")
library(here)
suppressMessages({
  library(data.table); library(countrycode); library(dplyr); library(magrittr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

OUT      <- here("data/tidy/energy_by_sector.csv")
RAW      <- here("data/raw/eurostat_nrg_bal_c_sectors.csv")
DOWNLOAD <- !file.exists(RAW)

BALANCES <- c("FC_E", "FC_IND_E", "FC_TRA_E", "FC_OTH_HH_E",
              "FC_OTH_CP_E", "FC_OTH_AF_E")

eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")
eu_iso2 <- countrycode(eu_iso3, "iso3c", "eurostat")   # EL for Greece, not GR

if (DOWNLOAD) {
  url <- sprintf(paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/nrg_bal_c/",
    "A.%s.TOTAL.GWH.%s?format=SDMX-CSV&startPeriod=%d&endPeriod=%d"),
    paste(BALANCES, collapse = "+"), paste(eu_iso2, collapse = "+"),
    REF_FIRST_YEAR - 3L, REF_LAST_YEAR + 1L)
  message("Downloading Eurostat nrg_bal_c sectoral split ...")
  dir.create(dirname(RAW), showWarnings = FALSE, recursive = TRUE)
  utils::download.file(url, RAW, quiet = TRUE)
}
stopifnot("Eurostat download failed" = file.exists(RAW))

raw <- fread(RAW)
stopifnot("unexpected Eurostat response - check the dimension order" =
            all(c("nrg_bal", "geo", "TIME_PERIOD", "OBS_VALUE") %in% names(raw)))

dt <- raw[, .(iso2 = geo, nrg_bal, year = as.integer(TIME_PERIOD),
              gwh = as.numeric(OBS_VALUE))]
dt[, iso3 := countrycode(iso2, "eurostat", "iso3c", warn = FALSE)]
dt <- dt[iso3 %in% eu_iso3 & !is.na(gwh)]

win <- dt[year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR),
          .(gwh = mean(gwh)), by = .(iso3, nrg_bal)]
wide <- dcast(win, iso3 ~ nrg_bal, value.var = "gwh")

miss <- setdiff(eu_iso3, wide$iso3)
if (length(miss)) stop("countries missing from nrg_bal_c: ", paste(miss, collapse = ", "))
for (b in BALANCES)
  if (!b %in% names(wide)) stop("balance code absent from the response: ", b)

# Business = everything that is not households. Transport is kept separate
# because it is neither cleanly business nor household and behaves differently.
wide[, `:=`(
  households_pct = 100 * FC_OTH_HH_E / FC_E,
  industry_pct   = 100 * FC_IND_E / FC_E,
  transport_pct  = 100 * FC_TRA_E / FC_E,
  services_pct   = 100 * FC_OTH_CP_E / FC_E,
  agri_pct       = 100 * FC_OTH_AF_E / FC_E)]
wide[, business_pct := industry_pct + services_pct + agri_pct]

out <- wide[, .(iso3, country = countrycode(iso3, "iso3c", "country.name"),
                final_energy_gwh = FC_E,
                households_pct = round(households_pct, 1),
                business_pct   = round(business_pct, 1),
                industry_pct   = round(industry_pct, 1),
                services_pct   = round(services_pct, 1),
                agri_pct       = round(agri_pct, 1),
                transport_pct  = round(transport_pct, 1))]
setorder(out, -households_pct)

# Shares must account for the whole of FC_E (bar rounding and a small residual).
chk <- wide[, households_pct + business_pct + transport_pct]
if (any(chk < 95 | chk > 105))
  warning("sectoral shares do not sum to ~100% for: ",
          paste(wide$iso3[chk < 95 | chk > 105], collapse = ", "), call. = FALSE)

fwrite(out, OUT)
message(sprintf("Wrote energy_by_sector.csv: %d countries, window %d-%d.",
                nrow(out), REF_FIRST_YEAR, REF_LAST_YEAR))
print(head(out[, .(country, households_pct, business_pct, transport_pct)], 5))
