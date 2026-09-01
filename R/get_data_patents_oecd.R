# Build the OECD green-patent panel: an APPLICATIONS-based counterpart to the
# repo's PATSTAT grants extract, needed to test whether the reference window can
# move past 2018.
#
# WHY THIS EXISTS
# ---------------
# sql/get_green_patents.sql counts EPO patents with `granted = 'Y'` by
# appln_filing_year. EPO grant lag is 3-5+ years, so against a 2026 snapshot the
# recent filing cohorts are severely undercounted (EU-27: 2018 = 5204, 2019 =
# 3781, 2020 = 1863, 2021 = 490, 2022 = 20). Waiting does not fix this quickly -
# roughly one usable filing year is gained per year of waiting. Counting
# APPLICATIONS instead removes the grant lag (only the ~18-month publication lag
# remains), at the cost of measuring filing activity rather than successful
# innovation.
#
# The OECD ENV-TECH patent database carries BOTH measures on the same underlying
# data, so grants-vs-applications can be compared without confounding the
# comparison with a source change. That is what makes the three options testable.
#
# SOURCE
# ------
# OECD Data Explorer, "Environment-related technologies patents"
#   dataflow OECD.STI.PIE:DSD_PATENTS@DF_PATENTS_ENVIROMENT (1.0)
# Fetched via the SDMX REST API (see DOWNLOAD block below) to
# data/raw/oecd/oecd_env_patents.csv (~213 MB, gitignored).
#
# SLICE, chosen to match sql/get_green_patents.sql as closely as the OECD
# dimensions allow:
#   PATENT_AUTHORITIES = 6F0        European Patent Office   (repo: appln_auth = 'EP')
#   AGENT_ROLE         = APPLICANT  applicant country        (repo: applt_seq_nr > 0)
#   DATE_TYPE          = APPLICATION filing year             (repo: appln_filing_year)
#   MEASURE            = AP | GR    applications | grants
#   OECD_TECHNOLOGY_PATENT = CCM ("climate change mitigation", closest to the
#     repo's Y02 + Y04S) and ENV_PAT ("environment-related technologies", the
#     broader OECD headline). Both are kept so the choice can be made on evidence
#     in R/appendix_patent_options.R rather than by assertion.
#
# Writes data/tidy/oecd_green_patents.csv: iso3 x year x measure x tech.

here::i_am("R/get_data_patents_oecd.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(countrycode)
})
source(here("R/country_classification.R"))

RAW <- here("data/raw/oecd/oecd_env_patents.csv")
DOWNLOAD <- FALSE   # TRUE re-fetches the 213 MB SDMX extract

if (DOWNLOAD || !file.exists(RAW)) {
  dir.create(dirname(RAW), showWarnings = FALSE, recursive = TRUE)
  url <- paste0(
    "https://sdmx.oecd.org/public/rest/data/",
    "OECD.STI.PIE,DSD_PATENTS@DF_PATENTS_ENVIROMENT,1.0/all",
    "?startPeriod=2010&endPeriod=2023",
    "&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
  message("Downloading OECD ENV-TECH patents (~213 MB) ...")
  utils::download.file(url, RAW, mode = "wb", quiet = FALSE)
}
stopifnot("OECD patent extract missing - set DOWNLOAD = TRUE" = file.exists(RAW))

eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")

raw <- fread(RAW, select = c("PATENT_AUTHORITIES", "MEASURE", "DATE_TYPE",
                             "AGENT_ROLE", "OECD_TECHNOLOGY_PATENT",
                             "REF_AREA", "TIME_PERIOD", "OBS_VALUE"))

pat <- raw[PATENT_AUTHORITIES == "6F0" &          # European Patent Office
             AGENT_ROLE == "APPLICANT" &          # applicant country
             DATE_TYPE == "APPLICATION" &         # filing year
             MEASURE %in% c("AP", "GR") &
             OECD_TECHNOLOGY_PATENT %in% c("CCM", "ENV_PAT") &
             REF_AREA %in% eu_iso3 &
             !is.na(OBS_VALUE)]

out <- pat[, .(iso3 = REF_AREA,
               year = as.integer(TIME_PERIOD),
               measure = fifelse(MEASURE == "AP", "applications", "grants"),
               tech = fifelse(OECD_TECHNOLOGY_PATENT == "CCM",
                              "climate_mitigation", "environment_all"),
               n_patents = as.numeric(OBS_VALUE))]
setorder(out, tech, measure, iso3, year)

stopifnot("duplicate iso3 x year x measure x tech rows" =
            !anyDuplicated(out[, .(iso3, year, measure, tech)]))

fwrite(out, here("data/tidy/oecd_green_patents.csv"))

cov <- out[, .(countries = uniqueN(iso3), total = sum(n_patents)),
           by = .(tech, measure, year)][order(tech, measure, year)]
message(sprintf("Wrote oecd_green_patents.csv: %d rows, %d countries, %d-%d.",
                nrow(out), uniqueN(out$iso3), min(out$year), max(out$year)))
print(dcast(cov[tech == "climate_mitigation"], year ~ measure, value.var = "total"))
