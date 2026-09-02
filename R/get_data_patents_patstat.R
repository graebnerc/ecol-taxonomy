# Ingest and validate the PATSTAT v2 green-patent extract.
#
# READY TO RUN: this script is written before the data exists. Run
# sql/get_green_patents_v2.sql against a current PATSTAT edition, save the result
# as data/tidy/patstat_green-patents_v2.csv, then:
#
#     Rscript R/get_data_patents_patstat.R
#
# It validates the file, quantifies the two defects the v2 query fixes
# (CPC/applicant double counting, and grant-lag truncation), and writes the tidy
# panel data/tidy/green_patents_panel.csv consumed by R/functions/indicators.R.
#
# If the file is absent the script exits with a clear message and changes
# nothing, so the rest of the pipeline keeps running on the old extract.
#
# Expected columns (see the SQL header): year, country, n_applications,
# n_granted, n_raw_join, n_applicants, n_cpc_matches.

here::i_am("R/get_data_patents_patstat.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(countrycode)
})
source(here("R/country_classification.R"))

# Accept either the documented location or the query's own filename dropped into
# data/raw/ -- the latter is what you get by saving the SQL result directly.
# sql/ first: data/raw/ is gitignored, and a PATSTAT extract cannot be
# regenerated without database access, so it belongs beside its query in version
# control rather than in a directory reserved for re-downloadable sources.
CANDIDATES <- c(here("sql/get_green_patents_v2.csv"),
                here("data/tidy/patstat_green-patents_v2.csv"),
                here("data/raw/get_green_patents_v2.csv"),
                here("data/raw/patstat_green-patents_v2.csv"))
RAW <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RAW)) RAW <- CANDIDATES[1]
OLD <- here("data/tidy/patstat_green-patents.csv")   # the grants-only v1 extract

if (!file.exists(RAW)) {
  message(
    "\n", strrep("-", 74), "\n",
    "PATSTAT v2 extract not found:\n  ", RAW, "\n\n",
    "To produce it:\n",
    "  1. run sql/get_green_patents_v2.sql against a current PATSTAT edition\n",
    "  2. save the result as the path above (CSV, header row included)\n",
    "  3. re-run: Rscript R/get_data_patents_patstat.R\n\n",
    "Nothing was changed. The pipeline continues to use the v1 grants extract\n",
    "(data/tidy/patstat_green-patents.csv), which is grant-lag truncated from\n",
    "2019 and therefore caps the reference window at 2018.\n",
    strrep("-", 74))
  quit(save = "no", status = 0)
}

message("Reading ", RAW)
pat <- fread(RAW)

# --- Validation: fail loudly, not silently -----------------------------------
need <- c("year", "country", "n_applications", "n_granted")
missing <- setdiff(need, names(pat))
if (length(missing))
  stop("PATSTAT v2 extract is missing required column(s): ",
       paste(missing, collapse = ", "),
       "\nGot: ", paste(names(pat), collapse = ", "),
       "\nDid the query run unmodified? See sql/get_green_patents_v2.sql.")

stopifnot(
  "duplicate year x country rows in the extract" =
    !anyDuplicated(pat[, .(year, country)]),
  "n_granted exceeds n_applications somewhere - check the conditional aggregate" =
    all(pat$n_granted <= pat$n_applications),
  "no rows with year >= 2019 - the filing-year bound was not extended" =
    any(pat$year >= 2019))

eu_iso2 <- countrycode(base_countries, "country.name", "iso2c")
eu <- pat[country %in% eu_iso2]
n_eu <- uniqueN(eu$country)
if (n_eu < 27)
  warning(sprintf("only %d of 27 EU member states present (missing: %s)", n_eu,
                  paste(setdiff(eu_iso2, eu$country), collapse = ", ")))

# --- Diagnostic 1: how much did the old query over-count? --------------------
if ("n_raw_join" %in% names(pat)) {
  infl <- eu[year %between% c(2010, 2018),
             .(applications = sum(n_applications), raw = sum(n_raw_join)),
             by = country][, ratio := raw / applications][order(-ratio)]
  cat("\n== Double counting in the v1 query (raw join / distinct), 2010-2018 ==\n")
  cat(sprintf("EU-27 overall inflation factor: %.2fx\n",
              sum(infl$raw) / sum(infl$applications)))
  cat("Most and least inflated member states (differential bias is the problem):\n")
  print(rbind(head(infl, 5), tail(infl, 5)), row.names = FALSE)
}

# --- Diagnostic 2: grant-lag truncation, applications vs grants --------------
cov <- eu[, .(applications = sum(n_applications), granted = sum(n_granted)),
          by = year][order(year)]
cov[, `:=`(app_vs_2018 = round(100 * applications / applications[year == 2018]),
           grant_vs_2018 = round(100 * granted / granted[year == 2018]))]
cat("\n== Applications vs grants by filing year (EU-27 totals, 2018 = 100) ==\n")
print(cov[year >= 2012], row.names = FALSE)
cat("\nThe last year where grants are >= 90% of the 2018 level is the effective\n",
    "cap of a grants-based window; applications should stay usable ~3 years longer.\n", sep = "")

# --- Cross-check against the v1 extract on mature cohorts --------------------
if (file.exists(OLD)) {
  v1 <- fread(OLD)[, .(country, year, n_v1 = n_patents)]
  cmp <- merge(eu[, .(country, year, n_granted, n_applications)], v1,
               by = c("country", "year"))[year %between% c(2014, 2018)]
  if (nrow(cmp)) {
    agg <- cmp[, lapply(.SD, sum), by = country,
               .SDcols = c("n_granted", "n_applications", "n_v1")]
    cat("\n== v2 vs v1 on the mature 2014-2018 window (cross-country) ==\n")
    cat(sprintf("  Spearman(v2 grants, v1)       = %.3f\n",
                cor(agg$n_granted, agg$n_v1, method = "spearman")))
    cat(sprintf("  Spearman(v2 applications, v1) = %.3f\n",
                cor(agg$n_applications, agg$n_v1, method = "spearman")))
    cat(sprintf("  Spearman(v2 applications, v2 grants) = %.3f\n",
                cor(agg$n_applications, agg$n_granted, method = "spearman")))
    cat("  (a high applications-vs-grants rank correlation is what licenses\n",
        "   swapping the measure to move the reference window.)\n", sep = "")
  }
}

# --- Write the tidy panel ----------------------------------------------------
out <- eu[, .(iso2 = country,
              iso3 = countrycode(country, "iso2c", "iso3c"),
              year,
              applications = as.numeric(n_applications),
              grants = as.numeric(n_granted))]
setorder(out, iso3, year)
fwrite(out, here("data/tidy/green_patents_panel.csv"))
message(sprintf("\nWrote green_patents_panel.csv: %d rows, %d countries, %d-%d.",
                nrow(out), uniqueN(out$iso3), min(out$year), max(out$year)))
