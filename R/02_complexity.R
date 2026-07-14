# 02 - Green economic complexity (Dimension 4).
#
# Computes ECI, PCI, GCI and GCP (Mealy & Teytelboym 2022; info/GreenComplexity.pdf)
# from Atlas HS92 6-digit export data on the GLOBAL country set, then extracts the
# EU-27. Exports are POOLED over the reference window (2014-2018) into a single
# cross-section; per-year averaging is a Phase 6 robustness check.
#
# Inputs (data/raw/ is gitignored):
#   * data/raw/atlas_hs92_6d.csv          - Atlas HS92 country-product-year exports
#   * data/tidy/green_products_hs6.csv    - green HS6 list (built by R/build_green_list.R)
# Output:
#   * data/tidy/green_complexity_eu.csv   - EU-27: iso3, country, ECI, GCI, GCP, diversity

here::i_am("R/02_complexity.R")
library(here)
suppressMessages({
  library(data.table)
  library(dplyr)
  library(countrycode)
  library(Matrix)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/complexity.R"))

atlas_path <- here("data/raw/atlas_hs92_6d.csv")
green_path <- here("data/tidy/green_products_hs6.csv")
cache_path <- here("data/raw/pooled_exports_1418.rds")   # gitignored derived cache
stopifnot("Green list missing (run build_green_list.R)" = file.exists(green_path))

# --- Load & pool exports over the reference window ---------------------------
# The 968MB Atlas read is slow, so we cache the pooled country x product table.
if (file.exists(cache_path)) {
  message("Loading cached pooled exports (", basename(cache_path), ") ...")
  exp_dt <- readRDS(cache_path)
} else {
  stopifnot("Atlas data missing (see plan Phase 1a)" = file.exists(atlas_path))
  message("Reading Atlas data (first run; will cache) ...")
  atlas <- fread(
    atlas_path,
    select = c("country_iso3_code", "product_hs92_code", "year", "export_value"),
    colClasses = list(character = "product_hs92_code")
  )
  setnames(atlas, c("iso3", "hs6", "year", "export"))
  atlas[, hs6 := formatC(hs6, width = 6, flag = "0")]
  exp_dt <- atlas[year >= REF_FIRST_YEAR & year <= REF_LAST_YEAR & export > 0,
                  .(export = sum(export)), by = .(iso3, hs6)]
  saveRDS(exp_dt, cache_path)
}
message(sprintf("Pooled %d-%d: %d countries x %d products.",
                REF_FIRST_YEAR, REF_LAST_YEAR,
                uniqueN(exp_dt$iso3), uniqueN(exp_dt$hs6)))

green_codes <- fread(green_path, colClasses = list(character = "hs6"))$hs6

# --- Compute complexity ------------------------------------------------------
rca <- build_rca_matrix(exp_dt)
message(sprintf("RCA matrix after filters: %d countries x %d products.",
                nrow(rca$M), ncol(rca$M)))

ci  <- complexity_indices(rca$M)
gci <- green_indicators(rca$M, ci$PCI, green_codes)
gci$ECI <- ci$ECI[gci$iso3]

# --- Validation --------------------------------------------------------------
rank_tbl <- gci |>
  mutate(country = countrycode(iso3, "iso3c", "country.name")) |>
  arrange(desc(ECI))
cat("\n--- ECI top 10 (expect advanced manufacturers) ---\n")
print(head(rank_tbl[, c("country", "ECI")], 10), row.names = FALSE)
cat("\n--- ECI bottom 10 (expect resource exporters) ---\n")
print(tail(rank_tbl[, c("country", "ECI")], 10), row.names = FALSE)
cat("\n--- GCI top 10 (Mealy: Germany #1) ---\n")
print(head(arrange(rank_tbl, desc(GCI))[, c("country", "GCI")], 10), row.names = FALSE)
cat(sprintf("\ncor(GCI, ECI) = %.3f  (Mealy: strongly positive, but distinct)\n",
            cor(gci$GCI, gci$ECI)))

# --- Extract EU-27 & write ---------------------------------------------------
eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")
out <- gci |>
  filter(iso3 %in% eu_iso3) |>
  mutate(country = countrycode(iso3, "iso3c", "country.name")) |>
  select(iso3, country, ECI, GCI, GCP, diversity) |>
  arrange(desc(GCI))

missing_eu <- setdiff(eu_iso3, out$iso3)
if (length(missing_eu))
  message("NOTE: EU countries absent from complexity output (filtered out): ",
          paste(countrycode(missing_eu, "iso3c", "country.name"), collapse = ", "))

fwrite(out, here("data/tidy/green_complexity_eu.csv"))
cat("\n--- EU-27 by GCI ---\n"); print(out, row.names = FALSE)
message("Wrote data/tidy/green_complexity_eu.csv")
