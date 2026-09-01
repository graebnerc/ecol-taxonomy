# Swap the EXIOBASE footprint layer of data/tidy/full_taxonomy_data.csv for the
# freshly computed 3.10.2 one, leaving every other column untouched.
#
# WHY NOT JUST RE-RUN get_data.R
#   get_data.R rebuilds the whole panel and its download flags default to TRUE,
#   so re-running it would also re-pull WDI and Eurostat and could move columns
#   that have nothing to do with this change. The footprint swap is the only
#   thing we want to change, so this script changes only that: it drops the five
#   EXIOBASE columns and re-joins them from data/tidy/exiobase_totals.csv.
#   Everything else (energy balances, population, patents, fossil shares) is
#   carried over from the committed panel bit-for-bit.
#
#   get_data.R has been updated to read the same source, so a full rebuild from
#   scratch produces the same footprint layer as this incremental path.
#
# WHAT CHANGES
#   * values: EXIOBASE 3.10.2 revised the emission accounts vs the 3.8.x vintage
#     the old extract used (world PBA 2015: 44.9 vs 39.2 Gt; country-level
#     correlation ~0.99 but levels differ by ~11-15%, unevenly)
#   * coverage: the footprint layer now runs to 2024 instead of stopping at 2019
#
# Writes data/tidy/full_taxonomy_data.csv (in place) and prints a before/after
# comparison so nothing moves silently. The previous file is copied to
# data/tidy/_backup_full_taxonomy_data_pre310.csv on first run.

here::i_am("R/update_panel_exiobase.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(countrycode)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

PANEL  <- here("data/tidy/full_taxonomy_data.csv")
TOTALS <- here("data/tidy/exiobase_totals.csv")
BACKUP <- here("data/tidy/_backup_full_taxonomy_data_pre310.csv")

stopifnot(
  "run R/get_data_exiobase.R first" = file.exists(TOTALS),
  "panel not found" = file.exists(PANEL))

EXIO_COLS <- c("GWP_Imports", "GWP_Exports", "Employment_pba",
               "GWP_pba", "ValueAdded_pba")

old <- fread(PANEL)
stopifnot("panel is missing the EXIOBASE columns" =
            all(EXIO_COLS %in% names(old)))
if (!file.exists(BACKUP)) file.copy(PANEL, BACKUP)

eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")
new <- fread(TOTALS)

# Drop years whose emissions are incomplete (see R/get_data_exiobase.R: in the
# 2023/2024 nowcast archives the dominant stressor is identically zero, so their
# GHG totals read ~26% of the true value while nothing errors).
if ("emissions_complete" %in% names(new)) {
  drop_yrs <- sort(unique(new[emissions_complete == FALSE]$year))
  if (length(drop_yrs)) {
    cat(sprintf("Dropping %d year(s) with incomplete emissions: %s\n",
                length(drop_yrs), paste(drop_yrs, collapse = ", ")))
    new <- new[emissions_complete == TRUE]
  }
  new[, emissions_complete := NULL]
} else {
  warning("exiobase_totals.csv has no emissions_complete flag - re-run ",
          "R/get_data_exiobase.R with FORCE = TRUE.", call. = FALSE)
}
new[, country := countrycode(region, "iso2c", "iso3c", warn = FALSE)]
new <- new[country %in% eu_iso3,
           c("country", "year", EXIO_COLS), with = FALSE]
stopifnot("duplicate country-year in the new totals" =
            !anyDuplicated(new[, .(country, year)]))
if (uniqueN(new$country) != 27)
  stop("expected 27 EU countries in the new extract, got ", uniqueN(new$country))

# --- Compare on the overlap BEFORE replacing ---------------------------------
ov <- merge(old[, c("country", "year", EXIO_COLS), with = FALSE],
            new, by = c("country", "year"), suffixes = c("_old", "_new"))
cat(sprintf("\nOverlap: %d country-years (%d-%d)\n", nrow(ov),
            min(ov$year), max(ov$year)))
cat("\n## Old (3.8.x) vs new (3.10.2) on the overlapping years\n\n")
cmp <- rbindlist(lapply(EXIO_COLS, function(cl) {
  o <- ov[[paste0(cl, "_old")]]; n <- ov[[paste0(cl, "_new")]]
  ok <- is.finite(o) & is.finite(n)
  data.table(column = cl,
             ratio_new_old = round(sum(n[ok]) / sum(o[ok]), 3),
             pearson = round(cor(o[ok], n[ok]), 4),
             spearman = round(cor(o[ok], n[ok], method = "spearman"), 4),
             median_country_ratio = round(median((n[ok] / o[ok])[o[ok] != 0]), 3))
}))
print(cmp)

# --- Replace ------------------------------------------------------------------
keep <- setdiff(names(old), EXIO_COLS)
merged <- merge(old[, ..keep], new, by = c("country", "year"), all = TRUE)
setcolorder(merged, names(old))
setorder(merged, country, year)

o_lo <- min(old[!is.na(GWP_pba)]$year);    o_hi <- max(old[!is.na(GWP_pba)]$year)
n_lo <- min(merged[!is.na(GWP_pba)]$year); n_hi <- max(merged[!is.na(GWP_pba)]$year)
cat(sprintf("\nPanel: %d -> %d rows; EXIOBASE coverage %d-%d -> %d-%d\n",
            nrow(old), nrow(merged), o_lo, o_hi, n_lo, n_hi))

# The new extract deliberately covers only the years the pipeline can use
# (R/get_data_exiobase.R: YEARS <- 2013:2024). Everything earlier therefore LOSES
# its footprint values rather than keeping the old vintage -- mixing two EXIOBASE
# releases inside one column would be worse than a documented gap. Say so loudly.
if (n_lo > o_lo)
  warning(sprintf(paste0(
    "EXIOBASE footprint years %d-%d are dropped (old extract had them, the new ",
    "one starts at %d).\n  Nothing in the 01-07 pipeline uses pre-%d data ",
    "(reference window %d-%d, robustness windows within %d-%d), but if you ever ",
    "need the long panel, widen YEARS in R/get_data_exiobase.R and re-run ",
    "(~2.5 min per year)."),
    o_lo, n_lo - 1L, n_lo, n_lo, REF_FIRST_YEAR, REF_LAST_YEAR, n_lo, n_hi),
    call. = FALSE)

# Non-EXIOBASE columns must be untouched on the rows that already existed.
chk <- merge(old[, ..keep], merged[, ..keep], by = c("country", "year"),
             suffixes = c("_o", "_n"))
other <- setdiff(keep, c("country", "year"))
bad <- other[vapply(other, function(cl)
  !isTRUE(all.equal(chk[[paste0(cl, "_o")]], chk[[paste0(cl, "_n")]])), logical(1))]
if (length(bad)) stop("non-EXIOBASE column(s) changed: ", paste(bad, collapse = ", "))
cat("Non-EXIOBASE columns verified unchanged on pre-existing rows.\n")

fwrite(merged, PANEL)
message(sprintf("Wrote full_taxonomy_data.csv (%d rows, %d-%d).",
                nrow(merged), min(merged$year), max(merged$year)))
