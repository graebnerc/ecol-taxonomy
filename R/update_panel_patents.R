# Add the green-patent APPLICATIONS series to data/tidy/full_taxonomy_data.csv.
#
# WHY. The panel's `GreenPatents_n` is EPO GRANTS by filing year
# (sql/get_green_patents.sql). EPO grant lag is 3-5+ years, so cohorts after
# 2018 are severely truncated -- which is what capped the reference window.
# Applications are complete ~18 months after filing and rank EU-27 countries
# essentially identically (Spearman 0.99; swapping the measure moves 0/27
# countries -- R/appendix_patent_options.R), so they carry a recent window at no
# measurable cost. `PATENT_MEASURE` in R/config.R selects which one the headline
# uses; both are always present.
#
# SOURCE PREFERENCE. If the PATSTAT v2 extract exists
# (data/tidy/green_patents_panel.csv, produced by sql/get_green_patents_v2.sql +
# R/get_data_patents_patstat.R) it is used, because it is like-for-like with the
# grants series -- same database, same CPC filter, same country attribution.
# Otherwise the OECD ENV-TECH applications series stands in
# (R/get_data_patents_oecd.R). The source actually used is reported.
#
# Writes data/tidy/full_taxonomy_data.csv in place, adding GreenPatentsApps_n.

here::i_am("R/update_panel_patents.R")
library(here)
suppressMessages({
  library(data.table); library(countrycode)
  library(dplyr); library(magrittr)   # country_classification.R uses %>%
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

PANEL <- here("data/tidy/full_taxonomy_data.csv")
V2    <- here("data/tidy/green_patents_panel.csv")
OECD  <- here("data/tidy/oecd_green_patents.csv")

panel <- fread(PANEL)
eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")

if (file.exists(V2)) {
  src <- "PATSTAT v2 (like-for-like with the grants series)"
  apps <- fread(V2)[, .(country = iso3, year, GreenPatentsApps_n = applications)]
} else {
  src <- "OECD ENV-TECH (PATSTAT v2 not yet retrieved)"
  apps <- fread(OECD)[tech == "climate_mitigation" & measure == "applications",
                      .(country = iso3, year, GreenPatentsApps_n = n_patents)]
}
message("Applications source: ", src)

stopifnot("duplicate country-year in the applications series" =
            !anyDuplicated(apps[, .(country, year)]))

# TRUE ZEROS vs MISSING DATA. The PATSTAT GROUP BY emits no row for a
# country-year with no qualifying application, so an absent row is a genuine
# zero, not a gap -- the same convention get_data.R already applies to the v1
# grants series (BG, CY, EE, HR, LT, LV, MT, RO). Silently filling every gap
# with 0 would however hide a real data problem, so a filled cell is only
# accepted where a zero is PLAUSIBLE: the country must appear elsewhere in the
# series and its counts in the window must be small. A gap in a country that
# files in volume is an error, not a zero.
grid <- CJ(country = eu_iso3, year = REF_FIRST_YEAR:REF_LAST_YEAR)
apps_win <- merge(grid, apps, by = c("country", "year"), all.x = TRUE)

absent_country <- setdiff(eu_iso3, unique(apps$country))
if (length(absent_country))
  stop("EU state(s) absent from the applications series entirely: ",
       paste(absent_country, collapse = ", "))

gaps <- apps_win[is.na(GreenPatentsApps_n)]
if (nrow(gaps)) {
  scale <- apps_win[, .(mx = max(GreenPatentsApps_n, na.rm = TRUE)), by = country]
  implausible <- merge(gaps, scale, by = "country")[mx > 20]
  if (nrow(implausible))
    stop("missing country-year(s) where a true zero is implausible ",
         "(the country files in volume elsewhere in the window):\n",
         paste(sprintf("  %s %d (max in window: %d)", implausible$country,
                       implausible$year, implausible$mx), collapse = "\n"))
  cat(sprintf("Filling %d absent country-year(s) as true zeros: %s\n", nrow(gaps),
              paste(sprintf("%s %d", gaps$country, gaps$year), collapse = ", ")))
  apps_win[is.na(GreenPatentsApps_n), GreenPatentsApps_n := 0]
}
apps <- rbind(apps[!(paste(country, year) %in% paste(apps_win$country, apps_win$year))],
              apps_win)

if ("GreenPatentsApps_n" %in% names(panel)) panel[, GreenPatentsApps_n := NULL]
panel <- merge(panel, apps, by = c("country", "year"), all.x = TRUE)
setorder(panel, country, year)

# A missing count inside the window would silently become NA in the indicator
# mean, so require completeness there; outside it, NA is fine and expected
# (the applications series starts in 2010).
win <- panel[country %in% eu_iso3 & year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR)]
stopifnot("gap-filling failed - NA remains inside the reference window" =
            !anyNA(win$GreenPatentsApps_n),
          "reference window is not fully populated" =
            nrow(win) == 27L * (REF_LAST_YEAR - REF_FIRST_YEAR + 1L))

fwrite(panel, PANEL)
cat(sprintf("\nAdded GreenPatentsApps_n from %s.\n", src))
cat(sprintf("Window %d-%d: %d countries, EU-27 total %.0f applications.\n",
            REF_FIRST_YEAR, REF_LAST_YEAR, uniqueN(win$country),
            sum(win$GreenPatentsApps_n)))
