# Appendix - can the reference window move, and what happens if it does?
#
# BACKGROUND. The window was frozen at 2014-2018 because EXIOBASE value added
# (the denominator of both intensities) stopped at 2019. EXIOBASE 3.10.2 now runs
# to 2024, so that constraint is gone and the binding constraint moved to the
# green-patent variable: sql/get_green_patents.sql counts EPO GRANTS by filing
# year, and grant lag truncates every cohort after 2018.
#
# R/appendix_patent_options.R establishes that APPLICATIONS and GRANTS rank
# EU-27 countries essentially identically (Spearman ~0.99; substituting either
# moves 0 of 27 countries), while applications stay complete about three years
# longer. That licenses using an applications-based patent variable to move the
# window -- which is what this script does.
#
# It rebuilds the ENTIRE typology (complexity re-pooled from the Atlas, all
# indicators re-averaged, patents re-measured) on each candidate window and
# reports how far the map moves. Nothing here changes the headline; it produces
# the evidence for that decision.
#
# Needs data/raw/exports_by_year_1224.rds (the widened Atlas cache; built by the
# block below on first run from the 968MB Atlas file).
#
# Writes data/tidy/window_options.csv and plots/window_options.{png,pdf}.

here::i_am("R/appendix_window_options.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr); library(ggplot2)
  library(countrycode); library(knitr); library(Matrix); library(ggrepel)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/complexity.R"))
source(here("R/functions/indicators.R"))
source(here("R/functions/typology.R"))

CACHE <- here("data/raw/exports_by_year_1224.rds")
if (!file.exists(CACHE)) {
  atlas_path <- here("data/raw/atlas_hs92_6d.csv")
  stopifnot("Atlas data missing" = file.exists(atlas_path))
  message("Building the widened Atlas cache (968MB read, once) ...")
  atlas <- fread(atlas_path,
    select = c("country_iso3_code", "product_hs92_code", "year", "export_value"),
    colClasses = list(character = "product_hs92_code", double = "export_value"))
  setnames(atlas, c("iso3", "hs6", "year", "export"))
  atlas[, hs6 := formatC(hs6, width = 6, flag = "0")]
  saveRDS(atlas[year %between% c(2012, 2024) & export > 0,
                .(export = sum(export)), by = .(iso3, hs6, year)], CACHE)
  rm(atlas); gc(FALSE)
}
eby <- as.data.table(readRDS(CACHE))

green <- fread(here("data/tidy/green_products_hs6.csv"),
               colClasses = list(character = "hs6"))
base_data  <- as_tibble(fread(here("data/tidy/full_taxonomy_data.csv")))
extra_data <- as_tibble(fread(here("data/tidy/new_data.csv")))
oecd <- fread(here("data/tidy/oecd_green_patents.csv"))[tech == "climate_mitigation"]
pop  <- fread(here("data/tidy/full_taxonomy_data.csv"))[
  , .(iso3 = country, year, population = population * 1000)]

# PATSTAT v2 (applications, like-for-like) if it has been retrieved.
v2_path <- here("data/tidy/green_patents_panel.csv")
HAVE_V2 <- file.exists(v2_path)
if (HAVE_V2) v2 <- fread(v2_path)

# Patent variable for a window, on a chosen measure.
patents_for <- function(y1, y2, meas = c("patstat_v1_grants",
                                        "oecd_applications",
                                        "patstat_v2_applications")) {
  # NB `meas`, not `measure`: `measure` is also a column of `oecd`, and inside a
  # data.table i-expression the column would win over the argument.
  meas <- match.arg(meas)
  src <- switch(meas,
    patstat_v1_grants = NULL,   # taken from the panel by build_indicator_table
    oecd_applications = oecd[measure == "applications",
                             .(iso3, year, n = n_patents)],
    patstat_v2_applications = if (HAVE_V2)
      v2[, .(iso3, year, n = applications)] else NULL)
  if (is.null(src)) return(NULL)
  src[year %between% c(y1, y2), .(n = sum(n, na.rm = TRUE)), by = iso3] |>
    merge(pop[year %between% c(y1, y2), .(population = mean(population)), by = iso3],
          by = "iso3") |>
    _[, .(iso3, patents_per_m = n / (population / 1e6))]
}

compute_gi <- function(exp_dt) {
  rca <- build_rca_matrix(exp_dt, min_country_export = 5e9)
  ci  <- complexity_indices(rca$M)
  green_indicators(rca$M, ci$PCI, green$hs6)
}

build_window <- function(y1, y2, meas) {
  gi <- compute_gi(eby[year %between% c(y1, y2), .(export = sum(export)),
                       by = .(iso3, hs6)])
  ind <- build_indicator_table(base_data, extra_data, first_year = y1, last_year = y2)
  ind$iso3 <- countrycode(ind$country, "country.name", "iso3c")
  m <- match(ind$iso3, gi$iso3)
  ind$GCI <- gi$GCI[m]; ind$GCP <- gi$GCP[m]
  if (anyNA(ind$GCI)) stop("EU country lost from complexity in ", y1, "-", y2)
  pat <- patents_for(y1, y2, meas)
  if (!is.null(pat)) {
    ind$GreenPatents_normed <- pat$patents_per_m[match(ind$iso3, pat$iso3)]
    if (anyNA(ind$GreenPatents_normed)) stop("patent gap in ", y1, "-", y2)
  }
  ind
}

score_of <- function(ind) {
  v <- axis_score(ind, INTENSITY_VARS,  "CarbonIntensity_normed", FOSSIL_VAR)$score
  p <- axis_score(ind, COMPLEXITY_VARS, "GCI", INNOV_VAR)$score
  list(country = ind$country, v = v, p = p,
       q = assign_quadrant(v, p, "short"),
       r2v = summary(lm(v ~ log(ind$GDP_normed)))$r.squared,
       r2p = summary(lm(p ~ log(ind$GDP_normed)))$r.squared,
       cor_vp = cor(v, p))
}

# --- Candidate windows --------------------------------------------------------
# 2014-2018 is the current headline. The others are the most recent windows each
# patent measure can support without importing truncation.
specs <- list(
  list(name = "2014-2018 (headline)", y1 = 2014, y2 = 2018, m = "patstat_v1_grants"),
  list(name = "2014-2018, OECD apps", y1 = 2014, y2 = 2018, m = "oecd_applications"),
  list(name = "2016-2020, OECD apps", y1 = 2016, y2 = 2020, m = "oecd_applications"),
  list(name = "2017-2021, OECD apps", y1 = 2017, y2 = 2021, m = "oecd_applications"))
if (HAVE_V2)
  specs <- c(specs, list(
    list(name = "2018-2022, PATSTAT v2 apps", y1 = 2018, y2 = 2022,
         m = "patstat_v2_applications"),
    list(name = "2019-2023, PATSTAT v2 apps", y1 = 2019, y2 = 2023,
         m = "patstat_v2_applications")))

res <- lapply(specs, function(s) {
  message("  ", s$name)
  sc <- score_of(build_window(s$y1, s$y2, s$m))
  c(list(name = s$name), sc)
})
base <- res[[1]]

out <- rbindlist(lapply(res, function(r) data.table(
  window = r$name,
  cor_vuln = round(cor(base$v, r$v, method = "spearman"), 2),
  cor_pot  = round(cor(base$p, r$p, method = "spearman"), 2),
  quad_changes = sum(r$q != base$q),
  r2_vuln_gdp = round(r$r2v, 2), r2_pot_gdp = round(r$r2p, 2),
  cor_axes = round(r$cor_vp, 2),
  moved = paste(base$country[r$q != base$q], collapse = ", "))))

cat("\n## Reference-window options, full typology rebuilt on each\n\n")
print(kable(out[, .(window, cor_vuln, cor_pot, quad_changes,
                    r2_vuln_gdp, r2_pot_gdp, cor_axes)], format = "pipe"))
cat("\nCountries moving vs the 2014-2018 headline:\n\n")
print(kable(out[quad_changes > 0, .(window, moved)], format = "pipe"))
fwrite(out, here("data/tidy/window_options.csv"))

# --- Figure: where countries sit under the headline vs the most recent window -
last <- res[[length(res)]]
pl <- rbind(
  data.table(country = base$country, v = base$v, p = base$p, w = base$name),
  data.table(country = last$country, v = last$v, p = last$p, w = last$name))
pl[, w := factor(w, levels = c(base$name, last$name))]
pl[, group := as.character(get_country_classification(
  countrycode(country, "country.name", "iso3c"), "jee"))]

p <- ggplot(pl, aes(v, p)) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey80") +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey80") +
  geom_point(aes(colour = group), size = 1.9) +
  ggrepel::geom_text_repel(aes(label = country), size = 2.3, max.overlaps = 14,
                           segment.colour = "grey75", segment.size = 0.2) +
  facet_wrap(~w) +
  scale_colour_manual(NULL, values = c(Core = "#0B3C5D", Finance = "#2E7CA8",
                                       Periphery = "#E65032", Workbench = "#F09372")) +
  labs(x = "Vulnerability (transition burden) →",
       y = "Potential (green capability) →",
       title = "Does the map survive moving to a recent window?",
       subtitle = "Full typology rebuilt on each window: complexity re-pooled, indicators re-averaged, patents re-measured.") +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 8.5),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8))

ggsave(here("plots/window_options.png"), p, width = 9.2, height = 5.2, dpi = 300)
ggsave(here("plots/window_options.pdf"), p, width = 9.2, height = 5.2)

message("\nappendix_window_options.R done.")
