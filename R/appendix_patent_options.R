# Appendix - the three green-patent measurement options, side by side.
#
# The green-innovation standalone is the most discriminating variable in the
# potential axis (14-fold spread across quadrants), and it is the ONLY input that
# caps the reference window at 2018. Once EXIOBASE covers 1995-2024, every other
# input supports a 2019-2023 window; patents do not, because
# sql/get_green_patents.sql counts EPO GRANTS by filing year and EPO grant lag is
# 3-5+ years. So the window question is really a patent-measure question.
#
# THE THREE OPTIONS
#   A  grants        status quo. Measures successful innovation. Grant-lag
#                    truncated -> window capped at 2018.
#   B  applications  measures filing activity, complete ~18 months after filing
#                    -> window can move to ~2021.
#   C  hybrid        grants for the headline, applications for an extended-window
#                    robustness spec. Costs nothing if A and B rank countries
#                    alike; is incoherent if they do not.
#
# The whole comparison turns on one question: DO GRANTS AND APPLICATIONS RANK
# COUNTRIES THE SAME? If yes, B is a safe swap and C is redundant. If no, the
# window cannot be moved without changing what the potential axis measures.
#
# DATA. The OECD ENV-TECH database carries both measures on the same underlying
# EPO data, so grants-vs-applications is tested without confounding it with a
# source change (R/get_data_patents_oecd.R). The repo's own PATSTAT v1 grants
# series is included as a third series to check that the OECD grants series is a
# faithful stand-in for it. When the PATSTAT v2 extract arrives (see
# sql/get_green_patents_v2.sql) this script picks it up automatically and adds
# the like-for-like PATSTAT applications series.
#
# Writes data/tidy/patent_options.csv, data/tidy/patent_options_scores.csv and
# plots/patent_options.{png,pdf}.

here::i_am("R/appendix_patent_options.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr)
  library(ggplot2); library(countrycode); library(knitr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/typology.R"))

GRANT_COL <- "#2E7CA8"; APP_COL <- "#E65032"; INK <- "#1A1A1A"

ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
ind$iso3 <- countrycode(ind$country, "country.name", "iso3c")
ind$group <- get_country_classification(ind$iso3, "jee")

pop <- fread(here("data/tidy/full_taxonomy_data.csv"))[
  , .(iso3 = country, year, population = population * 1000)]

oecd <- fread(here("data/tidy/oecd_green_patents.csv"))
oecd <- oecd[tech == "climate_mitigation"]        # closest to the repo's Y02 + Y04S

v1 <- fread(here("data/tidy/patstat_green-patents.csv"))
v1[, iso3 := countrycode(country, "iso2c", "iso3c", warn = FALSE)]
v1 <- v1[iso3 %in% ind$iso3, .(iso3, year, n = as.numeric(n_patents),
                               series = "PATSTAT v1 grants")]

series <- rbind(
  v1,
  oecd[measure == "grants",       .(iso3, year, n = n_patents, series = "OECD grants")],
  oecd[measure == "applications", .(iso3, year, n = n_patents, series = "OECD applications")]
)

# PATSTAT v2, if it has been retrieved (sql/get_green_patents_v2.sql).
v2_path <- here("data/tidy/green_patents_panel.csv")
HAVE_V2 <- file.exists(v2_path)
if (HAVE_V2) {
  v2 <- fread(v2_path)
  series <- rbind(series,
    v2[, .(iso3, year, n = grants,       series = "PATSTAT v2 grants")],
    v2[, .(iso3, year, n = applications, series = "PATSTAT v2 applications")])
  message("PATSTAT v2 extract found - included in the comparison.")
} else {
  message("PATSTAT v2 extract absent - comparison runs on OECD + PATSTAT v1.\n",
          "  Run sql/get_green_patents_v2.sql, then R/get_data_patents_patstat.R.")
}

# --- 1. Truncation: last year each series is still ~complete ------------------

tot <- series[iso3 %in% ind$iso3, .(total = sum(n, na.rm = TRUE)), by = .(series, year)]
tot[, idx := round(100 * total / total[year == 2018]), by = series]
trunc_tbl <- dcast(tot[year %between% c(2014, 2023)], year ~ series, value.var = "idx")

cat("\n## Truncation profile (EU-27 total, 2018 = 100)\n\n")
print(kable(trunc_tbl, format = "pipe"))
last_ok <- tot[idx >= 90, .(last_usable_year = max(year)), by = series][order(series)]
cat("\nLast year still within 10% of the 2018 level:\n")
print(kable(last_ok, format = "pipe"))

# --- 2. Do the measures rank countries alike? --------------------------------

win <- function(dt, y1, y2) {
  dt[year %between% c(y1, y2), .(n = sum(n, na.rm = TRUE)), by = .(series, iso3)] |>
    merge(pop[year %between% c(y1, y2), .(population = mean(population)), by = iso3],
          by = "iso3") |>
    _[, per_m := n / (population / 1e6)]
}
w1418 <- win(series, 2014, 2018)
wide <- dcast(w1418, iso3 ~ series, value.var = "per_m")
nm <- setdiff(names(wide), "iso3")
rk <- outer(nm, nm, Vectorize(function(a, b)
  round(cor(wide[[a]], wide[[b]], method = "spearman", use = "complete.obs"), 3)))
dimnames(rk) <- list(nm, nm)
cat("\n\n## Cross-country rank correlation of the measures (per capita, 2014-2018)\n\n")
print(kable(as.data.frame(rk), format = "pipe"))

# --- 3. What each option does to the map -------------------------------------

score_with <- function(patents_per_m) {
  d <- ind; d$GreenPatents_alt <- patents_per_m
  v <- axis_score(d, INTENSITY_VARS,  "CarbonIntensity_normed", FOSSIL_VAR)$score
  p <- axis_score(d, COMPLEXITY_VARS, "GCI", "GreenPatents_alt")$score
  list(v = v, p = p, q = assign_quadrant(v, p, "short"))
}
base <- score_with(ind$GreenPatents_normed)

opts <- list()
for (s in nm) {
  m <- match(ind$iso3, wide$iso3)
  opts[[s]] <- score_with(wide[[s]][m])
}
cmp <- lapply(names(opts), function(s) {
  o <- opts[[s]]
  data.frame(measure = s,
             cor_potential = round(cor(base$p, o$p, method = "spearman"), 3),
             quad_changes  = sum(o$q != base$q),
             moved = paste(ind$country[o$q != base$q], collapse = ", "))
}) |> bind_rows()
cat("\n\n## Effect on the typology, vs the current headline (PATSTAT v1 grants)\n\n")
print(kable(cmp, format = "pipe"))

fwrite(w1418, here("data/tidy/patent_options.csv"))
fwrite(cmp,   here("data/tidy/patent_options_scores.csv"))

# --- 4. Figure: truncation, and the grants/applications agreement ------------

p1dat <- tot[year %between% c(2012, 2023) &
               series %in% c("OECD grants", "OECD applications",
                             "PATSTAT v1 grants")]
p1 <- ggplot(p1dat, aes(year, idx, colour = series, linetype = series)) +
  geom_hline(yintercept = 90, linetype = 3, colour = "grey60") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.5) +
  scale_colour_manual(NULL, values = c("OECD applications" = APP_COL,
                                       "OECD grants" = GRANT_COL,
                                       "PATSTAT v1 grants" = "grey35")) +
  scale_linetype_manual(NULL, values = c("OECD applications" = 1,
                                         "OECD grants" = 1,
                                         "PATSTAT v1 grants" = 2)) +
  scale_x_continuous(breaks = seq(2012, 2023, 2)) +
  labs(x = NULL, y = "EU-27 total (2018 = 100)",
       title = "Grant lag, not publication lag, is what caps the window",
       subtitle = paste("Applications stay complete about three years longer than",
                        "grants.\nDotted line = 90% of the 2018 level.")) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15))

ggsave(here("plots/patent_options.png"), p1, width = 6.6, height = 4.4, dpi = 300)
ggsave(here("plots/patent_options.pdf"), p1, width = 6.6, height = 4.4)

message("\nappendix_patent_options.R done.")
