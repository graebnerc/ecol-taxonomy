# Appendix - is green capability becoming less income-dependent over time?
#
# THE OBSERVATION. R2(potential ~ log GDP p.c.) falls as the reference window
# moves forward: 0.37 (2014-17), 0.33 (2014-18), 0.21 (2017-21), 0.19 (2019-21),
# 0.18 (2020-22). Read naively that says green capability is decoupling from
# income -- which would be direct evidence on whether the catch-up East is
# closing the green-capability gap, and a possible second result alongside the
# typology.
#
# BUT IT MIGHT BE AN ARTIFACT, and the point of this script is to find out.
# Each window in that table re-pools complexity from the Atlas AND re-measures
# patents AND re-averages GDP, so at least four things move at once:
#   (a) genuine convergence in green capability;
#   (b) the GLOBAL comparison set shifting -- GCI/GCP are z-scored against all
#       countries each window, so the EU can "converge" because the rest of the
#       world moved;
#   (c) the patent measure differing between windows (grants vs applications);
#   (d) ordinary sampling noise across heavily OVERLAPPING windows, which are
#       nowhere near independent observations.
#
# WHAT THIS DOES
#   * holds the patent measure fixed (applications) across every window, killing (c)
#   * runs a rolling series of 5-year windows so the trajectory is visible rather
#     than inferred from two endpoints
#   * decomposes R2 into its parts -- GCI, GCP, the complexity twin, and patents --
#     so we can say WHICH component decouples
#   * reports the EU's position against the global distribution, to separate (a)
#     from (b)
#   * compares the two NON-OVERLAPPING endpoint windows, since the rolling series
#     is autocorrelated by construction and its apparent trend is not evidence of
#     6 independent observations
#
# NOTE the potential axis uses only Atlas complexity and green patents -- no
# EXIOBASE -- so it is NOT subject to the 2022 emissions cap. The binding limit
# here is the patent series (applications to 2021).
#
# Writes data/tidy/capability_convergence.csv and
# plots/capability_convergence.{png,pdf}.

here::i_am("R/appendix_capability_convergence.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(tidyr); library(ggplot2)
  library(countrycode); library(knitr); library(Matrix); library(ggrepel)
  library(magrittr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/complexity.R"))
source(here("R/functions/indicators.R"))
source(here("R/functions/typology.R"))

LEN   <- 5L                       # window length
ENDS  <- 2016:2021                # last year of each rolling window
eby   <- as.data.table(readRDS(here("data/raw/exports_by_year_1224.rds")))
green <- fread(here("data/tidy/green_products_hs6.csv"),
               colClasses = list(character = "hs6"))
base_data  <- as_tibble(fread(here("data/tidy/full_taxonomy_data.csv")))
extra_data <- as_tibble(fread(here("data/tidy/new_data.csv")))
eu_iso3 <- countrycode(base_countries, "country.name", "iso3c")

# Patent APPLICATIONS, held fixed across all windows (kills threat (c)).
v2 <- here("data/tidy/green_patents_panel.csv")
stopifnot("run R/get_data_patents_patstat.R first" = file.exists(v2))
pat <- fread(v2)[, .(iso3, year, apps = applications)]
pop <- as.data.table(base_data)[, .(iso3 = country, year, population = population * 1000)]

one_window <- function(y2) {
  y1 <- y2 - LEN + 1L
  rca <- build_rca_matrix(eby[year %between% c(y1, y2),
                              .(export = sum(export)), by = .(iso3, hs6)])
  ci  <- complexity_indices(rca$M)
  gi  <- as.data.table(green_indicators(rca$M, ci$PCI, green$hs6))

  ind <- as.data.table(build_indicator_table(base_data, extra_data, y1, y2))
  ind[, iso3 := countrycode(country, "country.name", "iso3c")]
  ind <- ind[iso3 %in% eu_iso3]

  p <- pat[year %between% c(y1, y2), .(n = sum(apps, na.rm = TRUE)), by = iso3]
  pp <- pop[year %between% c(y1, y2), .(population = mean(population)), by = iso3]
  p <- merge(p, pp, by = "iso3")[, .(iso3, patents_pm = n / (population / 1e6))]

  d <- merge(ind[, .(iso3, country, GDP_normed)], gi[, .(iso3, GCI, GCP)], by = "iso3")
  d <- merge(d, p, by = "iso3")
  if (nrow(d) != 27L) stop("window ", y1, "-", y2, ": ", nrow(d), " countries, expected 27")

  d[, lg := log(GDP_normed)]
  twin <- axis_score(as.data.frame(d), c("GCI", "GCP"), "GCI", "patents_pm")
  d[, `:=`(potential = twin$score, complexity = twin$twin, innovation = twin$solo)]

  r2 <- function(y) summary(lm(y ~ d$lg))$r.squared
  data.table(
    window = sprintf("%d-%d", y1, y2), y1 = y1, y2 = y2,
    r2_potential  = r2(d$potential),
    r2_complexity = r2(d$complexity),
    r2_innovation = r2(d$innovation),
    r2_GCI        = r2(d$GCI),
    r2_GCP        = r2(d$GCP),
    r2_patents    = r2(log(d$patents_pm)),
    # EU position against the GLOBAL distribution -- separates real EU movement
    # from the rest of the world shifting under the z-scoring.
    # The innovation standalone enters the axis UNTRANSFORMED (axis_score
    # z-scores the raw variable), and patents per capita are heavily
    # right-skewed. If skew changes across windows, R2 on the raw variable moves
    # for distributional reasons rather than because capability decoupled from
    # income -- so track skew, and compare raw-scale against log-scale R2.
    skew_patents  = { z <- d$patents_pm; mean((z - mean(z))^3) / sd(z)^3 },
    ratio_top_med = max(d$patents_pm) / median(d$patents_pm),
    eu_mean_GCI   = mean(d$GCI),
    eu_sd_GCI     = sd(d$GCI),
    world_sd_GCI  = sd(gi$GCI),
    scores        = list(d[, .(iso3, country, potential, complexity, innovation,
                               GCI, GCP, patents_pm, lg)]))
}

cat("Rebuilding complexity for", length(ENDS), "rolling windows ...\n")
res <- rbindlist(lapply(ENDS, function(e) { message("  ", e - LEN + 1L, "-", e); one_window(e) }))

cat("\n## R2 on log GDP p.c., by window (patent measure held fixed = applications)\n\n")
print(kable(res[, .(window,
                    potential = round(r2_potential, 2),
                    complexity_twin = round(r2_complexity, 2),
                    innovation = round(r2_innovation, 2),
                    GCI = round(r2_GCI, 2), GCP = round(r2_GCP, 2),
                    patents = round(r2_patents, 2))], format = "pipe"))

cat("\n\n## Which component decouples?\n\n")
first <- res[1]; last <- res[.N]
for (v in c("r2_potential", "r2_complexity", "r2_innovation", "r2_GCI",
            "r2_GCP", "r2_patents")) {
  cat(sprintf("  %-15s %.2f -> %.2f  (%+.2f)\n", sub("^r2_", "", v),
              first[[v]], last[[v]], last[[v]] - first[[v]]))
}

cat("\n## Is the innovation fall real, or a change in the patent DISTRIBUTION?\n\n")
print(kable(res[, .(window,
                    r2_raw_scale = round(r2_innovation, 2),
                    r2_log_scale = round(r2_patents, 2),
                    skew = round(skew_patents, 2),
                    top_over_median = round(ratio_top_med, 1))], format = "pipe"))
cat("\n  The axis uses the RAW variable. If r2_raw_scale falls much faster than\n",
    "  r2_log_scale while skew rises, the apparent decoupling is largely a\n",
    "  distributional artifact of a few high-patenting countries pulling away,\n",
    "  not poorer members catching up.\n", sep = "")

cat("\n## Is the EU moving, or the world moving under it?\n\n")
print(kable(res[, .(window, eu_mean_GCI = round(eu_mean_GCI, 2),
                    eu_sd_GCI = round(eu_sd_GCI, 2),
                    world_sd_GCI = round(world_sd_GCI, 2))], format = "pipe"))
cat("\n  GCI is z-scored against the GLOBAL set each window. If eu_mean_GCI is flat\n",
    "  while R2 falls, the decoupling is WITHIN the EU (income no longer predicts\n",
    "  which member state is capable), not the EU as a whole catching up or\n",
    "  falling back.\n", sep = "")

# --- The honest caveat: overlapping windows are not independent --------------
cat("\n## Non-overlapping endpoints (the only clean comparison)\n\n")
a <- res[y2 == min(ENDS)]; b <- res[y2 == max(ENDS)]
cat(sprintf("  %s vs %s -- these share no years.\n", a$window, b$window))
cat(sprintf("  R2(potential ~ logGDP): %.2f -> %.2f\n", a$r2_potential, b$r2_potential))
sa <- a$scores[[1]]; sb <- b$scores[[1]]
m <- merge(sa[, .(iso3, country, pot_a = potential, gci_a = GCI, lg_a = lg)],
           sb[, .(iso3, pot_b = potential, gci_b = GCI)], by = "iso3")
m[, group := as.character(get_country_classification(iso3, "jee"))]
m[, `:=`(d_pot = pot_b - pot_a, d_gci = gci_b - gci_a)]

cat("\n  Change in green capability by growth model (z-units, global scaling):\n\n")
print(kable(m[, .(n = .N, d_potential = round(mean(d_pot), 2),
                  d_GCI = round(mean(d_gci), 2)), by = group][order(-d_GCI)],
            format = "pipe"))
cat("\n  cor(baseline log GDP p.c., change in potential) = ",
    sprintf("%+.2f", cor(m$lg_a, m$d_pot)),
    "\n  (negative = poorer countries gained more, i.e. genuine convergence)\n", sep = "")

cat("\n  Biggest movers:\n\n")
print(kable(rbind(head(m[order(-d_pot), .(country, group, d_potential = round(d_pot, 2))], 5),
                  head(m[order(d_pot),  .(country, group, d_potential = round(d_pot, 2))], 5)),
            format = "pipe"))

out <- res[, .(window, r2_potential, r2_complexity, r2_innovation,
               r2_GCI, r2_GCP, r2_patents, skew_patents, ratio_top_med,
               eu_mean_GCI, eu_sd_GCI, world_sd_GCI)]
fwrite(out, here("data/tidy/capability_convergence.csv"))

# --- Figure -------------------------------------------------------------------
long <- melt(res[, .(window, Potential = r2_potential,
                     `Complexity (GCI+GCP)` = r2_complexity,
                     `Innovation (patents)` = r2_innovation)],
             id.vars = "window", variable.name = "component", value.name = "r2")
p <- ggplot(long, aes(window, r2, colour = component, group = component)) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(NULL, values = c(Potential = "#0B3C5D",
                                       `Complexity (GCI+GCP)` = "#2E7CA8",
                                       `Innovation (patents)` = "#E65032")) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = NULL, y = expression(R^2~"on log GDP per capita"),
       title = "Is green capability decoupling from income?",
       subtitle = paste0("Rolling ", LEN, "-year windows, patent measure held fixed.",
                         "\nWindows overlap heavily, so treat the series as a trajectory,",
                         " not as independent observations.")) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15))

ggsave(here("plots/capability_convergence.png"), p, width = 7.0, height = 4.8, dpi = 300)
ggsave(here("plots/capability_convergence.pdf"), p, width = 7.0, height = 4.8)

message("\nappendix_capability_convergence.R done.")
