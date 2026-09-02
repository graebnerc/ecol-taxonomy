# Central configuration for the WP1 country taxonomy pipeline.
# Sourced by the numbered pipeline scripts (01_, 02_, 03_).

# Reference window ------------------------------------------------------------
# The taxonomy uses country-level averages over a fixed window to smooth out
# year-to-year noise while staying close to the "current" structure of the
# economy. 2014-2018 is the most recent window in which all core sources
# (EXIOBASE footprints, Eurostat energy balances, PATSTAT green patents,
# WDI/Eurostat GDP) overlap with good coverage. Sensitivity to this choice is
# tested in Phase 6.
# CHANGED 2026-09-02 from 2014-2018 to 2017-2021. EXIOBASE 3.10.2 lifted the old
# cap (value added stopped at 2019); the binding constraint is now the green
# patent variable, which is why PATENT_MEASURE below matters. 2017-2021 is the
# most recent window in which every input is observed and complete: EXIOBASE
# emissions are complete only to 2022 (2023-2024 are nowcasts with a broken
# dominant stressor) and patent APPLICATIONS only to 2021. Evidence and the cost
# of the move (2/27 countries, both already borderline) in
# info/Report_2026-09-01_data-rebuild.md and R/appendix_window_options.R.
REF_FIRST_YEAR <- 2017
REF_LAST_YEAR  <- 2021

# Green-patent measure for the headline innovation standalone.
#   "applications" - EPO filings, complete ~18 months after filing. HEADLINE.
#   "grants"       - EPO grants by filing year. Grant lag is 3-5+ years, so the
#                    recent cohorts are severely truncated; kept as a robustness
#                    spec in 07_robustness.R.
# The two rank EU-27 countries at Spearman 0.99 and swapping them moves 0/27
# countries, which is what licenses using applications to reach a recent window
# (R/appendix_patent_options.R).
PATENT_MEASURE <- "applications"

# Typology blocks (Fig. 1) ----------------------------------------------------
# Each of the two blocks is built from TWO conceptual dimensions, combined with
# equal weight (see R/functions/typology.R::axis_score and info/PaperTodos.md):
#
#   Vulnerability = emission INTENSITY of production   (twin sub-index)
#                 + FOSSIL dependency of energy        (standalone)
#   Potential     = green economic COMPLEXITY          (twin sub-index)
#                 + green INNOVATION capability         (standalone)
#
# Each twin sub-index is two correlated indicators of one latent construct
# (intensity r=0.68, complexity r=0.78) reduced to PC1 - not double-counting.
# Note the income structure: within each block one part is income-linked and one
# is income-neutral, on opposite diagonals (intensity & patents track GDP p.c.;
# fossil share & complexity do not), so income is present but balanced.
INTENSITY_VARS  <- c("CarbonIntensity_normed", "EnergyIntensity_normed")  # emission intensity of production
FOSSIL_VAR      <- "ShareFossils_normed"                                  # fossil dependency (demand-side)
COMPLEXITY_VARS <- c("GCI", "GCP")                                        # green complexity (current + connectedness)
INNOV_VAR       <- "GreenPatents_normed"                                  # green innovation capability

# Flat variable lists (all three per block, single PCA, no sub-index structure).
# Retained for the specification-sensitivity checks in 07_robustness.R.
VULN_VARS <- c(INTENSITY_VARS, FOSSIL_VAR)
POT_VARS  <- c(INNOV_VAR, COMPLEXITY_VARS)

# Paths -----------------------------------------------------------------------
TIDY_DIR <- here::here("data", "tidy")
PLOT_DIR <- here::here("plots")
