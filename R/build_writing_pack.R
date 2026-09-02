# Build writing/ -- the self-contained evidence pack for drafting the paper.
#
# WHY THIS EXISTS. The paper is drafted by an assistant that can see ONLY the
# writing/ folder: no R, no data/tidy, no ability to re-run anything or check a
# number. So everything the draft needs must physically be inside writing/, and
# it must be regenerable rather than hand-copied -- otherwise it silently drifts
# out of date the next time the pipeline is re-run.
#
# This script regenerates the MACHINE parts of the pack:
#   writing/evidence/*.csv   exact numbers, copied from data/tidy
#   writing/evidence/numbers.md   every headline figure, computed here, with the
#                                 commit and date it came from
#   writing/figures/*.png    the figures the paper will use
#
# It does NOT touch the hand-written narrative (results-summary.md,
# open-questions.md, README.md) -- those are prose and are edited by hand.
#
# Run after any pipeline change:  Rscript R/build_writing_pack.R

here::i_am("R/build_writing_pack.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(countrycode); library(magrittr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))

W   <- here("writing")
EV  <- file.path(W, "evidence")
FIG <- file.path(W, "figures")
for (d in c(W, EV, FIG, file.path(W, "output"))) dir.create(d, showWarnings = FALSE, recursive = TRUE)

stamp <- sprintf("Generated %s from commit %s by R/build_writing_pack.R.",
                 format(Sys.Date()),
                 tryCatch(substr(system("git rev-parse HEAD", intern = TRUE), 1, 7),
                          error = function(e) "unknown"))

# --- 1. Evidence tables -------------------------------------------------------
copy_tables <- c(
  "taxonomy_scores.csv"        = "scores_by_country.csv",
  "taxonomy_indicators.csv"    = "indicators_by_country.csv",
  "quadrant_classification.csv"= "quadrant_classification.csv",
  "quadrant_profiles.csv"      = "quadrant_profiles.csv",
  "robustness_specs.csv"       = "robustness_specs.csv",
  "window_options.csv"         = "window_options.csv",
  "validation_external.csv"    = "validation_external.csv",
  "validation_group_means.csv" = "validation_group_means.csv",
  "burden_responsibility.csv"  = "burden_responsibility.csv",
  "offshoring_origins.csv"     = "offshoring_origins.csv",
  "growth_model_gradient.csv"  = "growth_model_gradient.csv",
  "patent_options.csv"         = "patent_options.csv",
  "country_profiles.csv"       = "country_profiles.csv",
  "energy_by_sector.csv"       = "energy_by_sector.csv",
  "eora_crosscheck.csv"        = "eora_crosscheck.csv",
  "patent_offices.csv"         = "patent_offices.csv",
  "capability_convergence.csv" = "capability_convergence.csv",
  "validation_group_tests.csv" = "validation_group_tests.csv")
copied <- character(0)
for (src in names(copy_tables)) {
  from <- here("data/tidy", src)
  if (file.exists(from)) {
    file.copy(from, file.path(EV, copy_tables[[src]]), overwrite = TRUE)
    copied <- c(copied, copy_tables[[src]])
  } else if (!src %in% c("patent_offices.csv")) {
    warning("missing evidence table: ", src, call. = FALSE)
  }
}

# --- 2. Figures ---------------------------------------------------------------
copy_figs <- c("typology_map.png", "quadrant_profiles.png",
               "validation_scores_by_group.png", "validation_alluvial.png",
               "burden_responsibility.png", "offshoring_origins.png",
               "window_options.png", "patent_options.png",
               "appendix_structure_map.png", "descriptives_correlations.png",
               "country_profiles.png", "eora_crosscheck.png", "patent_offices.png",
               "capability_convergence.png")
figs <- character(0)
for (f in copy_figs) {
  from <- here("plots", f)
  if (file.exists(from)) { file.copy(from, file.path(FIG, f), overwrite = TRUE)
                           figs <- c(figs, f) }
}

# --- 3. numbers.md ------------------------------------------------------------
s <- fread(here("data/tidy/taxonomy_scores.csv"))
i <- fread(here("data/tidy/taxonomy_indicators.csv"))
lg <- log(i$GDP_normed)
r2 <- function(y) summary(lm(y ~ lg))$r.squared
eta2 <- function(y) summary(lm(y ~ factor(s$group)))$r.squared

dat <- copy(s)
dat[, iso3 := countrycode(country, "country.name", "iso3c")]
val <- fread(here("data/tidy/validation_external.csv"))
rob <- fread(here("data/tidy/robustness_specs.csv"))
win <- if (file.exists(here("data/tidy/window_options.csv")))
  fread(here("data/tidy/window_options.csv")) else NULL

md <- c(
  "# Key numbers", "",
  paste0("*", stamp, "*"), "",
  "Every figure below is computed from the committed pipeline output. **Use these",
  "values verbatim; do not recompute, round differently, or estimate.** If a number",
  "the draft needs is not here, say so rather than inventing it.", "",
  "## Specification", "",
  sprintf("- Reference window: **%d-%d**", REF_FIRST_YEAR, REF_LAST_YEAR),
  sprintf("- Green-patent measure: **%s** (EPO, PATSTAT)", PATENT_MEASURE),
  "- Emissions/value added: **EXIOBASE 3.10.2** (Zenodo record 20051562)",
  "- Countries: EU-27", "",
  "## Headline diagnostics", "",
  "| quantity | value |", "|---|---:|",
  sprintf("| cor(vulnerability, potential) | %.2f |", cor(s$vulnerability, s$potential)),
  sprintf("| R2 vulnerability ~ log GDP p.c. | %.2f |", r2(s$vulnerability)),
  sprintf("| R2 potential ~ log GDP p.c. | %.2f |", r2(s$potential)),
  sprintf("| eta2 vulnerability ~ growth model | %.2f |", eta2(s$vulnerability)),
  sprintf("| eta2 potential ~ growth model | %.2f |", eta2(s$potential)), "",
  "## Within-block correlations (the two-part axis design)", "",
  "| pair | r |", "|---|---:|",
  sprintf("| carbon intensity ~ energy intensity (twin) | %.2f |",
          cor(i$CarbonIntensity_normed, i$EnergyIntensity_normed)),
  sprintf("| GCI ~ GCP (twin) | %.2f |", cor(i$GCI, i$GCP)),
  sprintf("| intensity sub-index ~ fossil standalone | %.2f |", cor(s$intensity, s$fossil)),
  sprintf("| complexity sub-index ~ innovation standalone | %.2f |",
          cor(s$complexity, s$innovation)), "",
  "## Quadrant membership", "")

qt <- s[, .(n = .N, countries = paste(sort(country), collapse = ", ")), by = quadrant][order(-n)]
md <- c(md, "| quadrant | n | countries |", "|---|---:|---|",
        sprintf("| %s | %d | %s |", qt$quadrant, qt$n, qt$countries), "",
        sprintf("Borderline (quadrant is convention-sensitive): **%s**",
                paste(s[boundary == TRUE]$country, collapse = ", ")), "",
        "## Quadrant x growth model", "")
tb <- table(s$quadrant, s$group)
md <- c(md, paste0("| quadrant | ", paste(colnames(tb), collapse = " | "), " |"),
        paste0("|---|", paste(rep("---:", ncol(tb)), collapse = "|"), "|"),
        apply(cbind(rownames(tb), tb), 1, function(r)
          paste0("| ", paste(r, collapse = " | "), " |")), "",
        "## External validity (partial = net of log GDP p.c.)", "",
        "| score | outcome | raw r | partial r |", "|---|---|---:|---:|",
        sprintf("| %s | %s | %.2f | %.2f |", val$score, val$outcome,
                val$raw_cor, val$partial_cor), "",
        "## Robustness: specification sensitivity", "",
        "Spearman rank correlation vs the headline; quadrant changes out of 27.", "",
        "| spec | cor vuln | cor pot | quadrant changes |", "|---|---:|---:|---:|",
        sprintf("| %s | %.2f | %.2f | %d |", rob$spec, rob$cor_vuln,
                rob$cor_pot, rob$quad_changes), "")

if (!is.null(win))
  md <- c(md, "## Robustness: reference window (whole typology rebuilt on each)", "",
          "| window | cor vuln | cor pot | quad changes | R2(v~GDP) | R2(p~GDP) | note |",
          "|---|---:|---:|---:|---:|---:|---|",
          sprintf("| %s | %.2f | %.2f | %d | %.2f | %.2f | %s |", win$window,
                  win$cor_vuln, win$cor_pot, win$quad_changes,
                  win$r2_vuln_gdp, win$r2_pot_gdp, win$note), "")

md <- c(md, "## Files in this pack", "",
        "**Tables** (`evidence/`): ", paste0("`", paste(copied, collapse = "`, `"), "`"), "",
        "**Figures** (`figures/`): ", paste0("`", paste(figs, collapse = "`, `"), "`"), "")

writeLines(md, file.path(EV, "numbers.md"))

cat(sprintf("\nwriting/ pack rebuilt: %d tables, %d figures.\n%s\n",
            length(copied), length(figs), stamp))
