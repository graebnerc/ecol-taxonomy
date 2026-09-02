# Appendix - does the EPO-only restriction bias the potential axis?
#
# READY TO RUN before the data exists. Run
# sql/get_green_patents_v3_all_offices.sql against a current PATSTAT edition,
# save as data/raw/get_green_patents_v3.csv, then:
#
#     Rscript R/appendix_patent_offices.R
#
# With the file absent it prints instructions and exits 0 without changing
# anything.
#
# THE CONCERN. The headline patent variable counts EPO filings only
# (`appln_auth = 'EP'`). Applicants in smaller and eastern member states are
# likelier to file domestically, so an EPO-only count plausibly understates
# exactly the low-potential tail the polarization finding rests on. Green patents
# are the most discriminating single variable in the potential axis, and the
# v1 -> v2 correction already showed how sensitive periphery counts are to query
# details (Slovakia 2.40x, Greece 2.34x vs Netherlands 1.49x).
#
# WHAT THIS IS NOT. All-offices counts are not "better". A domestic filing and an
# EPO filing are different objects; EPO filings clear a higher bar and confer
# European-wide protection, which is the concept the axis wants. This is a
# robustness check on whether the RANKING is an artifact of the office filter,
# not a candidate replacement for the headline.
#
# Writes data/tidy/patent_offices.csv and plots/patent_offices.{png,pdf}.

here::i_am("R/appendix_patent_offices.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(ggplot2)
  library(countrycode); library(knitr); library(ggrepel)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/typology.R"))

# Accept either the documented path or the query's own filename dropped into
# data/raw/ -- the latter is what you get by saving the SQL result directly, and
# is what happened with the v2 extract.
CANDIDATES <- c(here("data/tidy/patstat_green-patents_allauth.csv"),
                here("data/raw/get_green_patents_v3.csv"),
                here("data/raw/patstat_green-patents_allauth.csv"))
RAW <- CANDIDATES[file.exists(CANDIDATES)][1]
if (is.na(RAW)) {
  message(
    "\n", strrep("-", 74), "\n",
    "All-offices PATSTAT extract not found. Looked for:\n",
    paste0("  ", CANDIDATES, collapse = "\n"), "\n\n",
    "To produce it:\n",
    "  1. run sql/get_green_patents_v3_all_offices.sql against PATSTAT\n",
    "  2. save the result as data/raw/get_green_patents_v3.csv\n",
    "  3. re-run: Rscript R/appendix_patent_offices.R\n\n",
    "Nothing was changed. The headline keeps the EPO-only measure, which is the\n",
    "intended concept -- this script only tests whether the ranking depends on it.\n",
    strrep("-", 74))
  quit(save = "no", status = 0)
}

message("Reading ", RAW)
pat <- fread(RAW)
need <- c("year", "country", "n_applications_all", "n_applications_ep")
missing <- setdiff(need, names(pat))
if (length(missing))
  stop("extract is missing required column(s): ", paste(missing, collapse = ", "),
       "\nGot: ", paste(names(pat), collapse = ", "),
       "\nDid sql/get_green_patents_v3_all_offices.sql run unmodified?")

stopifnot(
  "duplicate year x country rows" = !anyDuplicated(pat[, .(year, country)]),
  "EPO count exceeds all-offices count somewhere" =
    all(pat$n_applications_ep <= pat$n_applications_all))

# THE CORRECTNESS CHECK. n_applications_ep must reproduce the v2 series exactly:
# same database, same CPC filter, same applicant rule, same DISTINCT. If it does
# not, this query differs from v2 in some way and the comparison is void -- fail
# rather than quietly compare two things that are not comparable.
V2 <- here("data/tidy/green_patents_panel.csv")
if (file.exists(V2)) {
  v2 <- fread(V2)[, .(country = iso2, year, ep_v2 = applications)]
  j <- merge(pat[, .(country, year, ep_v3 = n_applications_ep)], v2,
             by = c("country", "year"))
  if (!nrow(j)) {
    warning("no overlap with the v2 extract - cannot verify the EPO columns",
            call. = FALSE)
  } else {
    bad <- j[ep_v3 != ep_v2]
    cat(sprintf("\nEPO cross-check against v2: %d overlapping country-years, %d mismatched.\n",
                nrow(j), nrow(bad)))
    if (nrow(bad)) {
      print(head(bad[order(-abs(ep_v3 - ep_v2))], 10))
      stop("n_applications_ep does not reproduce the v2 series. The two queries ",
           "differ (country list? year range? applicant rule? CPC match?), so the ",
           "EPO-vs-all-offices comparison would not be like-for-like.")
    }
    cat("  EPO columns reproduce v2 exactly - the comparison is like-for-like.\n")
  }
} else {
  warning("v2 panel absent - the EPO columns could not be verified against it.",
          call. = FALSE)
}

# The query may be run in year batches if it is slow; report what arrived rather
# than assuming the full range.
cat(sprintf("Coverage: %d-%d, %d countries.\n",
            min(pat$year), max(pat$year), uniqueN(pat$country)))

ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
ind$iso3 <- countrycode(ind$country, "country.name", "iso3c")
pop <- fread(here("data/tidy/full_taxonomy_data.csv"))[
  year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR),
  .(population = mean(population * 1000)), by = .(iso3 = country)]

pat[, iso3 := countrycode(country, "iso2c", "iso3c", warn = FALSE)]
w <- pat[iso3 %in% ind$iso3 & year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR),
         .(all = sum(n_applications_all), ep = sum(n_applications_ep)), by = iso3]
w <- merge(w, pop, by = "iso3")
w[, `:=`(all_pm = all / (population / 1e6), ep_pm = ep / (population / 1e6))]
w[, ep_share := round(100 * ep / all, 1)]

cat("\n## EPO share of all green filings, by country (reference window)\n\n")
w2 <- merge(w, data.table(iso3 = ind$iso3, country = ind$country), by = "iso3")
w2[, group := as.character(get_country_classification(iso3, "jee"))]
print(kable(w2[order(ep_share), .(country, group, all, ep, ep_share)], format = "pipe"))

cat("\n## Is the EPO share systematically lower in the periphery?\n\n")
print(kable(w2[, .(n = .N, mean_ep_share = round(mean(ep_share), 1)), by = group][
  order(mean_ep_share)], format = "pipe"))

# --- Re-score the map on all-offices counts ----------------------------------
d <- as.data.table(ind)
d <- merge(d, w2[, .(iso3, all_pm, ep_pm)], by = "iso3")
score <- function(dd, innov) {
  dd <- as.data.frame(dd)          # axis_score indexes df[, vars]; data.table differs
  v <- axis_score(dd, INTENSITY_VARS, "CarbonIntensity_normed", FOSSIL_VAR)$score
  p <- axis_score(dd, COMPLEXITY_VARS, "GCI", innov)$score
  list(v = v, p = p, q = assign_quadrant(v, p, "short"))
}
base <- score(d, "GreenPatents_normed")
alt  <- score(d, "all_pm")

cat("\n## Effect on the typology\n\n")
cat(sprintf("  Spearman(patents p.c., EPO vs all offices) = %.3f\n",
            cor(d$ep_pm, d$all_pm, method = "spearman")))
cat(sprintf("  Spearman(potential axis)                   = %.3f\n",
            cor(base$p, alt$p, method = "spearman")))
cat(sprintf("  QUADRANT CHANGES                           = %d / 27\n",
            sum(base$q != alt$q)))
moved <- d$country[base$q != alt$q]
if (length(moved)) {
  cat("\n  moved:\n")
  print(kable(data.table(country = moved, EPO_only = base$q[base$q != alt$q],
                         all_offices = alt$q[base$q != alt$q]), format = "pipe"))
  cat("\n  NOTE: if countries move, the divergence IS the finding -- discuss it,\n",
      "  do not resolve it by adopting whichever series is friendlier.\n", sep = "")
}

# --- Optional: which offices account for the non-EPO filings? ----------------
BYOFF <- here("data/raw/get_green_patents_v3_by_office.csv")
if (file.exists(BYOFF)) {
  bo <- fread(BYOFF)
  bo[, iso3 := countrycode(country, "iso2c", "iso3c", warn = FALSE)]
  bo <- merge(bo, w2[, .(iso3, cname = country, group)], by = "iso3")
  bo[, share := 100 * n_applications / sum(n_applications), by = iso3]
  cat("\n## Top filing offices per country (non-EPO only, reference window)\n\n")
  print(kable(bo[office != "EP"][order(iso3, -share)][, head(.SD, 2), by = iso3][
    , .(country = cname, group, office, n_applications, share = round(share, 1))],
    format = "pipe"))
} else {
  message("\n(Optional by-office breakdown absent; see the second query in ",
          "sql/get_green_patents_v3_all_offices.sql if the ranking diverges.)")
}

out <- w2[, .(country, group, applications_all = all, applications_epo = ep,
              ep_share_pct = ep_share,
              per_million_all = round(all_pm, 1), per_million_epo = round(ep_pm, 1))]
fwrite(out, here("data/tidy/patent_offices.csv"))

p <- ggplot(w2, aes(ep_pm, all_pm, colour = group)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey70") +
  geom_point(size = 2.2) +
  ggrepel::geom_text_repel(aes(label = country), size = 2.6, max.overlaps = 16,
                           segment.colour = "grey75", segment.size = 0.2) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_manual(NULL, values = c(Core = "#0B3C5D", Finance = "#2E7CA8",
                                       Periphery = "#E65032", Workbench = "#F09372")) +
  labs(x = "Green filings per million, EPO only (log)",
       y = "Green filings per million, all offices (log)",
       title = "Does counting only EPO filings understate the periphery?",
       subtitle = paste("Points above the diagonal file more outside the EPO.",
                        "Reference window; log scales.")) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8))

ggsave(here("plots/patent_offices.png"), p, width = 7.0, height = 5.4, dpi = 300)
ggsave(here("plots/patent_offices.pdf"), p, width = 7.0, height = 5.4)

message("\nappendix_patent_offices.R done.")
