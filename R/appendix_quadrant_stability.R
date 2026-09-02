# Appendix - how much weight can the quadrant labels bear?
#
# THE QUESTION. Quadrants come from MEDIAN SPLITS of two continuous axes, cells
# hold 11/11/3/2 countries, and four countries sit within 0.10 z of a median. So
# before the paper treats the four cells as types, it should say how often a
# country would actually land in its assigned cell. This is a presentational
# decision with substantive consequences: four types, or a continuous map with
# quadrants as exposition?
#
# TWO SOURCES OF INSTABILITY, tested separately because they are different
# questions and a reader will want them separated.
#
#   1. WHICH YEARS. Every indicator is a mean over the reference window, so the
#      assignment depends on which years happen to be in it. Resample the window's
#      years WITH REPLACEMENT, re-average, re-score, re-split. This uses real
#      year-to-year variation rather than invented noise.
#
#   2. WHICH COUNTRIES. The medians are computed on the sample itself, so dropping
#      a country moves the thresholds for everyone else. Leave-one-out over all 27
#      (07_robustness.R does this for Luxembourg and Malta only).
#
# WHAT IS HELD FIXED: GCI and GCP are pooled over the whole window from the Atlas,
# not computed per year, so they cannot be resampled here without re-pooling every
# replicate (~40 s each). They are held at their headline values. 07_robustness.R
# shows per-year GCI correlates 0.97-1.00 with the pooled version, so this
# understates instability only slightly -- but it does understate it, and that
# should be stated rather than glossed.
#
# Writes data/tidy/quadrant_stability.csv and
# plots/quadrant_stability.{png,pdf}.

here::i_am("R/appendix_quadrant_stability.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(ggplot2)
  library(countrycode); library(knitr); library(magrittr)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/typology.R"))

B <- 2000L
set.seed(20260902)

ind <- as.data.table(fread(here("data/tidy/taxonomy_indicators.csv")))
ind[, iso3 := countrycode(country, "country.name", "iso3c")]
scr <- fread(here("data/tidy/taxonomy_scores.csv"))
panel <- as.data.table(fread(here("data/tidy/full_taxonomy_data.csv")))
extra <- as.data.table(fread(here("data/tidy/new_data.csv")))

Q_LEVELS <- c("Winners", "Exposed", "Low-stakes", "At risk")
short_q <- function(x) sub(" \\(.*$", "", sub("Exposed but capable", "Exposed",
             sub("Low-stakes / low capability", "Low-stakes", x)))
headline <- scr[, .(country, quadrant = short_q(quadrant))]

# --- Per-year values of every year-varying indicator input -------------------
yr <- merge(panel[year %between% c(REF_FIRST_YEAR, REF_LAST_YEAR)],
            extra[, .(country = iso3c, year, ShareFossils_GrossAvEn)],
            by = c("country", "year"))
yr <- yr[, .(iso3 = country, year,
             ci   = GWP_pba / ValueAdded_pba,
             ei   = FinalEnergyConsumption / ValueAdded_pba,
             fos  = ShareFossils_GrossAvEn / 100,
             pat  = GreenPatentsApps_n / (population * 1000 / 1e6))]
yr <- yr[iso3 %in% ind$iso3]
stopifnot("per-year table is incomplete" =
            nrow(yr) == 27L * (REF_LAST_YEAR - REF_FIRST_YEAR + 1L),
          "NA in the per-year inputs" = !anyNA(yr))

fixed <- ind[, .(iso3, country, GCI, GCP)]

score_from <- function(d) {
  d <- as.data.frame(d)
  v <- axis_score(d, c("ci", "ei"), "ci", "fos")$score
  p <- axis_score(d, c("GCI", "GCP"), "GCI", "pat")$score
  assign_quadrant(v, p, "short")
}

# Sanity: rebuilding from the per-year table must reproduce the headline map.
base_d <- merge(yr[, lapply(.SD, mean), by = iso3, .SDcols = c("ci","ei","fos","pat")],
                fixed, by = "iso3")
base_q <- score_from(base_d)
chk <- merge(data.table(country = base_d$country, rebuilt = base_q), headline, by = "country")
if (any(chk$rebuilt != chk$quadrant))
  stop("rebuilding from per-year inputs does not reproduce the headline map for: ",
       paste(chk[rebuilt != quadrant]$country, collapse = ", "))
cat("Rebuild check: per-year inputs reproduce the headline map exactly.\n")

# --- 1. Year bootstrap --------------------------------------------------------
yrs <- REF_FIRST_YEAR:REF_LAST_YEAR

# Resampling years WITH REPLACEMENT means a year can appear more than once, so a
# join would fan out. The mean over a multiset of years is just a weighted mean
# with weights = how often each year was drawn, so precompute one country x year
# matrix per indicator and take a matrix-vector product per replicate. Fast, and
# no join.
as_mat <- function(v) {
  m <- dcast(yr, iso3 ~ year, value.var = v)
  rn <- m$iso3; m <- as.matrix(m[, -1]); rownames(m) <- rn
  m[base_d$iso3, as.character(yrs), drop = FALSE]
}
MATS <- lapply(c(ci = "ci", ei = "ei", fos = "fos", pat = "pat"), as_mat)
fixed_al <- fixed[match(base_d$iso3, fixed$iso3)]

boot <- matrix(NA_character_, nrow = B, ncol = nrow(base_d),
               dimnames = list(NULL, base_d$country))
for (b in seq_len(B)) {
  w <- tabulate(match(sample(yrs, length(yrs), replace = TRUE), yrs),
                nbins = length(yrs))
  w <- w / sum(w)
  d <- data.frame(iso3 = base_d$iso3,
                  ci  = as.numeric(MATS$ci  %*% w),
                  ei  = as.numeric(MATS$ei  %*% w),
                  fos = as.numeric(MATS$fos %*% w),
                  pat = as.numeric(MATS$pat %*% w),
                  GCI = fixed_al$GCI, GCP = fixed_al$GCP)
  boot[b, ] <- score_from(d)
}

# The equal-weight replicate must reproduce the headline map, or the matrix
# construction is misaligned.
w1 <- rep(1 / length(yrs), length(yrs))
chk2 <- score_from(data.frame(iso3 = base_d$iso3,
                              ci = as.numeric(MATS$ci %*% w1),
                              ei = as.numeric(MATS$ei %*% w1),
                              fos = as.numeric(MATS$fos %*% w1),
                              pat = as.numeric(MATS$pat %*% w1),
                              GCI = fixed_al$GCI, GCP = fixed_al$GCP))
stopifnot("bootstrap matrices are misaligned with the headline" =
            all(chk2 == base_q))

stab <- rbindlist(lapply(colnames(boot), function(cn) {
  tb <- table(factor(boot[, cn], levels = Q_LEVELS))
  hq <- headline[country == cn]$quadrant
  data.table(country = cn, headline_quadrant = hq,
             pct_in_headline = round(100 * tb[[hq]] / B, 1),
             modal_quadrant = names(which.max(tb)),
             alt_quadrant = names(sort(tb, decreasing = TRUE))[
               names(sort(tb, decreasing = TRUE)) != hq][1],
             pct_alt = round(100 * sort(tb, decreasing = TRUE)[
               names(sort(tb, decreasing = TRUE)) != hq][1] / B, 1))
}))
stab <- merge(stab, scr[, .(country, group, borderline = boundary)], by = "country")
setorder(stab, pct_in_headline)

cat(sprintf("\n## Year bootstrap (%d resamples of the %d window years)\n\n", B, length(yrs)))
print(kable(stab[, .(country, group, headline_quadrant, pct_in_headline,
                     alt_quadrant, pct_alt, borderline)], format = "pipe"))

cat(sprintf("\n  Countries in their headline quadrant in <90%% of resamples: %d of 27\n",
            sum(stab$pct_in_headline < 90)))
cat(sprintf("  Median stability: %.1f%%   |   minimum: %.1f%% (%s)\n",
            median(stab$pct_in_headline), min(stab$pct_in_headline),
            stab$country[which.min(stab$pct_in_headline)]))
cat(sprintf("  Mean stability by quadrant:\n"))
print(kable(stab[, .(n = .N, mean_pct = round(mean(pct_in_headline), 1)),
                 by = headline_quadrant][order(mean_pct)], format = "pipe"))

# --- 2. Leave-one-out ---------------------------------------------------------
loo <- rbindlist(lapply(base_d$iso3, function(drop) {
  d <- base_d[iso3 != drop]
  q <- score_from(d)
  moved <- d$country[q != headline[match(d$country, country)]$quadrant]
  data.table(dropped = ind[iso3 == drop]$country, n_moved = length(moved),
             moved = paste(moved, collapse = ", "))
}))
setorder(loo, -n_moved)
cat("\n\n## Leave-one-out: dropping a country moves the medians for everyone else\n\n")
print(kable(loo[n_moved > 0], format = "pipe"))
cat(sprintf("\n  %d of 27 drops change at least one other country's quadrant.\n",
            sum(loo$n_moved > 0)))
cat(sprintf("  Mean countries moved per drop: %.2f of 26\n", mean(loo$n_moved)))

fwrite(stab, here("data/tidy/quadrant_stability.csv"))

# --- Figure -------------------------------------------------------------------
pd <- copy(stab)
pd[, country := factor(country, levels = pd[order(pct_in_headline)]$country)]
pd[, headline_quadrant := factor(headline_quadrant, levels = Q_LEVELS)]

p <- ggplot(pd, aes(pct_in_headline, country, fill = headline_quadrant)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 90, linetype = 2, colour = "grey45") +
  scale_fill_manual(NULL, values = c(Winners = "#0B3C5D", Exposed = "#2E7CA8",
                                     `Low-stakes` = "#F09372", `At risk` = "#E65032")) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02)),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Share of bootstrap resamples in the assigned quadrant", y = NULL,
       title = "How firm is each country's quadrant?",
       subtitle = paste0(B, " resamples of the ", REF_FIRST_YEAR, "-", REF_LAST_YEAR,
                         " window years. Dashed line = 90%.",
                         "\nComplexity is held fixed, so this slightly understates instability.")) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text.y = element_text(size = 7.6),
        plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(colour = "grey35", size = 8, lineheight = 1.15))

ggsave(here("plots/quadrant_stability.png"), p, width = 7.0, height = 6.6, dpi = 300)
ggsave(here("plots/quadrant_stability.pdf"), p, width = 7.0, height = 6.6)

message("\nappendix_quadrant_stability.R done.")
