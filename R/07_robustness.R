# 07 - Robustness & sensitivity (Phase 6, section A: self-contained checks).
#
# 1. Complexity: per-year GCI/ECI vs the pooled 2014-2018 cross-section.
# 2. Typology scores: alternative specifications (PCA vs mean, robust scaling,
#    GCI vs ECI, dropping vulnerability variables, renewable-only GCI) -> rank
#    correlation with the baseline and number of countries changing quadrant.
# 3. Clustering: silhouette + gap statistic for the number of clusters.
# 4. Outlier sensitivity: drop Luxembourg / Malta and re-map.
# 5. Indicator-window shift: rebuild the FULL typology (complexity re-pooled +
#    indicators re-averaged) on 2013-2017 and 2015-2019.

here::i_am("R/07_robustness.R")
library(here)
suppressMessages({
  library(data.table); library(dplyr); library(Matrix)
  library(countrycode); library(cluster)
})
source(here("R/config.R"))
source(here("R/country_classification.R"))
source(here("R/functions/complexity.R"))
source(here("R/functions/typology.R"))

ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))
ind$iso3 <- countrycode(ind$country, "country.name", "iso3c")
eu_iso3  <- ind$iso3
green    <- fread(here("data/tidy/green_products_hs6.csv"), colClasses = list(character = "hs6"))
green_codes <- green$hs6
renew_codes <- green$hs6[green$is_renewable == 1]

compute_complexity <- function(exp_dt, codes = green_codes, min_export = 5e9) {
  rca <- build_rca_matrix(exp_dt, min_country_export = min_export)
  ci  <- complexity_indices(rca$M)
  gi  <- green_indicators(rca$M, ci$PCI, codes)
  gi$ECI <- ci$ECI[gi$iso3]
  gi
}

# === 1. Per-year complexity vs pooled =========================================
cat("\n===== 1. Complexity: per-year vs pooled cross-section =====\n")
byyear_path <- here("data/raw/exports_by_year_1319.rds")
stopifnot(
  "exports_by_year_1319.rds missing - run 02_complexity.R from the Atlas first (it builds this cache)" =
    file.exists(byyear_path))
eby <- readRDS(byyear_path)
pooled <- as_tibble(fread(here("data/tidy/green_complexity_eu.csv")))
per_year <- lapply(REF_FIRST_YEAR:REF_LAST_YEAR, function(y) {
  gi <- compute_complexity(eby[year == y, .(iso3, hs6, export)])
  gi[gi$iso3 %in% eu_iso3, c("iso3", "GCI", "ECI")]
})
names(per_year) <- REF_FIRST_YEAR:REF_LAST_YEAR
yr_cor <- sapply(per_year, function(g) {
  m <- merge(g, pooled[, c("iso3", "GCI")], by = "iso3", suffixes = c("_y", "_pool"))
  c(GCI_spearman = cor(m$GCI_y, m$GCI_pool, method = "spearman"))
})
print(round(yr_cor, 3))
cat(sprintf("EU-27 GCI rank corr (per-year vs pooled): min %.3f, mean %.3f\n",
            min(yr_cor), mean(yr_cor)))

# === 2. Typology score specifications =========================================
cat("\n===== 2. Typology scores: specification sensitivity =====\n")
# Renewable-only GCI from the pooled matrix, joined to the indicator table.
pooled_exp <- readRDS(here("data/raw/pooled_exports_1418.rds"))
rca0 <- build_rca_matrix(pooled_exp)
ci0  <- complexity_indices(rca0$M)
gci_ren <- green_indicators(rca0$M, ci0$PCI, renew_codes)[, c("iso3", "GCI")]
names(gci_ren)[2] <- "GCI_ren"
ind <- left_join(ind, gci_ren, by = "iso3")

score_spec <- function(df, vuln_vars, pot_vars, method = "pca", scale = "z") {
  v <- block_score(df, vuln_vars, "ShareFossils_normed", method, scale)$score
  p <- block_score(df, pot_vars, if ("GCI" %in% pot_vars) "GCI" else "GCI_ren", method, scale)$score
  data.frame(vuln = v, pot = p)
}
base <- score_spec(ind, VULN_VARS, POT_VARS)
base_q <- assign_quadrant(base$vuln, base$pot, "short")

specs <- list(
  "simple mean (not PCA)"      = score_spec(ind, VULN_VARS, POT_VARS, method = "mean"),
  "robust (median/MAD) scaling" = score_spec(ind, VULN_VARS, POT_VARS, scale = "robust"),
  "potential: ECI replaces GCI" = score_spec(ind, VULN_VARS, c("GreenPatents_normed", "ECI", "GCP")),
  "potential: renewable-only GCI" = score_spec(ind, VULN_VARS, c("GreenPatents_normed", "GCI_ren", "GCP")),
  "vuln: drop fossil share"    = score_spec(ind, c("CarbonIntensity_normed", "EnergyIntensity_normed"), POT_VARS),
  "vuln: drop carbon intensity" = score_spec(ind, c("EnergyIntensity_normed", "ShareFossils_normed"), POT_VARS)
)
rob <- lapply(names(specs), function(nm) {
  s <- specs[[nm]]; q <- assign_quadrant(s$vuln, s$pot, "short")
  data.frame(spec = nm,
             cor_vuln = round(cor(base$vuln, s$vuln, method = "spearman"), 2),
             cor_pot  = round(cor(base$pot,  s$pot,  method = "spearman"), 2),
             quad_changes = sum(q != base_q))
}) |> bind_rows()
print(rob, row.names = FALSE)
cat("(cor = Spearman rank corr vs baseline; quad_changes out of 27 countries)\n")
# robustness_specs.csv is written at the end, after the window-shift rows (section 5).

# --- Median-tie convention sensitivity (finding A1) ---------------------------
# assign_quadrant() uses `>=` (ties go to the high side). With n = 27 one country
# sits exactly on each median; flip to `>` (ties go low) and report who moves.
cat("\n----- Median-tie convention: >= (baseline) vs > (ties go low) -----\n")
assign_quadrant_lowtie <- function(vuln, pot) {
  mx <- median(vuln); my <- median(pot)
  ifelse(vuln <= mx & pot >  my, "Winners",
  ifelse(vuln >  mx & pot >  my, "Exposed",
  ifelse(vuln <= mx & pot <= my, "Low-stakes", "At risk")))
}
q_flip  <- assign_quadrant_lowtie(base$vuln, base$pot)
moved   <- which(q_flip != base_q)
cat(sprintf("Countries exactly on a median: %s\n",
            paste(ind$country[abs(base$vuln - median(base$vuln)) < 1e-9 |
                              abs(base$pot  - median(base$pot))  < 1e-9], collapse = ", ")))
cat(sprintf("Flipping the tie convention moves %d/27 countries:\n", length(moved)))
if (length(moved))
  for (i in moved) cat(sprintf("  %-12s %s -> %s\n", ind$country[i], base_q[i], q_flip[i]))

# --- Export threshold for the global RCA estimation (baseline 5e9) ------------
# The min-country-export filter (functions/complexity.R) is otherwise never
# varied. Recompute the global complexity at 2.5e9 / 1e10, refresh EU GCI/GCP,
# and re-score the potential axis. (All 27 EU states survive every threshold.)
cat("\n----- Export threshold for the global RCA (baseline 5e9) -----\n")
for (thr in c(2.5e9, 1e10)) {
  gi  <- compute_complexity(pooled_exp, min_export = thr)
  m   <- match(ind$iso3, gi$iso3)
  ind_t <- ind; ind_t$GCI <- gi$GCI[m]; ind_t$GCP <- gi$GCP[m]
  s   <- score_spec(ind_t, VULN_VARS, POT_VARS)
  q   <- assign_quadrant(s$vuln, s$pot, "short")
  cat(sprintf("  threshold %.1e: cor_pot %.2f (Spearman vs baseline), %d/27 quadrant changes\n",
              thr, cor(base$pot, s$pot, method = "spearman"), sum(q != base_q)))
}

# === 3. Number of clusters ====================================================
cat("\n===== 3. Clustering: how many clusters? =====\n")
Xs <- scale(as.matrix(ind[, c(VULN_VARS, POT_VARS)])); rownames(Xs) <- ind$country
d  <- dist(Xs)
sil <- sapply(2:8, function(k) {
  mean(silhouette(cutree(hclust(d, "ward.D2"), k), d)[, 3])
})
names(sil) <- 2:8
cat("Average silhouette width by k:\n"); print(round(sil, 3))
cat(sprintf("Silhouette suggests k = %s\n", names(which.max(sil))))
set.seed(1)
gap <- clusGap(Xs, FUN = function(x, k) list(cluster = cutree(hclust(dist(x), "ward.D2"), k)),
               K.max = 8, B = 50)
kgap <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")
cat(sprintf("Gap statistic (Tibshirani SE rule) suggests k = %d\n", kgap))

# === 4. Outlier sensitivity ===================================================
cat("\n===== 4. Outlier sensitivity (re-map without a country) =====\n")
for (drop in c("Luxembourg", "Malta")) {
  sub <- ind[ind$country != drop, ]
  s <- score_spec(sub, VULN_VARS, POT_VARS)
  q <- assign_quadrant(s$vuln, s$pot, "short")
  base_sub <- base_q[ind$country != drop]
  cat(sprintf("Drop %-11s -> %d/%d remaining countries change quadrant\n",
              drop, sum(q != base_sub), nrow(sub)))
}

# === 5. Indicator-window shift (2013-2017 / 2015-2019) ========================
# Shifts the FULL typology, not just complexity: re-pool the Atlas exports over
# the shifted window (the by-year cache spans 2013-2019 for exactly this),
# recompute global ECI/PCI/GCI/GCP, re-average the indicator table over the
# shifted years, and re-score. All core sources cover 2013-2019 in full
# (EXIOBASE ends 2019), so no window uses imputed or partial years.
cat("\n===== 5. Indicator-window shift (full typology re-built) =====\n")
source(here("R/functions/indicators.R"))
base_data  <- as_tibble(fread(here("data/tidy/full_taxonomy_data.csv")))
extra_data <- as_tibble(fread(here("data/tidy/new_data.csv")))
shift_spec <- function(w1, w2) {
  gi <- compute_complexity(
    eby[year >= w1 & year <= w2, .(export = sum(export)), by = .(iso3, hs6)])
  ind_w <- build_indicator_table(base_data, extra_data,
                                 first_year = w1, last_year = w2)
  ind_w$iso3 <- countrycode(ind_w$country, "country.name", "iso3c")
  m <- match(ind_w$iso3, gi$iso3)
  ind_w$GCI <- gi$GCI[m]; ind_w$GCP <- gi$GCP[m]
  stopifnot("EU country lost from complexity in shifted window" = !anyNA(ind_w$GCI))
  ind_w <- ind_w[match(ind$country, ind_w$country), ]   # align to baseline order
  score_spec(ind_w, VULN_VARS, POT_VARS)
}
win_rows <- lapply(list(c(REF_FIRST_YEAR - 1, REF_LAST_YEAR - 1),
                        c(REF_FIRST_YEAR + 1, REF_LAST_YEAR + 1)), function(w) {
  s <- shift_spec(w[1], w[2])
  q <- assign_quadrant(s$vuln, s$pot, "short")
  moved <- ind$country[q != base_q]
  cat(sprintf("  window %d-%d: cor_vuln %.2f, cor_pot %.2f (Spearman vs baseline), %d/27 quadrant changes%s\n",
              w[1], w[2],
              cor(base$vuln, s$vuln, method = "spearman"),
              cor(base$pot,  s$pot,  method = "spearman"),
              length(moved),
              if (length(moved)) paste0(": ", paste(moved, collapse = ", ")) else ""))
  data.frame(spec = sprintf("window shift %d-%d", w[1], w[2]),
             cor_vuln = round(cor(base$vuln, s$vuln, method = "spearman"), 2),
             cor_pot  = round(cor(base$pot,  s$pot,  method = "spearman"), 2),
             quad_changes = length(moved))
})
rob <- bind_rows(rob, win_rows)
fwrite(rob, here("data/tidy/robustness_specs.csv"))

message("\n07_robustness.R done: wrote data/tidy/robustness_specs.csv")
