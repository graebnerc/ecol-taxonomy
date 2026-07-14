# 07 - Robustness & sensitivity (Phase 6, section A: self-contained checks).
#
# 1. Complexity: per-year GCI/ECI vs the pooled 2014-2018 cross-section.
# 2. Typology scores: alternative specifications (PCA vs mean, robust scaling,
#    GCI vs ECI, dropping vulnerability variables, renewable-only GCI) -> rank
#    correlation with the baseline and number of countries changing quadrant.
# 3. Clustering: silhouette + gap statistic for the number of clusters.
# 4. Outlier sensitivity: drop Luxembourg / Malta and re-map.

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

compute_complexity <- function(exp_dt, codes = green_codes) {
  rca <- build_rca_matrix(exp_dt)
  ci  <- complexity_indices(rca$M)
  gi  <- green_indicators(rca$M, ci$PCI, codes)
  gi$ECI <- ci$ECI[gi$iso3]
  gi
}

# === 1. Per-year complexity vs pooled =========================================
cat("\n===== 1. Complexity: per-year vs pooled cross-section =====\n")
eby <- readRDS(here("data/raw/exports_by_year_1418.rds"))
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
fwrite(rob, here("data/tidy/robustness_specs.csv"))

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
message("\n07_robustness.R done: wrote data/tidy/robustness_specs.csv")
