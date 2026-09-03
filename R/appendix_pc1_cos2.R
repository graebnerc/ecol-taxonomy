# Appendix table: per-country quality of representation on PC1 (cos^2), i.e. the
# share of each country's own multivariate variation that a single PC1 captures.
#
# CORRECTED 2026-09-03. The header previously claimed these numbers "match
# 04_typology.R". They did not. This script reduced each block with a SINGLE PCA
# over all three block variables (VULN_VARS / POT_VARS), which stopped being the
# headline in July when 04 moved to axis_score() -- a twin sub-index (PC1 of the
# two correlated indicators) combined with a standalone. It was therefore
# reporting representation quality for an abandoned specification while asserting
# the opposite. Same defect as appendix_decomposed_map.R had.
#
# Both are now reported and labelled:
#   twin_cos2  PC1 of the TWO twin indicators -- what the headline actually uses
#   flat_cos2  PC1 of all three block variables -- the alternative in 07
# Writes data/tidy/appendix_pc1_cos2.csv and prints a markdown table.

suppressMessages({
  library(here); library(data.table); library(dplyr)
})
here::i_am("R/appendix_pc1_cos2.R")
source(here("R/config.R"))
source(here("R/functions/typology.R"))

ind <- as_tibble(fread(here("data/tidy/taxonomy_indicators.csv")))

# Per-country cos^2 on PC1 for one block, plus the block-level variance share.
block_cos2 <- function(df, vars) {
  X  <- scale_mat(df[, vars], "z")                 # identical scaling to block_score()
  pc <- prcomp(X, center = FALSE, scale. = FALSE)
  cos2 <- pc$x[, 1]^2 / rowSums(pc$x^2)            # rotation is orthonormal -> norm preserved
  list(cos2 = cos2, ve = summary(pc)$importance[2, 1])
}

# Headline construction: PC1 of the twin pair only.
v_twin <- block_cos2(ind, INTENSITY_VARS)
p_twin <- block_cos2(ind, COMPLEXITY_VARS)
# The flat alternative, kept for comparison with the 07 spec.
v <- block_cos2(ind, VULN_VARS)
p <- block_cos2(ind, POT_VARS)

tab <- tibble(
  country            = ind$country,
  vuln_twin_cos2     = round(100 * v_twin$cos2, 0),
  pot_twin_cos2      = round(100 * p_twin$cos2, 0),
  vuln_flat_cos2     = round(100 * v$cos2, 0),
  pot_flat_cos2      = round(100 * p$cos2, 0)
) |> arrange(country)

fwrite(tab, here("data/tidy/appendix_pc1_cos2.csv"))

cat(sprintf("HEADLINE construction (twin PC1):  variance explained -- intensity %.0f%%, complexity %.0f%%\n",
            100 * v_twin$ve, 100 * p_twin$ve))
cat(sprintf("  mean per-country cos^2:          intensity %.0f%%, complexity %.0f%%\n",
            mean(100 * v_twin$cos2), mean(100 * p_twin$cos2)))
cat(sprintf("FLAT alternative (all 3 vars):     variance explained -- vulnerability %.0f%%, potential %.0f%%\n",
            100 * v$ve, 100 * p$ve))
cat(sprintf("  mean per-country cos^2:          vulnerability %.0f%%, potential %.0f%%\n\n",
            mean(100 * v$cos2), mean(100 * p$cos2)))

cat("| Country | Intensity twin | Complexity twin | Vuln flat | Pot flat |\n")
cat("|---|--:|--:|--:|--:|\n")
for (i in seq_len(nrow(tab)))
  cat(sprintf("| %s | %d%% | %d%% | %d%% | %d%% |\n", tab$country[i],
              tab$vuln_twin_cos2[i], tab$pot_twin_cos2[i],
              tab$vuln_flat_cos2[i], tab$pot_flat_cos2[i]))
