# Appendix table: per-country quality of representation on PC1 (cos^2) for each
# block, i.e. the share of each country's own (multivariate) variation that the
# single PC1 score captures. Uses the exact typology machinery (config VULN_VARS /
# POT_VARS, z-scaling, prcomp center=FALSE) so numbers match 04_typology.R.
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

v <- block_cos2(ind, VULN_VARS)
p <- block_cos2(ind, POT_VARS)

tab <- tibble(
  country            = ind$country,
  vuln_cos2          = round(100 * v$cos2, 0),
  pot_cos2           = round(100 * p$cos2, 0)
) |> arrange(country)

fwrite(tab, here("data/tidy/appendix_pc1_cos2.csv"))

cat(sprintf("Block-level PC1 variance explained: vulnerability %.0f%%, potential %.0f%%\n",
            100 * v$ve, 100 * p$ve))
cat(sprintf("Mean per-country cos^2: vulnerability %.0f%%, potential %.0f%%\n\n",
            mean(100 * v$cos2), mean(100 * p$cos2)))

# print as a markdown table for pasting into the slide
cat("| Country | Vuln PC1 | Pot PC1 |\n|---|--:|--:|\n")
for (i in seq_len(nrow(tab)))
  cat(sprintf("| %s | %d%% | %d%% |\n",
              tab$country[i], tab$vuln_cos2[i], tab$pot_cos2[i]))
