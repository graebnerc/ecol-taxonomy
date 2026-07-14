# 02 - Green economic complexity (Dimension 4)  ***PHASE 1 STUB***
#
# This script will compute the green-complexity indicators following Mealy &
# Teytelboym (2022, info/GreenComplexity.pdf, sections 3.4-3.7). It is a stub:
# it runs, reports what is missing, and exits cleanly until the input data are
# in place. See info/ImplementationPlan.md, Phase 1a.
#
# Required inputs (to be placed under data/raw/, which is gitignored):
#   * Atlas HS92 6-digit country-product-year export values (GLOBAL country set,
#     1995 onward) -> data/raw/atlas_hs92_6d.<csv/parquet>
#   * Green HS6 code list (293 products) + renewable subset (57), HS1992 vintage
#     -> data/tidy/green_products_hs6.csv  (columns: hs6, is_green, is_renewable)
#
# Method (compute on the GLOBAL country set, then extract EU-27):
#   1. RCA (Balassa)  -> binary M matrix (RCA > 1)
#   2. ECI, PCI (standardised, Hausmann et al. 2014)
#   3. GCI  = sum of PCI over green products a country is competitive in
#   4. proximity phi, density omega -> GCP (Green Complexity Potential), GAP
#   5. Validate ECI/PCI against Atlas's published values (+ TradeWeave cross-check)
#
# Output: data/tidy/green_complexity_eu.csv (country, ECI, GCI, GCP), averaged
# over the reference window; joined into the indicator table by 01_.

here::i_am("R/02_complexity.R")
library(here)
source(here("R/config.R"))

atlas_path <- here("data/raw/atlas_hs92_6d.csv")
green_path <- here("data/tidy/green_products_hs6.csv")

missing <- c(
  if (!file.exists(atlas_path)) "Atlas HS92 6-digit export data (data/raw/atlas_hs92_6d.csv)",
  if (!file.exists(green_path)) "green HS6 product list (data/tidy/green_products_hs6.csv)"
)

if (length(missing) > 0) {
  message("02_complexity.R is a stub - required inputs not found:\n  - ",
          paste(missing, collapse = "\n  - "),
          "\nSee info/ImplementationPlan.md (Phase 1a). Skipping.")
} else {
  stop("Inputs present but the green-complexity computation is not implemented yet (Phase 1).")
}
