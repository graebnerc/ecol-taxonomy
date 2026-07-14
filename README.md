# Ecological taxonomy for the EU (WP1)

Empirical work for Work Package 1 of the OPUS project on the green transition and
economic polarization in Europe. See `info/ProjectProposal.pdf` for the project,
`info/ImplementationPlan.md` for the research plan, and `CLAUDE.md` for a repo map.

## Pipeline

Run the numbered scripts in order from the project root:

```r
source("R/dependencies.R")   # install required packages (see file re: renv)
Rscript R/build_green_list.R      # OECD CLEG -> data/tidy/green_products_hs6.csv (HS92)
Rscript R/02_complexity.R         # green complexity -> data/tidy/green_complexity_eu.csv
Rscript R/01_build_indicators.R   # base indicators + complexity -> taxonomy_indicators.csv
Rscript R/03_descriptives.R       # correlations, income R^2, ranked bar charts
Rscript R/04_typology.R           # PCA per block -> vulnerability x potential map + scores
Rscript R/05_clustering.R         # clustering robustness layer -> dendrogram + membership
```

`02_complexity.R` needs the Atlas HS92 6-digit file at `data/raw/atlas_hs92_6d.csv`
(gitignored; see `data/raw/atlas_hs92_6d_REFERENCE.txt`). It runs before `01` so the
indicator table can fold in ECI/GCI/GCP.

- `R/config.R` — central settings (reference window `2014–2018`, paths).
- `R/country_classification.R` — EU-27 list + growth-model / geographic group lookups.
- `R/functions/` — reusable helpers (`indicators.R`, `clustering_helpers.R`).
- `R/experiments/` — archived exploratory scripts (`clustering_legacy.R` = the former
  canonical clustering; `clustering2.R`, `clustering-experiments.R`). Reference only.

## Data

`data/tidy/taxonomy_indicators.csv` is built from `full_taxonomy_data.csv`, which is
committed so the pipeline runs without re-downloading. To rebuild the raw inputs:

* `R/get_data.R` downloads and assembles `full_taxonomy_data.csv` (Eurostat energy
  balances, WDI population, EXIOBASE footprints, PATSTAT green patents).
* `sql/get_green_patents.sql` recreates `patstat_green-patents.csv` (rest of `sql/` is testing).
