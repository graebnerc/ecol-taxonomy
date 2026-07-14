# _archive

Superseded material, kept for reference only. **Nothing here is part of the current
pipeline** (`R/01`–`R/07`, `R/build_green_list.R`). Paths inside these files may be stale.

- `R/` — the original exploratory clustering scripts. `clustering_legacy.R` was the first
  canonical clustering (now superseded by `R/05_clustering.R`); `clustering2.R` and
  `clustering-experiments.R` were variable-set / cluster-count experiments.
- `plots/` — figures from that early exploration (externalization, GWP-vs-GDP, fossil
  variants), not produced by the current pipeline.
- `data/` — orphan raw Eurostat CSVs (`eurostat_fossil-share.csv`, `eurostat_renew-share.csv`)
  no longer read by any live script.
- `quarto/` — rendered artifacts of the legacy report (`.html`, `.pdf`, `_files`). The
  report source `quarto/CountryTaxonomy.qmd` remains live for the Phase 7 rebuild.
- `md/` — an exploratory note on Eurostat energy-balance variables.
- `sql/` — testing / notes SQL. The live PATSTAT extraction query stays at
  `sql/get_green_patents.sql`.
