# CLAUDE.md

Guidance for working in this repository.

## What this project is

Empirical work for **Work Package 1** of the OPUS project *"The green transition and
economic polarization in Europe"* (project description in `info/ProjectProposal.pdf`).
WP1 (lead: C. Gräbner-Radkowitsch) builds a **national-level country taxonomy** of the
EU-27 that captures how well-placed each Member State is for the European green
transition, and asks whether the transition risks reinforcing existing socio-economic
polarization (core vs. periphery vs. "workbench" East).

The proposal frames the taxonomy along **four dimensions** organised as two
*vulnerabilities* and two *potentials* (Fig. 1 of the proposal):

| Block | Dimension | Status in repo |
|-------|-----------|----------------|
| Vulnerability | Current inputs to domestic production (energy sources / mix) | Partial (energy mix shares) |
| Vulnerability | Energy-intensity of production (esp. export activities) | Partial (final consumption; intensity computed ad hoc) |
| Potential | Capabilities for environmental activities (innovation) | Partial (green patents only) |
| Potential | Actual production & service activities (economic complexity, green products) | **Missing (TBD)** |

The intended methods: descriptive statistics, PCA, clustering, IO-based footprints
(production- and consumption-based), regression, patent network analysis, and economic
complexity (Hidalgo–Hausmann) / product space. So far only **hierarchical clustering**
has been implemented.

## Repo layout

Numbered pipeline (run in order from the project root; see README):

- `R/config.R` — central settings: reference window (`REF_FIRST_YEAR`/`REF_LAST_YEAR` =
  2014–2018), output paths. Sourced by the numbered scripts.
- `R/01_build_indicators.R` — reads `full_taxonomy_data.csv` (+ `new_data.csv`), builds the
  per-country indicator table `data/tidy/taxonomy_indicators.csv`. All per-capita/share
  transforms live in `R/functions/indicators.R::build_indicator_table()`.
- `R/02_complexity.R` — green economic complexity (Dimension 4). **Currently a Phase 1
  stub**: runs, reports which inputs are missing (Atlas HS92 6-digit data + green HS6
  list), and exits cleanly. Method spec in the file header and `info/ImplementationPlan.md`.
- `R/03_analysis.R` — the consolidated typology analysis (Ward clustering, dendrogram,
  alluvial vs. growth-model groups, membership table). Reproduces the former canonical
  result; helpers in `R/functions/clustering_helpers.R`.
- `R/dependencies.R` — installs required packages; notes on pinning with renv (Phase 0).

Supporting:

- `R/get_data.R` — builds `data/tidy/full_taxonomy_data.csv` from Eurostat energy
  balances (`nrg_bal_c`, `nrg_bal_s`), World Bank population, EXIOBASE footprints
  (`TXNY_GWP_Trade.csv`, produced by an external Python script in another project), and
  PATSTAT green patents (see `sql/`). Downloads are gated by `if` flags near the top.
- `R/country_classification.R` — defines `base_countries` (EU-27) and
  `get_country_classification(x, classification)` mapping ISO codes to the Gräbner et al.
  (2020) growth-model groups (`jee`: Core / Finance / Periphery / Workbench / Germany /
  France) and to `geo_struc` (Central / Eastern / Southern Europe). Sourced by all scripts.
- `R/experiments/` — archived exploratory scripts, **reference only**: `clustering_legacy.R`
  (former canonical clustering, now superseded by `03_analysis.R`), `clustering2.R`,
  `clustering-experiments.R` (variants with different variable sets / cluster counts).
- `quarto/CountryTaxonomy.qmd` — report from the legacy pipeline (renders to `.html`/`.pdf`);
  to be rebuilt around the numbered pipeline in Phase 7. Contains an "Open issues" list.
- `sql/get_green_patents.sql` — PATSTAT query for EPO-tagged green patents (rest is testing).
- `data/tidy/` — analysis-ready CSVs. `full_taxonomy_data.csv` and `taxonomy_indicators.csv`
  are committed vantage points, so the pipeline runs without re-downloading. `data/raw/`
  is gitignored.
- `plots/`, `md/` — exported figures and an energy-variable reference note.
- `info/` — proposal, Mealy & Teytelboym green-complexity paper, and the implementation plan.

## Key data columns (`data/tidy/full_taxonomy_data.csv`)

Panel, country (ISO3) × year, EU-27. Notable columns:
- `GWP_Imports`, `GWP_Exports` — consumption-side emissions embodied in trade (EXIOBASE, GWP).
- `GWP_pba` — production-based GHG emissions; `ValueAdded_pba`, `Employment_pba` — sectoral totals.
- `PrimaryEnergyProduction`, `FinalEnergyConsumption`, `EnergyExports`/`Imports`/`NetTrade` (GWh, Eurostat).
- `population` (1000s), `GreenPatents_n`, `ShareRenewables_PrimEnProd`, `ShareFossils_PrimEnProd`.
- `new_data.csv` adds `GDP_ppp`, `GDP_real`, and gross-available-energy fossil/renewable shares.

Convention: raw levels are turned into per-capita or share indicators (`*_normed`) inside
each script before scaling — normalisation is **not** baked into the tidy CSV.

## Conventions

- R with `here::i_am(...)` for paths, `data.table::fread` + `dplyr`/`tidyr`, `countrycode`
  for country-code mapping, `cluster`/`factoextra` for clustering, `ggalluvial` for flow plots.
- This is an RStudio project (`ecol-taxonomy.Rproj`); reports are Quarto.
- Prose in reports is English; a few code comments are in German.

## Known open problems (state as of last commit)

1. Two of the four proposal dimensions (brown employment, green products / economic
   complexity) are not yet operationalised.
2. The pipeline is now consolidated (`01`–`03`), but the clustering variable set still
   needs principled selection (Phase 3 of `info/ImplementationPlan.md`).
3. Per-capita normalisation makes economic scale dominate, so clusters largely reproduce
   the known rich-North / poorer-East–South development-model split (limited novelty).
4. All four dimensions are pooled into one Euclidean distance, so the vulnerability vs.
   potential structure of Fig. 1 is not preserved in the output.
5. No PCA, no cluster-number validation (silhouette/gap), no stability checks; the authors
   note the cluster discrimination "is not very convincing."
