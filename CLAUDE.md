# CLAUDE.md

Guidance for working in this repository.

> **When working on the paper:** `info/PaperTodos.md` tracks open items from
> reviewer/presentation feedback that must be folded into the manuscript. Read it
> before drafting or revising paper text, act on the relevant open items, and keep
> it in sync — check off items as they are handled and append new implications as
> they surface.

## What this project is

Empirical work for **Work Package 1** of the OPUS project *"The green transition and
economic polarization in Europe"* (project description in `info/ProjectProposal.pdf`).
WP1 (lead: C. Gräbner-Radkowitsch) builds a **national-level country taxonomy** of the
EU-27 that captures how well-placed each Member State is for the European green
transition, and asks whether the transition risks reinforcing existing socio-economic
polarization (core vs. periphery vs. "workbench" East).

The proposal frames the taxonomy along **four dimensions** organised as two
*vulnerabilities* and two *potentials* (Fig. 1 of the proposal). All four are now
operationalised, each as one part of a two-part axis (see `04_typology.R`):

| Block | Dimension | Implemented as |
|-------|-----------|----------------|
| Vulnerability | Energy-intensity of production | **twin sub-index**: carbon intensity + energy intensity, per unit value added |
| Vulnerability | Current inputs to domestic production (energy sources / mix) | **standalone**: fossil share of gross available energy (demand-side) |
| Potential | Actual production & service activities (economic complexity, green products) | **twin sub-index**: GCI + GCP (Mealy & Teytelboym 2022) |
| Potential | Capabilities for environmental activities (innovation) | **standalone**: EPO green patent applications p.c. |

Implemented methods: descriptive statistics, PCA, IO-based footprints (production- **and**
consumption-based, incl. bilateral origin×destination flows), green economic complexity
(Hidalgo–Hausmann / Mealy–Teytelboym), and hierarchical clustering — the last demoted to a
robustness layer, since silhouette/gap diagnostics favour the continuous 2-D map over hard
clusters. Regression and patent *network* analysis remain unimplemented and are not
currently planned.

**The paper is drafted in `writing/`** (see below), not from these scripts directly.

## Repo layout

Numbered pipeline (run in order from the project root; see README):

- `R/config.R` — central settings: reference window (`REF_FIRST_YEAR`/`REF_LAST_YEAR` =
  **2017–2021** since 2026-09-02), the green-patent measure (`PATENT_MEASURE` =
  `"applications"`; `"grants"` reproduces the pre-2026-09 headline), and output paths.
  Sourced by the numbered scripts. The window is the most recent one in which every
  input is observed and complete — EXIOBASE emissions end at 2022 and patent
  applications at 2021.
- `R/01_build_indicators.R` — reads `full_taxonomy_data.csv` (+ `new_data.csv`), builds the
  per-country indicator table `data/tidy/taxonomy_indicators.csv`. All per-capita/share
  transforms live in `R/functions/indicators.R::build_indicator_table()`.
- `R/02_complexity.R` — green economic complexity. Computes ECI/PCI/GCI/GCP from Atlas
  HS92 data on the global country set, pooled over the reference window, extracts EU-27 →
  `data/tidy/green_complexity_eu.csv`. Math in `R/functions/complexity.R`. Caching is
  **window-agnostic**: a wide by-year table (`data/raw/exports_by_year_1224.rds`, 2012–2024)
  is built once from the 968MB Atlas read, and each window's pooled table is derived from it
  and cached under a window-stamped name (`pooled_exports_1721.rds`). Changing the reference
  window therefore costs no Atlas re-read. Both caches gitignored. **Run before `01`** so the
  indicator table folds in GCI.
- `R/03_descriptives.R` — Phase 2: correlation matrix, income-drivenness (R² of each
  indicator on log GDP p.c.), ranked bar charts. Writes figures + `indicator_*.csv`.
- `R/04_typology.R` — Phase 3/4 headline: PCA per block → vulnerability & potential PC1
  scores, the go/no-go independence check, and the 2-D quadrant map (`plots/typology_map.*`,
  `data/tidy/taxonomy_scores.csv`). **Four-dimension structure** (`axis_score`): each block
  is a two-indicator *twin sub-index* + a *standalone*, combined at equal weight —
  vulnerability = emission intensity (carbon + energy, per value added) + fossil dependency
  (demand-side); potential = green complexity (GCI + GCP) + green innovation (patents).
  Rationale/derivation in `info/PaperTodos.md`.
- `R/05_clustering.R` — clustering robustness layer (Ward on the six block variables,
  dendrogram, alluvial vs. growth-model groups, membership); helpers in
  `R/functions/clustering_helpers.R`.
- `R/06_validation.R` — Phase 5: external validity of the scores (renewable share, GDP
  growth; partial correlations net of GDP p.c.) and comparison with the Gräbner et al.
  growth-model / geographic groups (group means, quadrant contingency, Cramér's V,
  alluvial). Writes `validation_*.csv` and `plots/validation_*`.
- `R/07_robustness.R` — Phase 6 (section A): per-year vs pooled complexity, score-spec
  sensitivity **against the structured headline** (flat single-PCA blocks, twin mean vs PCA,
  robust scaling, twin:standalone part weight, GCI vs ECI, renewable-only GCI, production-
  based fossil, consumption-based carbon accounting), cluster-number diagnostics
  (silhouette/gap), outlier drops, patent grants-vs-applications, and the full-typology
  indicator-window shift (reference window ±1). Writes `robustness_specs.csv`.
- `R/appendix_*.R` — standalone scripts outside the audited `01`–`07` pipeline.
  *Design support*: `structure_map` (flat vs two-part axes), `decomposed_map`, `pc1_cos2`,
  `quadrant_profiles` (classification table + per-quadrant descriptives),
  `patent_options` (grants vs applications: truncation profile and rank agreement),
  `window_options` (whole typology rebuilt on eight candidate windows, incl. a
  2014–2017 row matching EORA coverage so the MRIO choice can be isolated from the
  window choice).
  *Findings*: `burden_responsibility` (production- vs consumption-based footprints),
  `offshoring_origins` (bilateral test of the offshoring mechanism claim + the
  growth-model gradient under both weightings).
  *Exploratory*: `window_coverage`, `capability_trajectory`, `vulnerability_drift`,
  `forward_validation`.
- `R/functions/typology.R` — shared scoring helpers (`scale_mat`, `block_score`,
  `axis_score`, `assign_quadrant`) used by `04` and `07`. `axis_score` builds the two-part
  (twin sub-index + standalone) axes; the four dimensions live in `R/config.R`
  (`INTENSITY_VARS`, `FOSSIL_VAR`, `COMPLEXITY_VARS`, `INNOV_VAR`; `VULN_VARS`/`POT_VARS`
  are kept as flat lists for the robustness specs).
- `R/dependencies.R` — installs required packages; notes on pinning with renv (Phase 0).

Supporting:

- `R/get_data.R` — builds `data/tidy/full_taxonomy_data.csv` from Eurostat energy
  balances (`nrg_bal_c`, `nrg_bal_s`), World Bank population, EXIOBASE footprints and
  PATSTAT green patents (see `sql/`). Downloads are gated by `if` flags near the top.
- `R/get_data_exiobase.R` — computes the EXIOBASE layer **in-repo** from the official
  IOT archives (EXIOBASE 3.10.2, Zenodo record 20051562; fetched by
  `data/raw/exiobase/fetch.sh`, gitignored). Replaces the former external Python script.
  Writes `exiobase_totals.csv` (per-region GWP_pba/Imports/Exports, value added,
  employment) and `exiobase_bilateral.csv` (the 49×49 origin×destination embodied-GHG
  matrix). Restartable per year. **2023–2024 are nowcasts with broken emissions**
  (`CO2 - combustion - air` identically zero) — flagged `emissions_complete = FALSE`
  and dropped downstream; the last complete year is 2022.
- `R/update_panel_exiobase.R` — swaps only the five EXIOBASE columns into the panel,
  asserting every other column is unchanged.
- `R/get_data_patents_patstat.R` — ingests and validates the PATSTAT **v2** extract
  (`sql/get_green_patents_v2.sql`), which is the current source of both patent measures.
  Quantifies the two defects v2 fixes and writes `data/tidy/green_patents_panel.csv`.
  Exits cleanly with instructions if the extract is absent.
- `R/get_data_patents_oecd.R` — OECD ENV-TECH applications, kept as an independent
  cross-check on PATSTAT and as a fallback source. Carries both measures on the same
  underlying EPO data, which is what allowed grants-vs-applications to be compared
  without a source change confounding it.
- `R/update_panel_patents.R` — adds `GreenPatentsApps_n` to the panel, preferring the
  PATSTAT v2 extract and falling back to OECD. An absent country-year is a **true zero**
  (the SQL `GROUP BY` emits no row), but it is filled only where a zero is plausible —
  country present elsewhere, small counts in the window — and errors otherwise.
- `R/get_data_extra.R` — builds the external-validator panel `data/tidy/new_data.csv`
  (GDP PPP/real + `renew_share_overall`), one row per country-year. `DOWNLOAD = TRUE`
  rebuilds from WDI + Eurostat `nrg_ind_ren`; `DOWNLOAD = FALSE` (default) reproduces it
  offline from `_archive/data/new_data_blended_raw.csv` by selecting the overall (REN)
  series. Fixes audit finding B1 (the old file duplicated each country-year four times).
- `R/build_writing_pack.R` — regenerates the machine parts of `writing/` (evidence tables,
  figures, `numbers.md`) from the pipeline output. **Run after any analysis change**, or the
  drafting pack silently goes stale — the writing assistant cannot detect drift because it
  cannot see the pipeline.
- `R/build_green_list.R` — reconstructs the green-product list in HS1992 6-digit from the
  OECD CLEG (`info/OECD-Report_List.pdf`, Table A.1) via the HS2007→HS1992 concordance
  (`info/HS 2007-to-HS1992 .xls`). Writes `data/tidy/green_products_hs6.csv` (244 codes,
  consumed by `02_complexity.R`) and `green_products_cleg_hs2007.csv` (provenance).
- `R/country_classification.R` — defines `base_countries` (EU-27) and
  `get_country_classification(x, classification)` mapping ISO codes to the Gräbner et al.
  (2020) growth-model groups (`jee`: Core / Finance / Periphery / Workbench / Germany /
  France) and to `geo_struc` (Central / Eastern / Southern Europe). Sourced by all scripts.
- `quarto/CountryTaxonomy.qmd` — the (legacy) report source, to be rebuilt around the
  numbered pipeline in Phase 7; its old rendered `.html`/`.pdf` are in `_archive/quarto/`.
- `sql/get_green_patents_v2.sql` — **the current** PATSTAT query. Returns applications and
  grants side by side by filing year, and fixes a defect in `get_green_patents.sql`: that
  query is `COUNT(appln.appln_id)` over joins to the CPC and applicant tables, so each
  application was counted once per matching CPC symbol × same-country applicant. The
  inflation was 1.67× overall and **differential** (Slovakia 2.40×, Greece 2.34× vs
  Netherlands 1.49×), i.e. it over-counted the eastern/southern periphery relative to the
  core. v2 uses `COUNT(DISTINCT ...)` and returns the old behaviour as `n_raw_join` so the
  distortion stays measurable.
- `sql/get_green_patents.sql` — superseded v1 query, kept for provenance (testing SQL /
  notes are in `_archive/sql/`).
- `sql/get_green_patents_v3_all_offices.sql` — robustness query without the EPO-only
  restriction (`appln_auth = 'EP'`), returning EPO-only and all-offices counts side by
  side so the two cannot drift apart. Consumed by `R/appendix_patent_offices.R`, which
  asserts the EPO columns reproduce v2 exactly before comparing. **Not yet run.**
- **PATSTAT query results live in `sql/`, beside the query that produced them**
  (`sql/get_green_patents_v2.csv`, and `_v3.csv` when it exists). `data/raw/*` is
  gitignored because it holds large re-downloadable sources; a PATSTAT extract is the
  opposite — small, and impossible to regenerate without database access — so it is
  version-controlled with its provenance instead. The ingestion scripts look in `sql/`
  first and fall back to `data/raw/` and `data/tidy/`.
- `data/tidy/` — analysis-ready CSVs. `full_taxonomy_data.csv` and `taxonomy_indicators.csv`
  are committed vantage points, so the pipeline runs without re-downloading. `data/raw/`
  is gitignored.
- `plots/` — current pipeline figures only (stale exploration plots are in `_archive/plots/`).
- `info/` — proposal, Mealy & Teytelboym green-complexity paper, OECD CLEG + HS concordance,
  OECD EPS, and the implementation plan.
- `writing/` — **self-contained pack for drafting the paper**, so a writing assistant can
  work with access to this directory alone. It has **its own `writing/CLAUDE.md`**, which is
  what such an assistant loads (the root file is invisible to it); that file carries the
  drafting rules — never invent a number, `evidence/numbers.md` is authoritative, nothing in
  `open-questions.md` is settled, do not soften the caveats. Alongside it: the hand-written
  `results-summary.md` and `open-questions.md`, generated `evidence/` (result tables +
  `numbers.md`) and `figures/`, and `output/` for drafts.
  Regenerate the generated parts with `Rscript R/build_writing_pack.R` after ANY
  pipeline change — otherwise the pack silently goes stale, and the assistant cannot
  detect it.
- `_archive/` — superseded scripts/plots/report/notes, reference only (see `_archive/README.md`).

## Key data columns (`data/tidy/full_taxonomy_data.csv`)

Panel, country (ISO3) × year, EU-27. Notable columns:
- `GWP_Imports`, `GWP_Exports` — consumption-side emissions embodied in trade (EXIOBASE, GWP).
  Country totals; for *where* embodied imports originate use `data/tidy/exiobase_bilateral.csv`
  (49×49 origin×destination), which is what refuted the offshoring-mechanism claim
  (see `info/PaperTodos.md` § Offshoring).
- `GWP_pba` — production-based GHG emissions; `ValueAdded_pba`, `Employment_pba` — sectoral totals.
  The headline carbon variable is production-based on purpose (adjustment *burden*); the
  consumption-based counterparts `CarbonIntensityCBA_normed` / `GWP_cba_normed` measure
  *responsibility* and are used for robustness and the interpretive layer, not the axis.
- `PrimaryEnergyProduction`, `FinalEnergyConsumption`, `EnergyExports`/`Imports`/`NetTrade` (GWh, Eurostat).
- `population` (1000s), `ShareRenewables_PrimEnProd`, `ShareFossils_PrimEnProd`.
- Green patents: `GreenPatents_n` (EPO **grants** by filing year) and `GreenPatentsApps_n`
  (EPO **applications**, added by `R/update_panel_patents.R`). `indicators.R` builds both as
  `GreenPatentsGrants_normed` / `GreenPatentsApps_normed` and `PATENT_MEASURE` selects the
  headline, so `07` can test the other without rebuilding the panel.
- `new_data.csv` adds `GDP_ppp`, `GDP_real`, `ShareFossils_GrossAvEn`, and the overall
  renewable share `renew_share_overall` (Eurostat SHARES REN). One row per country-year;
  built by `R/get_data_extra.R` (do not re-introduce the old blended multi-indicator rows).

Convention: raw levels are turned into per-capita or share indicators (`*_normed`) inside
each script before scaling — normalisation is **not** baked into the tidy CSV.

## Conventions

- R with `here::i_am(...)` for paths, `data.table::fread` + `dplyr`/`tidyr`, `countrycode`
  for country-code mapping, `cluster`/`factoextra` for clustering, `ggalluvial` for flow plots.
- This is an RStudio project (`ecol-taxonomy.Rproj`); reports are Quarto.
- Prose in reports is English; a few code comments are in German.

## State of play (2026-09-02)

The analysis is complete and stable; what remains is writing and a few optional checks.

**Settled.** All four proposal dimensions are operationalised. The headline is the
two-part-axis typology on a **2017–2021** window with **EXIOBASE 3.10.2** and **EPO patent
applications**. The map survives everything tested: 0/27 under the MRIO release change
(an 18% revision of the emission accounts), 0/27 under grants-vs-applications, 0/27 across
every window from 2017 on. It moves most (8/27) under flat single-PCA aggregation, which is
precisely why the two-part construction is argued for rather than asserted.

**Open — see `writing/open-questions.md` for the full list with evidence.**

1. **Gating: descriptive typology or an explicit H1–H3 test?** H1–H3 are referenced
   throughout the project material and written down nowhere. The evidence constrains the
   choice: the *narrow* claim (catch-up East more vulnerable **and** less capable than the
   Core) is strongly supported, but the *broad* claim (four differentiated growth models) is
   not — Finance, Periphery and Workbench are statistically indistinguishable from one
   another. The pattern is Core vs everyone else.
2. GCI wording — it correlates ~0.998 with a plain green-product count, so it is green
   *diversity* rather than sophistication. A referee will check.
3. Target journal, and whether to make a policy ask.
4. Optional checks that each close a referee line: EORA on the 2014–2017 window (isolates
   MRIO choice from window choice), a patent run without the EPO-only restriction, sectoral
   energy splits for the flagged countries.

**A note on how this pipeline fails.** Three times in two days it returned *plausible
numbers* rather than an error: EXIOBASE nowcast years with a zeroed dominant stressor, an
NA emission column that only worked because zero-output sectors masked it, and a window
silently shortening itself because `build_indicator_table()` averages with `na.rm = TRUE`.
Each is now guarded. When adding anything here, prefer an assertion that fails loudly over
a computation that quietly succeeds.
