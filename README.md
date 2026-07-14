# Ecological taxonomy of the EU-27

Empirical work for **Work Package 1** of the OPUS project *"The green transition and
economic polarization in Europe"* (project description: `info/ProjectProposal.pdf`).
WP1 builds a national-level taxonomy of EU-27 Member States capturing how well-placed
each is for the European green transition, and asks whether the transition risks
reinforcing existing core–periphery polarization.

For a full repo map see `CLAUDE.md`; for the detailed research plan and every decision
see `info/ImplementationPlan.md`.

## The approach

Rather than one pooled clustering of all indicators (which just reproduces GDP per
capita), we mirror Figure 1 of the proposal: **two vulnerability dimensions** and **two
potential dimensions**. Each block is reduced to a single PC1 score with PCA, and the
EU-27 are placed on a **vulnerability × potential map** with four quadrants. Hard
clustering is kept only as a robustness layer. The dimension that gives the paper its
novelty is **green economic complexity** (Mealy & Teytelboym 2022), computed from Atlas
trade data — it is orthogonal to income and is what lets the taxonomy say something new.

**Blocks** (`R/config.R`):
- Vulnerability = carbon intensity (GHG / value added), energy intensity (energy / VA),
  fossil share of primary energy — intensity-based (per value added), which removes the
  mechanical scale effect. The vulnerability score still correlates with income
  (R² = 0.50 vs log GDP p.c., 0.46–0.53 leave-one-out), which we read as substantive
  (catch-up economies are more carbon-intensive), while the potential axis is
  income-independent (R² = 0.07; at most 0.19 leave-one-out, when dropping Luxembourg).
- Potential = green patents per capita, Green Complexity Index (GCI), Green Complexity
  Potential (GCP).

## Pipeline

Run in order from the project root:

```r
source("R/dependencies.R")   # install required packages (see file re: renv)
Rscript R/build_green_list.R    # OECD CLEG -> data/tidy/green_products_hs6.csv (HS92)
Rscript R/02_complexity.R       # green complexity (global set) -> green_complexity_eu.csv
Rscript R/01_build_indicators.R # base indicators + complexity -> taxonomy_indicators.csv
Rscript R/03_descriptives.R     # correlations, income R^2, ranked bar charts
Rscript R/04_typology.R         # PCA per block -> vulnerability x potential map + scores
Rscript R/05_clustering.R       # clustering robustness layer -> dendrogram + membership
Rscript R/06_validation.R       # external validity + comparison with growth models
Rscript R/07_robustness.R       # sensitivity checks
```

`02_complexity.R` runs before `01` (so the indicator table folds in ECI/GCI/GCP) and needs
the Atlas HS92 6-digit file at `data/raw/atlas_hs92_6d.csv` (gitignored; source in
`data/raw/atlas_hs92_6d_REFERENCE.txt`). It caches the pooled table to a gitignored RDS so
the 968 MB read happens once. `R/get_data.R` rebuilds `full_taxonomy_data.csv` from the raw
sources (Eurostat, WDI, EXIOBASE, PATSTAT).

## What we did — and the headline results

- **Green complexity built & validated** — GCI top-10 is consistent with Mealy & Teytelboym
  Fig. 3 (Germany #1; Italy, Austria, USA, Denmark, China, Japan, Czechia, France, UK),
  cor(GCI, ECI) = 0.78. Crucially GCI is **income-independent** (R² vs log GDP p.c. ≈ 0.01):
  the catch-up East (Czechia, Poland) outranks high-GDP finance economies (Luxembourg,
  Ireland, Netherlands).
- **Typology passes the go/no-go check** — the two axes are orthogonal (cor = −0.19) and
  the potential axis is income-independent (R² = 0.07, ≤ 0.19 leave-one-out), so the 2-D
  structure is *not* reducible to GDP. Quadrants: Winners (DE, DK, AT, FR, SE, IT, ES, FI, PT, BE),
  **Exposed but capable (CZ, NL, PL, SI)** — the polarization tension — At risk (BG, RO,
  Baltics, HR, SK, EL, CY, HU), Low-stakes (LU, IE, MT). Note NL and BE sit *exactly* on
  the vulnerability and potential medians respectively: they are borderline, and under the
  opposite median-tie convention NL is a Winner and BE is Low-stakes (see `07_robustness.R`).
- **Externally valid** — potential predicts renewable share (partial +0.43 net of GDP,
  Eurostat SHARES overall) and OECD EPS policy stringency (+0.42, but only on the 20 OECD-
  member EU states — 6 of the 9 "At risk" countries are missing, so this validator cannot
  speak to the low-potential tail). Comparison with the Gräbner et al. (2020) growth models
  is close (Cramér's V = 0.71, a descriptive association index; n=27 is too sparse for
  chi-square, so significance is from a Monte-Carlo Fisher test) and supports H1–H3 (the
  catch-up "Workbench" group is significantly more vulnerable, +1.60 p<0.001, and less
  capable, −1.03 p<0.05).
- **Robust** — GCI is stable year-to-year (rank corr ≥ 0.97 vs pooled, mean 0.98), and
  shifting the whole reference window to 2013–2017 or 2015–2019 (complexity re-pooled from
  the Atlas, indicators re-averaged) changes **no quadrant at all** (rank corr vs baseline
  ≥ 0.99) — the window choice is innocuous. The typology also survives PCA-vs-mean,
  renewable-only GCI, variable drops, RCA-threshold changes (2.5–10bn, zero changes), and
  outlier removal (drop LU/MT: 2/26 change). The only sensitive specification is robust
  (median/MAD) rescaling of the potential axis (rank corr 0.51), which reflects green-patent
  skew, not the green signal (renewable-only GCI corr 0.96). Discrete cluster structure is
  weak (silhouette ≈ 0.28, gap k=1), which supports the continuous map over hard clusters.

Key figure: `plots/typology_map.png`. Scores: `data/tidy/taxonomy_scores.csv`.

## Verification status

The pipeline was independently re-verified after the 2026-07-14 audit fixes: every committed
`data/tidy/*.csv` regenerates **byte-identically** from the committed inputs on a clean
checkout (including the full Atlas → cache → complexity path), the complexity implementation
was cross-checked against the method of reflections (agreement to machine precision, and the
recovered PCI satisfies the product-side eigen relation exactly), the green list (244 unique
HS92 codes, 52 renewable) is fully present in the Atlas data, and the external validators
enter neither block (no circularity). Every number quoted above matches the re-run exactly.

## What is open

- **Phase 7 — the paper.** The analysis is complete; the writeup is not started. The
  legacy report has been archived; a fresh report should be built in `quarto/`.
- **Optional strengtheners** (non-gating): swap in Mealy & Teytelboym's authoritative
  293-code green list when the authors reply (we currently use the OECD CLEG, 244 codes);
  add an environmental-patents validator; the `is_renewable` flag is a provisional proxy
  (OECD "REP" medium) pending their 57-code renewable list.
- **Out of scope (by decision):** a brown-employment vulnerability variable (would need
  EXIOBASE sector employment) — the paper stands without it.

## Archive

`_archive/` holds superseded material kept for reference only: the old exploratory
clustering scripts, stale exploration plots, the rendered legacy report, an energy-balance
note, and testing SQL. Nothing in `_archive/` is part of the current pipeline.
