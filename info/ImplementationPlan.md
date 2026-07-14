# WP1 Country Taxonomy — Implementation Plan

Working plan to turn the current clustering exercise into a publishable paper.
Companion to `CLAUDE.md` (repo map) and the proposal (`info/ProjectProposal.pdf`).

## 0. Guiding idea (the reframe)

The proposal's Figure 1 already gives the structure: two **vulnerability** dimensions
(energy inputs/mix, energy intensity) and two **potential** dimensions (green innovation,
green production capabilities). The paper should mirror that structure instead of pooling
all variables into one clustering.

**Headline design:** reduce each block to a single score with PCA →
place the EU-27 on a 2-D **vulnerability × potential map** → four policy-legible quadrants.
Clustering becomes a *robustness layer*, not the headline. The novelty that lifts this
above "we rediscovered GDP per capita" is the **green economic complexity** dimension
(Mealy & Teytelboym 2022, `info/GreenComplexity.pdf`).

**Core empirical question (H1–H3):** does the catching-up East / periphery
systematically land in the high-vulnerability / low-potential quadrant — i.e. does the
green transition threaten to reinforce existing core–periphery polarization?

**Target journals:** Ecological Economics, Structural Change and Economic Dynamics,
Environmental Innovation and Societal Transitions, or Journal of Economic Structures.

---

## Phase 0 — Repo hygiene & scaffolding

- [ ] Consolidate to **one** canonical pipeline; retire `clustering2.R` /
      `clustering-experiments.R` to an `R/experiments/` folder (keep for reference).
- [ ] Fix the stale column bug (`FossilShare`/`RenewShare` → `ShareFossils_PrimEnProd`/
      `ShareRenewables_PrimEnProd`).
- [ ] Adopt a functions-based structure: `R/01_build_indicators.R`,
      `R/02_complexity.R`, `R/03_analysis.R`, sourced helpers in `R/functions/`.
- [ ] Decide reference window (currently 2014–2018 mean). Keep, but document the choice
      and test sensitivity later.
- [ ] Pin package versions (renv) so results are reproducible for co-authors/referees.

## Phase 1 — Data acquisition & indicator construction

### 1a. Green complexity (Dimension 4) — the new build
- [ ] Obtain **Atlas HS92 6-digit country–product–year** export data (full global country
      set, 1995 onward). Store raw in `data/raw/` (gitignored).
- [x] Obtain the **green HS6 code list**. Reconstructed transparently via
      `R/build_green_list.R`: extract the OECD CLEG (248 HS2007 codes, Table A.1 of
      `info/OECD-Report_List.pdf`) → convert HS2007→HS1992 (`info/HS 2007-to-HS1992 .xls`)
      → **244 unique HS92 green products** (52 flagged renewable via the REP medium, a
      provisional proxy). All 244 present in the Atlas data. Output
      `data/tidy/green_products_hs6.csv` (+ `green_products_cleg_hs2007.csv` provenance).
      NOTE: this is the OECD CLEG, close to but not identical with Mealy & Teytelboym's
      293-code union — swap in the authors' list when they reply for exact comparability.
- [ ] Compute, following `GreenComplexity.pdf` §3.4–3.7, on the **global** country set:
  - RCA (Balassa), binary M matrix (RCA>1)
  - ECI, PCI (standardised)
  - **GCI** = Σ PCI of green products a country is competitive in
  - proximity φ, density ω → **GCP** (Green Complexity Potential) and GAP
- [ ] Extract EU-27 rows; average over the reference window. Output
      `data/tidy/green_complexity_eu.csv` (country, ECI, GCI, GCP).
- [ ] **Validate** ECI/PCI against Atlas's own published ECI and (optionally) TradeWeave
      PCI as an independent cross-check.

### 1b. Brown employment (optional vulnerability variable)
- [ ] Pull EXIOBASE sector employment + sector GHG (same source as existing footprints).
- [ ] Compute emission-intensity-weighted **brown employment share** per country
      (Σ brown-sector employment ÷ total employment). Output to tidy CSV.
- [ ] Fallback if EXIOBASE sector employment is awkward: list-based NACE brown flag
      (B05–09, C19, C23–24, fossil D35) on Eurostat `nama_10_a64`.

### 1c. Assemble the indicator matrix
- [ ] Build one wide table `data/tidy/taxonomy_indicators.csv`: 1 row per EU-27 country,
      columns grouped by block (see Phase 3). All per-capita / share / intensity
      transforms done here, explicitly, with a data dictionary.

## Phase 2 — Descriptive analysis (WP1 deliverable in its own right)

- [ ] Dimension-by-dimension descriptives: ranked bar charts + summary tables for each
      indicator, EU-27, coloured by growth-model group (JEE) for orientation.
- [ ] Correlation matrix of all indicators; flag redundant/collinear variables and the
      degree to which each is driven by economic scale (regress on GDP p.c., inspect).
- [ ] Short narrative per dimension — this alone answers the "descriptive statistics"
      requirement of the WP.

## Phase 3 — Dimensionality reduction (two block scores)

Candidate variables (finalise after Phase 2 correlations):

**Vulnerability block**
- production-based GHG per capita
- energy intensity (final energy / value added or GDP)
- fossil share of primary energy / gross available energy
- externalised emissions (net embodied-GWP imports p.c.) — *watch the sign/interpretation*
- (optional) brown employment share

**Potential block**
- green patents per capita
- Green Complexity Index (GCI)
- Green Complexity Potential (GCP)

- [ ] Standardise; run **PCA within each block** separately.
- [ ] Report loadings + variance explained; confirm PC1 of each block is interpretable
      and correctly signed (higher = more vulnerable / more potential). Flip signs if needed.
- [ ] Extract `vulnerability_score` and `potential_score` (PC1 of each block).
- [ ] Sanity check: are the two scores roughly independent of each other, and not both
      just proxies for GDP p.c.? (This is the test of whether the paper says something new.)

## Phase 4 — The typology

- [ ] **Headline figure:** scatter of EU-27 on vulnerability (x) × potential (y), median
      splits → four quadrants, points labelled + coloured by growth-model group.
- [ ] Narrate the quadrants: winners (low vuln / high pot), at-risk (high vuln / low pot),
      and the two mixed types. This is the taxonomy.
- [ ] **Robustness layer — clustering:** Ward hierarchical clustering on the standardised
      indicators (and separately on the two scores). Validate cluster number with
      silhouette + gap statistic (not just the agnes `ac` coefficient). Show dendrogram.
- [ ] Cross-tabulate clusters vs. quadrants vs. growth-model groups (alluvial). Do the
      data-driven clusters agree with the 2-D quadrants? Do they add info beyond GDP?

## Phase 5 — Validation & external relevance (WP1 "relation to other indicators")

- [ ] Regress / correlate the scores against external indicators the taxonomy should
      predict if it is meaningful: CO2 p.c., environmental patents, OECD EPS policy
      stringency, GDP growth, employment — controlling for GDP p.c. (mirrors the
      GreenComplexity.pdf validation strategy).
- [ ] Explicitly test H1–H3: is high-vulnerability/low-potential membership predicted by
      catch-up / Eastern / lower-GDP status?

## Phase 6 — Robustness & sensitivity

- [ ] Reference window (single year vs 2014–2018 vs later years).
- [ ] Variable set (drop/add brown employment; GCI vs ECI; with/without externalisation).
- [ ] Normalisation choice (per capita vs per VA; standardised vs robust-scaled).
- [ ] Cluster count k and linkage; Luxembourg / small-state outlier sensitivity.
- [ ] PCA vs simple standardised averages for the block scores.

## Phase 7 — Writing

- [ ] Draft structure: Intro → Framework (Fig 1 → two blocks) → Data → Methods → Results
      (descriptives → PCA scores → 2-D typology → clustering robustness → validation) →
      Discussion (polarization / H1–H3) → Policy implications → Conclusion.
- [ ] Rebuild `quarto/CountryTaxonomy.qmd` around the new pipeline; fix placeholder titles
      ("Titel", "student curricula across several semesters").
- [ ] Position against Gräbner et al. (2020) growth models and Mealy & Teytelboym (2022).

---

## Key risks / decisions to watch

1. **Scale dominance.** If block scores still track GDP p.c. 1:1, the paper adds little —
   Phase 3 sanity check is the go/no-go moment. Mitigation: the potential block (GCI/GCP)
   and vulnerability block are conceptually orthogonal to income; verify empirically.
2. **PCI is a global property.** Complexity must be estimated on all countries, then EU-27
   extracted — don't compute ECI/PCI on 27 countries only.
3. **Green list vintage.** Must be HS1992 6-digit to match Atlas HS92; concordance drift if
   a different HS vintage is used.
4. **n = 27, small.** Prefer the 2-D map + few robust clusters over many fine clusters;
   keep claims modest.
5. **Externalisation sign.** Net embodied-emission imports can read as "cleaner domestic
   production" *or* "outsourced dirt" — fix and document the interpretation before PCA.
6. **Brown employment is optional** — cut it cleanly if EXIOBASE sector employment proves
   costly; the core paper stands without it.
