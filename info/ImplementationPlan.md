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
- [x] Pin package versions (renv) so results are reproducible for co-authors/referees.
      `renv.lock` records R 4.4.3 + 129 packages; restore with `renv::restore()`. No
      renv/activate.R hook (manifest only), so the project still runs against the system
      library. See `R/dependencies.R`.

## Phase 1 — Data acquisition & indicator construction

### 1a. Green complexity (Dimension 4) — the new build
- [x] Obtain **Atlas HS92 6-digit country–product–year** export data (full global country
      set, 1995–2024; 232 countries, 5039 products). At `data/raw/atlas_hs92_6d.csv`
      (gitignored; source in `atlas_hs92_6d_REFERENCE.txt`).
- [x] Obtain the **green HS6 code list**. Reconstructed transparently via
      `R/build_green_list.R`: extract the OECD CLEG (248 HS2007 codes, Table A.1 of
      `info/OECD-Report_List.pdf`) → convert HS2007→HS1992 (`info/HS 2007-to-HS1992 .xls`)
      → **244 unique HS92 green products** (52 flagged renewable via the REP medium, a
      provisional proxy). All 244 present in the Atlas data. Output
      `data/tidy/green_products_hs6.csv` (+ `green_products_cleg_hs2007.csv` provenance).
      NOTE: this is the OECD CLEG, close to but not identical with Mealy & Teytelboym's
      293-code union — swap in the authors' list when they reply for exact comparability.
- [x] Compute, following `GreenComplexity.pdf` §3.4–3.7, on the **global** country set
      (`R/functions/complexity.R`, driven by `R/02_complexity.R`; exports pooled over
      2014–2018): RCA (Balassa) → binary M → ECI/PCI (eigenvalue method; PCI recovered
      from the country-side eigenvector to avoid a P×P eigendecomposition) → **GCI** →
      proximity φ, density ω → **GCP**. Country filter: pooled exports ≥ USD 5bn.
- [x] Extract EU-27; write `data/tidy/green_complexity_eu.csv` (iso3, country, ECI, GCI,
      GCP, diversity). Folded into `taxonomy_indicators.csv` by `01_build_indicators.R`.
- [x] **Validate**: GCI top-10 reproduces Mealy & Teytelboym Fig. 3 (Germany #1, then
      Italy, Austria, Czechia, Denmark, China, USA, Japan, France, UK); ECI tails sensible
      (Switzerland/Korea/USA top; oil/resource exporters bottom); cor(GCI, ECI)=0.78.
      Substantive: GCI is orthogonal to income — catch-up East (CZE, POL, SVN, HUN) ranks
      above high-GDP finance/service economies (LUX, IRL, NLD).
      TODO (Phase 6): per-year averaging vs pooling; renewable-subset GCI; TradeWeave PCI
      cross-check; swap in authors' 293-code list.

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

## Phase 2 — Descriptive analysis (WP1 deliverable in its own right)  [DONE]

`R/03_descriptives.R`.
- [x] Ranked bar charts per indicator, coloured by growth-model group (`plots/descriptives_rankings.pdf`).
- [x] Correlation matrix (`plots/descriptives_correlations.pdf`, `indicator_correlations.csv`)
      + income-drivenness: R² of each indicator on log GDP p.c. (`indicator_income_r2.csv`).
      Key finding: GCI (0.01) and GCP (0.00) and fossil/renewable shares are essentially
      income-independent; value added (0.92), green patents (0.62) and per-capita energy
      (0.61) are income-driven — hence the vulnerability block uses per-value-added
      intensities, not per-capita levels (see Phase 3).

## Phase 3 — Dimensionality reduction (two block scores)  [DONE]

`R/04_typology.R`. Final blocks (per-capita levels were dropped from vulnerability after
the first PCA gave an incoherent, income-dominated PC1):

**Vulnerability block** — carbon intensity (GHG / VA), energy intensity (energy / VA),
fossil share of primary energy. PC1 = 61% var, all-positive loadings.
**Potential block** — green patents p.c., GCI, GCP. PC1 = 61% var, driven by GCI/GCP.

- [x] PCA within each block; PC1 scores oriented (fossil-share / GCI anchors).
- [x] **GO/NO-GO PASSED**: cor(vulnerability, potential) = −0.19 (orthogonal); potential
      R² vs log GDP p.c. = 0.07 (income-independent, ≤0.19 leave-one-out); vulnerability
      R² = 0.50 (correlates
      with income, but substantively — catch-up economies really are more carbon-intensive
      — and the axis is coherent). The 2-D structure is not reducible to income.

## Phase 4 — The typology

- [x] **Headline figure:** `R/04_typology.R` → `plots/typology_map.{pdf,png}`; quadrant
      membership + scores in `data/tidy/taxonomy_scores.csv`.
- [x] Quadrants (median splits): Winners = Austria, Belgium, Denmark, Finland, France,
      Germany, Italy, Portugal, Spain, Sweden; **Exposed but capable = Czechia,
      Netherlands, Poland, Slovenia** (the polarization tension — capable but carbon-locked);
      At risk = Bulgaria, Croatia, Cyprus, Estonia, Greece, Hungary, Latvia, Lithuania,
      Romania, Slovakia; Low-stakes = Ireland, Luxembourg, Malta. The Workbench group splits
      on *potential* while sharing high vulnerability. (Belgium and the Netherlands sit
      exactly on a median — borderline; see the tie-convention check in `07_robustness.R`.)
- [ ] **Robustness layer — clustering:** align `R/05_clustering.R` to cluster on the block
      variables / the two scores; validate cluster number with
      indicators (and separately on the two scores). Validate cluster number with
      silhouette + gap statistic (not just the agnes `ac` coefficient). Show dendrogram.
- [ ] Cross-tabulate clusters vs. quadrants vs. growth-model groups (alluvial). Do the
      data-driven clusters agree with the 2-D quadrants? Do they add info beyond GDP?

## Phase 5 — Validation & comparison with other classifications  [DONE]

`R/06_validation.R`.
- [x] External validity (validators NOT used to build scores; partial corr controls for
      log GDP p.c.): potential → renewable share of energy raw +0.24 / partial **+0.43**
      (green capability predicts renewables even more strongly net of income). potential →
      GDP growth −0.60 / −0.49 (β-convergence: capability sits in mature, slower-growing
      economies — a convergence confound, not a failure). Vulnerability → renewables weakly
      negative, as expected. **OECD EPS added** (`info/OECD-EPS-Index.csv`, aggregate EPS,
      20/27 EU states — 7 small/newer-EU missing, 6 of them "At risk", so read as
      indicative): potential → EPS +0.34 raw / **+0.42** partial; vulnerability → EPS −0.27 —
      green capability predicts stricter policy net of income. TODO: env. patents validator.
- [x] Comparison with Gräbner et al. (2020) growth models — **supports H1–H3**:
      group means → Core (vuln −0.71, pot +0.85), Periphery (−0.35, +0.10), Workbench
      (**+0.88, −0.18**), Finance (−0.83, −0.92). Regressions vs Core: Workbench
      vulnerability **+1.60 (p<0.001)**, potential **−1.03 (p<0.05)** — catch-up economies
      are systematically more exposed and less capable. Quadrant × growth model Cramér's
      V = **0.71** (bias-corrected 0.67, Monte-Carlo Fisher p<1e-5); quadrant × data-driven
      cluster (05) V = 0.64 (robustness). Figures:
      `plots/validation_scores_by_group.{pdf,png}`, `plots/validation_alluvial.{pdf,png}`.
- [x] The "Exposed but capable" quadrant (Poland, Czechia, Slovenia, NL) nuances
      the story: some catch-up economies have real green capability despite high burden.

## Phase 6 — Robustness & sensitivity

### Section A — self-contained (`R/07_robustness.R`)  [DONE]
- [x] Reference window / pooling: EU-27 GCI rank corr per-year vs pooled ≥ 0.97
      (mean 0.98). Pooling and window choice are innocuous.
- [x] Variable set (GCI vs ECI; renewable-only GCI; drop vulnerability vars) and
      PCA vs simple mean: Spearman corr with baseline ≥0.90 and ≤4/27 quadrant changes
      in every case **except two** — robust (median/MAD) scaling of the potential axis
      (corr 0.51, 8 changes) and swapping ECI for GCI on the potential axis
      (corr 0.77, 4 changes). The ECI swap is *expected* to move the ranking: it replaces
      the green signal with general economic complexity, so a lower rank correlation is the
      point — that it still yields only 4 quadrant changes is what shows the map is robust.
      Robust rescaling reflects green-patent skew (patents load low, 0.26; GCI/GCP drive
      PC1 — renewable-only GCI corr 0.96 confirms the green signal is stable).
- [x] Cluster number: silhouette peaks at k=3 but is low (~0.28) and the gap statistic
      gives k=1 — i.e. **weak discrete structure**, which *supports* the continuous 2-D
      map over hard clustering (clustering stays illustrative only).
- [x] Outlier sensitivity: dropping Luxembourg or Malta changes only 2/26 quadrants.
- [x] Median-tie convention: flipping `>=` to `>` moves exactly 2/27 countries (the two on
      a median); export threshold for the global RCA (2.5e9 / 1e10) is innocuous (cor_pot
      1.00, 0 quadrant changes). Both in `07_robustness.R`.
- [ ] Indicator-window shift (2013–2017 / 2015–2019) for the *full* typology — still open;
      needs re-pooling the Atlas over the wider window (the cached exports are 2014–2018).

### Section B — needs new data / decisions (optional, not gating)
- [x] OECD EPS added as an external validator (see Phase 5). Environmental patents still
      optional.
- [ ] Brown-employment dimension (EXIOBASE sector employment) — decision: currently OUT.
- [ ] Swap in Mealy & Teytelboym's 293-code green list when the authors reply.
- [ ] Per-year averaging of the *full* typology (not just complexity), if desired.

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
