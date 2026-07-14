# Independent audit — WP1 country-taxonomy pipeline

**Date:** 2026-07-14 · **Scope:** `R/build_green_list.R`, `R/01`–`R/07`, `R/functions/`,
committed data in `data/tidy/`, claims in `README.md` and `info/ImplementationPlan.md`.
**Method:** full pipeline re-run (`02→01→03→04→05→06→07`, using the local Atlas file and
caches), byte-level comparison against committed outputs, independent re-derivation of the
complexity mathematics, independent re-extraction of the green list, leave-one-out (LOO)
influence analysis on every headline statistic, and targeted data forensics.

---

## 1. Executive summary

The pipeline is fundamentally sound. The complexity implementation is **provably correct**
(the PCI shortcut is exactly equivalent to the product-side eigendecomposition; all of
Mealy & Teytelboym's eqs. 1–9 are implemented faithfully), the green list survives
independent re-extraction code-for-code, every headline number in the README reproduces
exactly, and the main output CSVs regenerate byte-identically.

Against that clean core stand:

- **1 data bug** that feeds the validation script a variable that does not exist as
  documented (Finding B1);
- **1 artefact** that puts 2 of the 5 members of the flagship "Exposed but capable"
  quadrant there by tie-breaking convention alone (A1);
- **2 misreported or overclaimed statements** in the prose (C1, C2);
- **several reproducibility gaps** (two inputs with no generating script, stale committed
  outputs, manually exported PNGs) (R1–R3);
- **2 validity caveats** for the paper (EPS range restriction, Cramér's V inference) (M1, M2).

None of these overturn the substantive conclusions — the LOO analysis shows the headline
results are not driven by single countries — but B1, A1, C1 and C2 must be fixed before
the paper is drafted, because each is the kind of thing a referee finds in an afternoon.

---

## 2. Reproduction status

### Reproduced exactly (re-run, committed data + local Atlas/caches)

| Claim (README / plan) | Re-run result |
|---|---|
| GCI top-10: DEU, ITA, AUT, CZE, DNK, CHN, USA, JPN, FRA, GBR | identical |
| cor(GCI, ECI) = 0.78 | 0.777 |
| Go/no-go: cor(vuln, pot) = −0.17; R² vuln = 0.50; R² pot = 0.05 | −0.17 / 0.50 / 0.05 |
| Quadrant membership (all 27 countries) | identical |
| Renewables partial +0.44; EPS +0.39; growth −0.46 | +0.439 / +0.394 / −0.464 |
| Workbench vuln +1.60 (p<0.001); potential −0.96 (p<0.05) | +1.60 (p=0.00013) / −0.96 (p=0.045) |
| Cramér's V 0.65 (growth model), 0.59 (cluster) | 0.65 / 0.59 |
| Per-year GCI rank corr 0.97–1.00, mean 0.99 | 0.973–0.996, mean 0.987 |
| Robust scaling: cor_pot 0.45, 10 quadrant changes | identical |
| Silhouette ≈ 0.28 at k=3; gap k=1 | 0.282 / k=1 |
| Outlier drops (LU, MT): 2/26 changes | identical |

All of `taxonomy_indicators.csv`, `taxonomy_scores.csv`, `green_complexity_eu.csv`,
`cluster_membership.csv`, `validation_*.csv`, `robustness_specs.csv` regenerate
**byte-identically**.

### Not reproduced / not reproducible

- `data/tidy/indicator_income_r2.csv` and `indicator_correlations.csv` are **stale**
  (see R2).
- `data/tidy/new_data.csv` and `data/raw/exports_by_year_1418.rds` have **no generating
  script** in the repo (B1, R1).
- The raw rebuild (`get_data.R`) depends on an external EXIOBASE Python script and a live
  PATSTAT connection; not auditable from here.

---

## 3. Findings

Tags: **Bug** · **Artefact** · **Methodological concern** · **Reproducibility** · **OK-noting**.

### B1 · Bug — the "renewable share" validator is a blend of four different indicators

**Where:** `data/tidy/new_data.csv`; consumed at `R/06_validation.R:30–39` and joined (harmlessly) at `R/functions/indicators.R:19–22`.

`new_data.csv` contains each country-year **four times** (2,538 rows; 1,620 duplicates).
`GDP_ppp`, `GDP_real` and `ShareFossils_GrossAvEn` are identical across the four copies,
but `ShareRenewables_GrossAvEn` **differs** — AUT 2015: 33.50 / 71.49 / 33.23 / 11.41.
That pattern is unmistakably the Eurostat SHARES dataset (renewable share **overall /
electricity / heating&cooling / transport**) collapsed with its identifying column dropped.

`06_validation.R:35` computes `renew_share = mean(ShareRenewables_GrossAvEn)` over all
rows — a mean of four heterogeneous series, not "renewable share of gross available
energy" as the code and README describe.

**Impact (quantified):** partial correlation of potential with the blend = **+0.44**; with
the true overall share (row 1 of each group) also **+0.44** — a lucky coincidence; the four
sub-series individually give +0.44 / +0.39 / +0.44 / +0.13. The conclusion survives, but
the published number would describe a nonexistent variable. The `01` join quadruples rows
too, but every column it uses is constant within duplicate groups, so
`taxonomy_indicators.csv` is numerically unaffected (verified byte-identical).

**Aggravating factor:** no script creates `new_data.csv` (it entered in commit `4bfa769`
"work during flensburg meeting"); its provenance cannot be audited.

### A1 · Artefact — median-split tie convention decides flagship-quadrant membership

**Where:** `R/functions/typology.R:41–43`; duplicated inline at `R/04_typology.R:54–58`.

With n=27 (odd), one country sits **exactly at** each median, and the code's `>=` branch
assigns it to the high side. Those countries are the **Netherlands** (vulnerability
median) and **Hungary** (potential median) — and both are thereby placed in "Exposed but
capable". Under the opposite convention, NL is a **Winner** and HU is **At risk**: 2 of
the 5 members of the quadrant the README calls "the polarization tension" are there by
convention alone. Neighbouring scores are also close (14th vs 15th vulnerability:
−0.144 vs +0.074), so the split region is dense.

### C1 · Bug (misreported claim) — robustness prose contradicts its own table

**Where:** `info/ImplementationPlan.md:154–156` vs `data/tidy/robustness_specs.csv`.

The plan claims "Spearman corr with baseline ≥ 0.90 and ≤ 4/27 quadrant changes in every
case except robust … scaling". The table itself shows **"potential: ECI replaces GCI" has
cor_pot = 0.72** (4 quadrant changes). The README's list of survived checks quietly omits
the ECI swap. A referee reading the CSV will catch this.

### C2 · Methodological concern — "all income-neutral" overclaims

**Where:** `README.md:23–24` ("carbon intensity …, energy intensity …, fossil share — all
income-neutral") vs the pipeline's own `indicator_income_r2.csv`.

The regenerated descriptives show EnergyIntensity_normed R² = **0.51** and
CarbonIntensity_normed R² = **0.46** vs log GDP p.c.; the vulnerability score itself has
R² = 0.50. The plan (`ImplementationPlan.md:103–105`) discloses and argues this; the
README's "income-neutral (by construction)" framing is not supported. Related fragility:
the potential axis's R² = 0.05 rises to **0.17** when Luxembourg is dropped (LOO range
0.01–0.17; the go/no-go verdict survives, but quote a range, not a point).

### R1 · Reproducibility — `07_robustness.R` reads a cache nothing creates

**Where:** `R/07_robustness.R:38` reads `data/raw/exports_by_year_1418.rds` (gitignored).

No script writes this file; it was made interactively. A fresh clone plus the Atlas file
**cannot run robustness §1** (the per-year GCI stability claim). Verified mitigation: the
cache is internally consistent — summing it over years reproduces `pooled_exports_1418.rds`
exactly — so the 0.97–1.00 numbers are sound; only the generating code is missing.

### R2 · Reproducibility — stale committed outputs; hand-made PNGs

- Committed `indicator_income_r2.csv` / `indicator_correlations.csv` **lack the
  CarbonIntensity/EnergyIntensity rows**: they predate the current indicator table and
  change on re-run.
- `plots/typology_map.png` (the README's "key figure") and both `validation_*.png` are
  written by **no script** — `04`/`06` save only PDFs; the PNGs are manual exports that
  can silently go stale.

### R3 · Reproducibility — no package pinning

Plan Phase 0 ("pin package versions with renv") is still unchecked. All results were
reproduced under the current local library; referee-proof reproduction needs a lockfile.

### M1 · Methodological concern — EPS validation sample excludes the "At risk" group

**Where:** `R/06_validation.R:44–48`.

The 7 EU countries missing from the OECD EPS are BG, HR, CY, LV, LT, RO, MT — i.e. **6 of
the 9 "At risk" countries**. The +0.39 partial correlation is estimated under severe range
restriction on exactly the taxonomy's low-potential tail. The code comment and plan say
"indicative"; `README.md:63` reports +0.39 unqualified. (LOO on the 20 available countries
is stable, 0.31–0.46, but LOO cannot address missingness.)

### M2 · Methodological concern — Cramér's V has no inferential validity here

**Where:** `R/06_validation.R:89–95`.

Quadrant × growth model is a 4×4 table with n=27: **100 % of expected cell counts are
below 5** (min 0.59), the `chisq.test` warning is suppressed, and no small-sample bias
correction is applied (uncorrected V is biased upward in sparse tables). V = 0.65 is fine
as a *descriptive* index; it cannot carry significance-flavoured language.

### N1 · OK-noting — GCI is, in this data, green diversity

cor(GCI, count of green products with RCA ≥ 1) = **0.998**: the PCI weighting contributes
essentially nothing. Mealy & Teytelboym say the same (their footnote 9). Temper the
"technologically sophisticated green capabilities" interpretation, and soften
`README.md:52` — "GCI top-10 **reproduces** Mealy Fig. 3" is loose (Mealy's 2014 order is
DEU, ITA, **USA**, AUT, DNK; here USA is 7th, CZE 4th; different list, 244 vs 293 codes,
and a pooled window). Say "consistent with".

### N2 · OK-noting — Atlas junk codes and missing country filters

`02_complexity.R` keeps `999999` and `XXXXXX` ("unspecified" trade, together ~4.8 % of
world exports) as products. Quantified: removing them changes ECI/GCI/GCP by cor ≈ 0.999
and neither is a PCI extreme (ranks 1986 / 187 of 5011) — negligible, but exclude them on
principle. There is also no population/data-quality filter (Hausmann standard: pop ≥
~1.25 M), so re-export hubs enter the global estimation (Singapore is #6 in ECI); this
touches EU-27 results only via the global PCI, i.e. weakly.

### N3 · OK-noting — smaller items

- `functions/complexity.R:40` uses RCA **≥ 1**; the paper says **> 1**. Measure-zero in
  continuous data; align the docstring.
- `get_data.R:190` converts missing PATSTAT counts to 0. Ten EU country-years in the
  window are affected (BG, CY, EE, HR, LT, LV, MT, RO) — almost certainly true zeros from
  the SQL `GROUP BY`, but the assumption is undocumented.
- `06_validation.R:36` computes GDP growth with `first()/last()` — row-order-dependent;
  currently safe (file is year-sorted) but fragile.
- `05_clustering.R:1` header still says "03 - Typology analysis"; its final message says
  "03_analysis.R done"; K=4 is hard-coded while `07` finds silhouette k=3 / gap k=1
  (tolerable only while clustering stays illustrative).
- `04_typology.R:54–58` re-implements quadrant assignment inline instead of calling
  `assign_quadrant()` — consistent today, a drift risk.
- The 5e9 export threshold and the 2014–2018 indicator window (beyond complexity) are
  never varied in `07`.
- Window means are means-of-yearly-ratios, not ratios of window sums — fine, but state it.

---

## 4. Verified clean (attacked and held)

- **ECI/PCI mathematics.** The PCI-from-country-side shortcut is exactly the product-side
  second eigenvector: algebraically (if M̃꜀k = λk then k_p = U⁻¹Mᵀk satisfies
  M̃ₚk_p = λk_p, sharing the non-zero spectrum) and numerically on the real M
  (residual 7.7 × 10⁻¹⁶; spectrum real because M̃ₚ is similar to a symmetric matrix;
  λ₂ = 0.276 vs λ₃ = 0.209, well separated → unique eigenvector). Balassa broadcasting is
  correct on both axes; sign conventions consistent (ECI ↑ diversity, PCI tied to it);
  the trim loops cannot over-prune or loop forever (empty-matrix case exits) and dropped
  nothing here. ECI ranks JPN/CHE/DEU/KOR top and Angola/Chad/South Sudan bottom.
- **GCI/GCP formulas** match Mealy eqs. 6–9 exactly, including proximity
  min-conditional ≡ C_ig/max(u_i,u_g), density with self-proximity (as in Hidalgo 2007),
  GCP as the **mean** over not-held green products (eq. 9), and PCI [0,1]-normalisation
  (affine-invariant to the prior z-scoring). No division-by-zero exposure: min not-held
  green products = 87.
- **Green list.** Independent text-based re-extraction of the PDF yields exactly the same
  248 HS2007 codes (no parsing drift, no duplicates hidden by `distinct()`); all medium
  tokens clean; concordance direction confirmed from the sheet's own header, one-to-one
  for all 248 codes, leading-zero padding correct; spot checks right (850231 → 850230).
  All 244 HS92 codes present in the Atlas **and** in the post-filter M. The 244-vs-293
  gap and the provisional `is_renewable` REP proxy are disclosed in README, plan and code.
- **Indicators.** 27 countries, zero NAs; denominators, population×1000 and the
  patents-per-million transform correct; the `new_data` join duplication does not leak
  into any indicator (verified).
- **No circularity.** Validators (SHARES renewables, growth, EPS) are not score inputs;
  the vulnerability-block fossil share correlates only −0.20 with the renewables validator.
- **Determinism & influence.** Only stochastic step (gap statistic) is seeded; end-to-end
  re-run is byte-identical. LOO: cor(vuln, pot) ∈ [−0.29, −0.09]; renewables partial
  ∈ [0.39, 0.48]; EPS partial ∈ [0.31, 0.46]; Workbench coefficient ∈ [1.43, 1.68] — no
  headline result is driven by one or two countries.

---

## 5. Remediation plan (prioritised)

### P0 — before any paper text is drafted

1. **Rebuild `new_data.csv` from source, with a script (fixes B1).**
   Add a section to `R/get_data.R` (or a new `R/get_data_extra.R`) that downloads
   Eurostat SHARES (`nrg_ind_ren`) *keeping the indicator dimension*, plus GDP
   (`GDP_ppp`, `GDP_real`, e.g. WDI `NY.GDP.MKTP.PP.KD` / `NY.GDP.MKTP.KD`), and writes a
   **deduplicated** panel with one row per country-year and an explicit
   `renew_share_overall` column. Then:
   - `06_validation.R:35`: use `renew_share_overall` only;
   - `functions/indicators.R:19`: add a guard, e.g.
     `stopifnot(!anyDuplicated(extra_data[, c("iso3c","year")]))`;
   - re-run `01`–`07` and re-check the +0.44 (expect ≈ unchanged; the overall share alone
     already gives +0.44).

2. **Make the median-split tie explicit (fixes A1).**
   Cheapest honest fix: keep the convention, but (a) state in the paper that NL and HU sit
   exactly at the medians and are boundary cases; (b) add a robustness line to `07` that
   flips the tie convention (`<=`/`>`) and reports the two membership changes; (c) mark
   boundary countries visually on `typology_map` (e.g. hollow points within ε of a median).
   Alternative: report quadrant membership with a "borderline" band (e.g. |score − median|
   < 0.1 → labelled boundary) instead of pretending a hard partition.
   Also: make `04_typology.R` call `assign_quadrant()` instead of its inline copy.

3. **Correct the two prose claims (fixes C1, C2).**
   - `info/ImplementationPlan.md:154`: change to "≥ 0.90 in every case except robust
     scaling (0.45) and the ECI-for-GCI swap (0.72)" — and say why the ECI swap is *expected*
     to move the ranking (it replaces the green signal with general complexity; that it
     still yields only 4 quadrant changes is the actual robustness point).
   - `README.md:23–24`: replace "all income-neutral" with "intensity-based (per value
     added), which removes the mechanical scale effect; the vulnerability score still
     correlates with income (R² = 0.50), which we read as substantive (catch-up economies
     are more carbon-intensive), while the potential axis is income-independent
     (R² = 0.05, LOO ≤ 0.17)".

### P1 — before submission

4. **Close the reproducibility gaps (R1–R3).**
   - Add the by-year cache builder to `02_complexity.R` (~4 lines: when building from the
     Atlas, also `saveRDS(atlas[year in window, .(export=sum(export)), by=.(iso3,hs6,year)],
     "data/raw/exports_by_year_1418.rds")`), and a `stopifnot(file.exists(...))` with a
     pointer in `07`.
   - Re-run `03` and commit the refreshed `indicator_income_r2.csv` /
     `indicator_correlations.csv`.
   - Save PNGs alongside PDFs in `04` and `06` (one extra `ggsave()` each), so
     `plots/typology_map.png` is pipeline-produced.
   - `renv::init()` + commit the lockfile (plan Phase 0).

5. **Qualify the EPS result (M1).** In README and paper: "+0.39 on the 20 OECD-member EU
   states; 6 of the 9 'At risk' countries are missing, so this validator cannot speak to
   the low-potential tail." Optionally test sensitivity by imputing worst/best-case EPS
   ranks for the missing 7 to bound the correlation.

6. **Replace the chi-square behind Cramér's V (M2).** Keep V as description, but get the
   p-value from `fisher.test(tab, simulate.p.value = TRUE, B = 1e5)` or a permutation of
   quadrant labels; optionally report bias-corrected V (Bergsma 2013). Drop the suppressed
   warning.

7. **Product-set hygiene (N2).** In `02_complexity.R`, filter
   `!hs6 %in% c("999999", "XXXXXX")` (state it in the text); consider the standard
   population filter for the global set. Effects are pre-quantified as negligible
   (cor ≈ 0.999), so this is a one-line change plus one sentence.

### P2 — nice-to-have / cosmetic

8. Reword `README.md:52` "reproduces Mealy Fig. 3" → "consistent with"; add one sentence
   on GCI ≈ green diversity (cor 0.998, citing Mealy fn. 9) where GCI is interpreted (N1).
9. Align RCA docstring (≥ vs >); document the patents NA→0 assumption in `get_data.R`;
   replace `first()/last()` in `06` with an explicit `year`-based selection.
10. Fix the `05_clustering.R` header/message; either set K from the `07` diagnostics or
    add a comment that K=4 is illustrative and unsupported by silhouette/gap.
11. Add two cheap robustness lines to `07`: vary the export threshold (2.5e9 / 1e10) and
    shift the indicator window (2013–2017 / 2015–2019) for the *full* typology.

---

*All quantitative statements in this report were produced by re-running the committed
pipeline and by scratch verification scripts (complexity equivalence, junk-code ablation,
LOO influence, green-list re-extraction); the working tree was restored to the committed
state afterwards.*
