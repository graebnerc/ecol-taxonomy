# Verification log

Every quantitative claim in `results-summary.md` and `open-questions.md` was
recomputed against the committed pipeline outputs on **2026-09-03**. This file
records what the audit found, so the corrections are not silently absorbed.

## Errors found and fixed

| # | claim as written | actual | cause |
|---|---|---|---|
| 1 | flat-PCA standalone loads "~0.14–0.28" | **0.02** fossil, **0.30** patents | never rechecked after the window/measure change |
| 2 | net embodied imports ~ log GDP "+0.65" | **+0.77** | stale: 2014–2018 run |
| 3 | western blocs consume "22–28%" more, at "230–270 g/€" | **27–38%**, **232–240 g/€** | stale: pre-window-change run |
| 4 | Workbench net export "~+70 Mt, ~8–9%" | **+78.7 Mt, 9.3%** | rounded from a superseded run |
| 5 | consumption "reverses" the production ordering | it **widens**; no reversal | true of the old vintage, not this one |
| 6 | "~25% higher carbon intensity" after the MRIO change | **~23%** | arithmetic overstatement |
| 7 | renewable-only GCI correlates "0.96", gap "footnote-sized" | **0.89**, and it moves **4/27** | stale, and the conclusion drawn from it was wrong |
| 8 | GCI ~ green count "~0.998" | **0.997** global, **0.996** EU-27 | imprecise |

## Reproducibility gap found and closed

The **pairwise permutation p-values** in §5 were produced by an uncommitted
scratch script — a number in the pack that no committed code generated. Now
computed by `06_validation.R` and exported to
`evidence/validation_pairwise_tests.csv`.

While closing it, one value moved across the 0.05 threshold between Monte-Carlo
seeds (Periphery/potential: .053 → .049). Both are now reported as **marginal**,
with an explicit instruction not to lean on which side of .05 they fall.

## Claims checked and confirmed correct

Within-block correlations; R² on log GDP for both axes; cor(vulnerability,
potential); the borderline list; eta² by group; all within-group standard
deviations; group mean potentials; quadrant vs development-model coherence
(0.75 / 0.72); the within-quadrant component correlations; Bulgaria and Malta
component scores; the full external-validity table; the whole robustness table;
the offshoring shares (10.4% / 68.8%); the household/business energy split and
its three correlations; the Luxembourg and Malta vignette figures; all EPO-share
figures; the entire convergence table including skew and the log-scale
comparison; all quadrant-stability figures; the v1 double-counting inflation
(1.667× and every per-country ratio); the EXIOBASE revision ratios.

## Second pass (same day): "if it cannot be reproduced, remove it"

The first pass *dated* two figures rather than resolving them. That was the wrong
call — a number that cannot be reproduced in the current setting does not belong
in the pack. Both turned out to be reproducible, and both were wrong.

| claim as written | actual | note |
|---|---|---|
| income-neutral spec "reclassified 18/27" | **14/27** | reproducible after fixing a defect, see below |
| "silhouette ~0.28 at k = 3" | **0.33** at k = 3; max **0.39** at k = 7 | k = 3 was never the peak |

The silhouette conclusion survives (all values are far below the ~0.5 threshold
and the gap statistic prefers k = 1), but the numbers quoted were wrong and the
implication that k = 3 was the best candidate was unsupported.

### A defect found while reproducing the first one

`R/appendix_decomposed_map.R` predated the four-dimension restructure and still
scored **both** specifications with `block_score()` — a flat single PCA — while
`04_typology.R` had moved to `axis_score()`. Its "headline" map was therefore not
the headline: it reported quadrant totals of 4/10/9/4 against the real 11/3/2/11,
with Austria and Finland shown as *Exposed* rather than *Winners*. Every
reclassification count taken from that script was against the wrong baseline,
which is where "18/27" came from.

Fixed, and the script now **asserts** that its headline matches
`taxonomy_scores.csv` before comparing anything, so the baseline cannot drift
again.

## Third pass: sweep for the same defect class

Having found two scripts producing plausible-but-wrong output, every script was
checked for the same failure modes: wrong scoring baseline, hardcoded years
reaching a figure, hardcoded patent variable, and outputs inconsistent with the
headline.

**Found and fixed**

| script | defect | reached the pack? |
|---|---|---|
| `appendix_burden_responsibility.R` | figure axis label hardcoded "(2014-2018 mean)" while the window is 2017–2021 | **yes** — `burden_responsibility.png` is in `figures/` |
| `appendix_pc1_cos2.R` | header claimed "numbers match 04_typology.R" while computing a flat single PCA per block — the specification abandoned in July | no |
| `appendix_window_coverage.R` | subtitle described a window comparison that is no longer the live one | no |

**Checked and clean**

- `appendix_structure_map.R` — its "structured" side reproduces the headline
  exactly (11/3/2/11).
- All six quadrant-producing outputs match `taxonomy_scores.csv` country by
  country.
- `eora_crosscheck.csv` differs on exactly two countries (Ireland, Slovakia) —
  **correct**, because it runs on the 2014–2017 window by design, and
  `window_options.csv` independently reports those same two as the difference
  between that window and the headline.
- The files in `data/tidy/` older than the window change are all *source inputs*
  (Eurostat, PATSTAT v1, WDI, the superseded EXIOBASE extract), not stale outputs.
- No other hardcoded year or patent variable reaches a figure or a table.

**A useful by-product.** Correcting `appendix_pc1_cos2.R` produced a new argument
for the two-part design that was not previously available: the twin PC1 captures
83% (intensity) and 65% (complexity) of each country's own variation on average,
against 65% and 46% under the flat three-variable PCA. Added to
`results-summary.md` §2.

## Standing rule

A figure that cannot be reproduced from committed code in the current
specification is removed, not annotated. Re-run this audit after any change to
the window, the patent measure, or the data vintage — six of the eight errors in
the first pass came from exactly those.
