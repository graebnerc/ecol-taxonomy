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

## One methodological caveat

The claim that a fully income-neutral specification "reclassified 18/27
countries" is **historical** — it was measured in July 2026 on the 2014–2018
window and has not been recomputed for the present specification. It is now
dated in the text rather than presented as current.
