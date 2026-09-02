# Results summary — EU-27 green-transition typology (WP1)

*Last revised 2026-09-02. Numbers here must agree with `evidence/numbers.md`,
which is machine-generated; if they ever disagree, **numbers.md wins**.*

This is the substantive briefing for drafting the paper. It states what was
found, how confident we are, and where the argument is vulnerable. Open decisions
are in `open-questions.md`.

---

## 1. What the paper is

Work Package 1 of the OPUS project *"The green transition and economic
polarization in Europe"*. It builds a national typology of the EU-27 capturing
how well-placed each Member State is for the green transition, and asks whether
the transition risks **reinforcing existing core–periphery polarization**.

The design mirrors Fig. 1 of the project proposal: two *vulnerabilities*
(transition burden) and two *potentials* (green capability), placed on a
**vulnerability × potential map** with four quadrants.

The novelty is the **green economic complexity** dimension, computed per Mealy &
Teytelboym (2022) from Atlas HS92 6-digit export data on the global country set,
then extracted for the EU-27. This is an application of their method to a new
question, not a methodological contribution.

## 2. Design: four dimensions, two-part axes

Six indicators, two per dimension:

| Block | Twin sub-index (2 indicators → PC1) | Standalone |
|---|---|---|
| **Vulnerability** | emission intensity: carbon intensity + energy intensity, both per unit value added | fossil dependency (share of gross available energy) |
| **Potential** | green complexity: GCI + GCP | green innovation (EPO patent applications p.c.) |

Each axis is built in two steps: reduce the correlated twin pair to a single
sub-index (PC1), then combine that with the standalone at **equal weight**.

**Why not a flat PCA over all three variables per block.** The twins are strongly
correlated (carbon↔energy intensity r = 0.74; GCI↔GCP r = 0.76), so under a flat
PCA they dominate PC1 and the standalone loads only ~0.14–0.28 — effectively
ignored. The two-part construction treats them as co-equal *conceptual*
dimensions, so neither the twin pair (2 indicators) nor the standalone (1) wins
by count.

**The elegant consequence, and the answer to the "income-neutral?" objection.**
Within each block one part is income-linked and one is income-neutral, on
opposite diagonals: intensity and patents track GDP p.c.; fossil share and
complexity do not. Equal-weighting makes income **present but balanced in both
blocks** — R²(vuln ~ log GDP) = 0.27, R²(pot ~ log GDP) = 0.21. Income is not
purged (an earlier fully income-neutral specification was rejected: it
reclassified 18/27 countries and produced an undifferentiated map); it is simply
no longer the sole driver of either axis.

The twins are near-orthogonal to their standalone (intensity↔fossil −0.01,
complexity↔patents 0.19), confirming these are genuinely two dimensions per block
rather than double counting.

## 3. Data

| input | source | note |
|---|---|---|
| GHG emissions, value added | **EXIOBASE 3.10.2** (Zenodo 20051562) | computed in-repo; usable 2013–2022 |
| Energy balances, fossil share | Eurostat `nrg_bal_c`/`nrg_bal_s` | to 2023 |
| Green complexity (GCI, GCP) | Atlas HS92 6-digit + OECD CLEG green list (244 HS6 codes) | global country set, pooled over the window |
| Green patents | **PATSTAT, EPO applications** by filing year, Y02 + Y04S | |
| GDP, renewable share | WDI, Eurostat SHARES | |

**Reference window: 2017–2021** — the most recent window in which every input is
observed and complete. Two independent constraints converge on it: EXIOBASE
emissions are complete only to 2022 (2023–2024 are nowcasts whose dominant
stressor is identically zero), and patent applications only to 2021.

## 4. The map

| quadrant | n | countries |
|---|---:|---|
| **Winners** (low vuln / high pot) | 11 | Austria, Belgium, Denmark, Finland, France, Germany, Italy, Luxembourg, Slovenia, Spain, Sweden |
| **At risk** (high vuln / low pot) | 11 | Bulgaria, Croatia, Cyprus, Estonia, Greece, Hungary, Latvia, Lithuania, Malta, Romania, Slovakia |
| **Exposed but capable** | 3 | Czechia, Netherlands, Poland |
| **Low-stakes / low capability** | 2 | Ireland, Portugal |

Borderline (convention-sensitive): **Ireland, Latvia, Slovakia, Slovenia**.

Axes are distinct: cor(vulnerability, potential) = **−0.55** — related, but far
from ±1, so not one axis in disguise.

**Note the quadrants are unbalanced (11/11/3/2).** Because the axes correlate
−0.55, the off-diagonal cells are thin. Five countries carry the whole
"interesting" story and two of them sit in a Low-stakes cell that is closer to a
residual than a type. This is a real limitation, not a presentational one — see
`open-questions.md`.

## 5. The polarization result

Quadrant × growth model (Gräbner et al. 2020):

| quadrant | Core | Finance | Periphery | Workbench |
|---|---:|---:|---:|---:|
| Winners | **6** | 1 | 3 | 1 |
| Exposed | 0 | 1 | 0 | 2 |
| Low-stakes | 0 | 1 | 1 | 0 |
| At risk | 0 | 1 | 2 | **8** |

All six Core countries are Winners; 8 of 11 Workbench economies are At risk.
Cramér's V = 0.50 (bias-corrected 0.38), Monte-Carlo Fisher p = 0.003.
eta²(vulnerability ~ growth model) = 0.40, eta²(potential ~ growth model) = 0.41.

**The headline:** those least equipped for the transition also carry the largest
burden. The transition threatens to *reinforce* the core–periphery divide.

**But it is a refinement of the development model, not a relabelling.** V is
moderate, not 1, and the informative cases are the mismatches: Finance splits
(Luxembourg → Winners vs Malta → At risk, Ireland → Low-stakes); Periphery splits
(Italy, Spain, France → Winners vs Greece, Cyprus → At risk); within Workbench,
coal-heavy Poland and Czechia are *Exposed but capable* while Slovenia is a
Winner. **Lead with the mismatches, not the diagonal** — they are what income
cannot explain, and Luxembourg vs Ireland/Malta are all rich.

### Important caveat on how far this differentiates

Regressed on growth model with Core as reference, **the three non-Core groups are
statistically indistinguishable from one another**:

| vs Core | Finance | Periphery | Workbench |
|---|---:|---:|---:|
| potential | −1.5 (p=.009) | −1.4 (p=.008) | −1.6 (p=.001) |
| vulnerability | +1.1 (p=.047) | +1.1 (p=.035) | +1.6 (p=.0007) |

The pattern is **Core vs everyone else**, not a four-way gradient. A claim that
"different growth models face *different* transition challenges" (plural,
differentiated) is **not supported**. The narrow claim — the catch-up East is
both more vulnerable and less capable than the Core — is strongly supported.
This constrains the framing decision; see `open-questions.md` §1.

## 6. External validity

Validated against outcomes not used to build the scores, controlling for income:

| score | validator | raw r | partial r (net of log GDP p.c.) |
|---|---|---:|---:|
| potential | OECD EPS (policy stringency) | +0.56 | **+0.56** |
| potential | renewable share | +0.28 | **+0.40** |
| potential | GDP growth | −0.53 | −0.36 |
| vulnerability | renewable share | −0.53 | −0.57 |
| vulnerability | OECD EPS | −0.50 | −0.49 |
| vulnerability | GDP growth | +0.30 | +0.20 |

Expected signs hold and survive controlling for income.

**Circularity flag:** fossil dependency is ~half of vulnerability and fossil
share ≈ (1 − renewable share), so `vulnerability ~ renewable share` is **partly
mechanical** — report it as descriptive. Renewable share remains a clean
validator for *potential*, which contains no fossil term. EPS covers only 20/27
countries and misses much of the At-risk tail, so validation should be presented
modestly.

## 7. Robustness

The map is stable to every specification tested (quadrant changes out of 27):

| spec | cor vuln | cor pot | changes |
|---|---:|---:|---:|
| twin sub-index as mean not PCA | 1.00 | 1.00 | **0** |
| complexity: ECI replaces GCI | 1.00 | 0.92 | **0** |
| carbon: consumption-based accounting | 0.98 | 1.00 | **0** |
| patents: grants instead of applications | 1.00 | 1.00 | **0** |
| robust (median/MAD) scaling | 0.99 | 1.00 | 2 |
| part weights 2:1 | 0.95 | 0.98 | 2 |
| complexity: renewable-only GCI | 1.00 | 0.97 | 4 |
| fossil: production-based share | 0.70 | 1.00 | 4 |
| **flat blocks (single PCA)** | 0.71 | 0.90 | **8** |
| window shift 2016–2020 / 2018–2022 | 1.00 / 0.99 | 0.99 | 2 / 0 |

The map moves most under the two genuine *measurement* choices — flat aggregation
and the production-based fossil share — which is exactly why both are argued for
explicitly in the method rather than asserted.

Three robustness results deserve to be findings in their own right:

1. **Invariance to the MRIO release.** Rebuilding on EXIOBASE 3.10.2 rather than
   3.8.x — an 18% upward revision of EU production emissions and a 3.5% downward
   revision of value added, i.e. ~25% higher carbon intensity in level — moves
   **0 of 27** countries. MRIO choice is a standard referee question; this
   answers it.
2. **Invariance to the patent measure.** Applications and grants rank EU-27
   countries at Spearman 0.99 and swapping them moves **0/27** — even inside
   2017–2021, where grants are severely grant-lag truncated. The truncation is
   proportional across countries.
3. **Invariance across recent windows.** Every window from 2017 onward agrees
   perfectly (0/27 for 2019–2021, 2020–2022, 2018–2022). Only pre-2019 windows
   differ, and always by the same two borderline countries (Ireland, Slovakia).

Clustering diagnostics support the continuous map over hard clusters: silhouette
is low (~0.28 at k=3) and the gap statistic prefers k=1.

## 8. Burden vs. responsibility (the offshoring layer)

The vulnerability axis is **production-based** on purpose: it measures the
domestic *adjustment burden* — the plants, workers and energy system a country
must itself retool. Consumption-based accounting measures something different:
*responsibility*, whose final demand the emissions serve.

The finding is the **asymmetry between them**. In production terms Winners and
At-risk countries emit about the same per capita; in consumption terms it
reverses. The intensity gap narrows but the ordering never changes, which is why
the consumption-based robustness spec moves 0/27.

Net embodied imports p.c. is deliberately **not** an axis variable: it correlates
~+0.65 with log GDP p.c. and would reimport exactly the income confound the
per-value-added intensities remove.

### What the bilateral data refutes

The 49×49 origin×destination matrix settles a claim that was previously
untestable. **Only ~10% of the Core's embodied GHG imports originate in the
Workbench East; ~69% come from outside the EU entirely.** So *"the core's clean
profile rests on offshoring to the European East, and that transfer is the
mechanism of polarization"* **cannot be claimed**. The core offshores to the
world, not to its own periphery.

The direction still holds — the Workbench East is a net embodied-emission
exporter to every other EU bloc (~+70 Mt/yr, ~8–9% of its own production
emissions) — but that is an order of magnitude too small to be *the mechanism*.

**Framing to use:** state the intra-EU transfer as present, in the predicted
direction, and small. The stronger and defensible claim is that the Workbench
East's high burden is overwhelmingly its **own** carbon-intensive,
low-value-added production rather than production for the West — which makes the
polarization **structural rather than a transfer**, and arguably harder to fix.

On growth-model aggregates the production-vs-consumption gap is **binary, not a
gradient**: the three western blocs all consume ~22–28% more than they produce
and all produce at ~230–270 g CO₂e/€, while the Workbench consumes essentially
exactly what it produces and produces at ~2.5× the intensity. A four-way ordering
appears only under an unweighted mean over member countries, where Luxembourg and
Malta dominate a four-country Finance bloc — do not quote that version without
saying so. Note also that growth model adds only ~0.05 R² over log GDP p.c. alone
in explaining this gap, so it should not carry an "it's not just income" argument.

## 8b. Does the MRIO table choice matter? No.

A referee can ask whether the map is an artifact of EXIOBASE, since both
vulnerability intensities are ratios of EXIOBASE quantities. Two separate tests:

- **Release.** EXIOBASE 3.8.x → 3.10.2, an 18% upward revision of EU production
  emissions: **0/27**.
- **Table.** A completely independent MRIO — **EORA26** (different compiler,
  source data, 26 vs 200 sectors, 190 vs 49 regions) — on the same 2014–2017
  window, CO2 only on both sides: CO2-intensity Spearman **0.89** across
  countries, vulnerability-axis Spearman **0.98**, **0/27** quadrant changes.

Holding the window fixed at 2014–2017 is deliberate: it isolates the *table*
choice from the *window* choice. This is why `evidence/window_options.csv`
carries a 2014–2017 row.

**One caveat that must be reported if this is used.** The comparison runs on CO2
only, and that is forced rather than preferred. EORA26's GHG satellite block is
partly unusable in this release: every fluorinated gas carries ~51,000 Gg
(SF6 50931, NF3 51500, HFC23 51332 …), near-identical across gases *and* across
years — placeholder fill, not data. Real global SF6 is ~10 Gg; characterised at
AR4 GWPs those rows alone give ~8,000 Gt CO₂e, 160× the world total. N₂O is
inflated ~5×. Only CO₂ is credible (33.3 Gt against a real ~36). EXIOBASE was
therefore recomputed on a CO₂-only basket for the same years, so the two sides
differ by *table* and nothing else. That EORA cannot supply a usable multi-gas
footprint is itself worth a sentence: it is not a drop-in alternative.

## 8c. Country drill-downs: demand composition does not explain the map

The presentation feedback asked whether energy demand in the surprising countries
is driven by business or households. It was worth checking and the answer is
negative:

| | Winners | Exposed | Low-stakes | At risk |
|---|---:|---:|---:|---:|
| households % of final energy | 24.8 | 27.1 | 22.5 | 28.5 |
| business % | 42.6 | 45.7 | 41.5 | 37.9 |
| transport % | 31.8 | 26.9 | 35.5 | 33.1 |

cor(household share, vulnerability) = **+0.16**; business **−0.35**; industry
**−0.38**. The composition of demand is not what separates the quadrants — report
this as a negative result rather than omitting it.

It does supply the **Malta / Luxembourg vignette**:

- **Luxembourg** (Winners): produces 12.0 t CO₂e p.c. but *consumes* 26.1 —
  a ratio of **2.18×**, the highest in the EU. Only **13.9%** of its final energy
  goes to households, the lowest in the EU, with transport correspondingly
  inflated — the fuel-tourism artifact, worth naming explicitly.
- **Malta** (At risk): produces 5.7 t p.c., consumes 11.6 (**2.03×**), on a
  **96.6%** fossil energy mix.

Both look cleaner in production terms than they are in consumption terms, which
is the burden-vs-responsibility point at country level.

## 8d. The EPO-only restriction: the bias is real, and it does not bite

The patent variable counts EPO filings only (`appln_auth = 'EP'`). The worry was
that applicants in smaller and eastern member states file domestically instead,
so an EPO-only count would understate exactly the low-potential tail the
polarization finding rests on. Tested by re-running the query without the office
filter, returning both counts from one scan.

**The bias is real and systematic**, in precisely the predicted direction — EPO
share of all green filings, reference window:

| growth model | mean EPO share |
|---|---:|
| Workbench | **12.8%** |
| Periphery | 18.0% |
| Finance | 19.0% |
| Core | **21.2%** |

Extremes: Romania 3.9%, Czechia 8.5%, Poland 8.7%, Bulgaria and Slovakia 9.1%
against Denmark 27.4%, Slovenia 25.9%, Sweden and the Netherlands 22.4%. Core
applicants use the EPO at roughly **1.7×** the rate of Workbench applicants.

**But it does not change the map:**

| | |
|---|---:|
| Spearman(patents p.c., EPO vs all offices) | **0.967** |
| Spearman(potential axis) | **0.991** |
| **quadrant changes** | **0 / 27** |

So the restriction understates the periphery in *levels* without disturbing the
*ranking*, which is all the axis uses. This is the strongest form the answer
could take: the objection is conceded as real, quantified, and then shown not to
matter. Report it that way rather than dismissing the concern.

**Two nuances that complicate a clean East/West reading.** Slovenia has the
second-highest EPO share (25.9%), and Luxembourg (15.6%) and Ireland (16.2%) are
among the lowest despite being rich — plausibly because their applicants are
foreign-owned multinationals filing through other routes. The gradient tracks
firm structure as much as geography, so do not describe it as a simple
core-periphery effect.

**Keep EPO as the headline measure.** An EPO filing confers European-wide
protection, which is the concept the potential axis wants; all-offices counts mix
objects of different standing. The all-offices series is the robustness check,
not a better measure.

## 9. A correction that strengthens the story

The original PATSTAT query lacked `COUNT(DISTINCT ...)`, counting each
application once per matching CPC symbol × same-country applicant. Overall
inflation **1.67×**, and **differential**: Slovakia 2.40×, Greece 2.34×, Hungary
2.27×, Portugal 2.06×, Poland 1.94× against Belgium 1.55×, Denmark 1.54×, Finland
1.53×, Netherlands 1.49×.

It over-counted the eastern and southern periphery relative to the core — exactly
the low-potential tail — so earlier versions **understated** the core–periphery
green-innovation gap. The correction widens it. Worth stating explicitly: the fix
moves the result in the direction *less* convenient for a sceptic.

## 10. Caveats to keep visible

- **n = 27.** Everything is descriptive association — Cramér's V, partial
  correlations, Monte-Carlo Fisher. No large-sample inference. State this once,
  early, plainly.
- **Quadrants come from median splits**, so four countries near a median are
  convention-sensitive.
- **GCI ≈ green export diversity.** In this sample GCI correlates ~0.998 with a
  plain count of green products exported with RCA ≥ 1. Mealy & Teytelboym concede
  this in their fn. 9. Decide the honest wording — a referee will check.
- **Green list** is the OECD CLEG 244 codes, not Mealy & Teytelboym's 293.
  Renewable-only GCI correlates 0.96 with the full version, so the gap is
  footnote-sized.
- **Patents count EPO filings only** (`appln_auth = 'EP'`). This understates the
  eastern states in levels — measured, and real — but changes no country's
  quadrant. Resolved; see §8d.
- **EXIOBASE 2023–2024 are unusable** (dominant stressor identically zero), so no
  window may end after 2022.
