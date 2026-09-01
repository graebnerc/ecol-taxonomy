# Paper TODOs — reviewer feedback to fold in before/while writing

Working list of open items from presentation feedback, so nothing is lost when
the paper is drafted. **Keep this file in sync**: check off items as they are
handled, and add new implications as they come up. Status legend:
`[ ]` open · `[~]` in progress · `[x]` done.

Grounding numbers below come from a re-run of the current pipeline
(`taxonomy_indicators.csv`, 2014–2018 window) and are quoted so the prose can be
written without re-deriving them.

---

## 1. "Income-neutral" claim vs. vulnerability–income correlation (framing)

**Feedback:** I say the two components are income-neutral, but I also say
vulnerability correlates with income. Contradiction — reframe?

**Diagnosis (from the go/no-go check in `04_typology.R`):**

| Score | R² on log GDP p.c. | reading |
|-------|--------------------|---------|
| Potential | **0.07** | genuinely income-neutral |
| Vulnerability | **0.50** (r ≈ −0.71) | clearly income-linked |

The blanket "both are income-neutral" is false. The reviewer caught a real
inconsistency.

**Implications / TODO:**
- [ ] Reframe in prose: *potential* is income-neutral (by construction and
  empirically, R²=0.07); *vulnerability* is partly income-linked (R²=0.50), and
  that is itself a **finding** — the poorer East/South are more carbon- and
  energy-intensive per unit of value added. Sets up the polarization story
  (periphery is doubly disadvantaged: low potential + high, income-correlated
  vulnerability).
- [ ] Lean on the **partial correlations** in `06_validation.R` (net of log GDP
  p.c.) to show vulnerability carries signal *beyond* income — not a mere GDP
  proxy.
- [ ] Tie to item 3: much of vulnerability's income-loading comes from the
  *choice* of variables (both per-value-added). Decomposition (item 3) isolates
  a near-income-neutral component. Report the improved R² once decomposed spec
  is chosen.
- Status of code support: `04_typology.R` already prints per-axis R² on log GDP;
  `07_robustness.R` now also reports R²(vuln~logGDP) for baseline vs decomposed
  vulnerability (see item 3).

## 2. Country drill-downs after the map (e.g. Malta) — descriptives

**Feedback:** After the map, discuss selected surprising countries (Malta
surprised many); show descriptives, e.g. whether energy demand is driven by
business vs. households.

**Data availability:** Eurostat `nrg_bal_c` already carries final consumption
split by **households / industry / transport / services** — no new download.

**Caveats worth a narrative hook:**
- Malta's `ShareFossils_normed = 0` is a **data artifact**: Malta produces almost
  no primary energy, so "fossil share of *primary production*" is degenerate.
- Malta's low apparent vulnerability is propped up by very high net embodied-
  emission imports p.c. (`GWP_trade_normed` ≈ 9,200, second only to Luxembourg):
  it looks clean partly because it offshores emissions.

**Implications / TODO:**
- [x] Production- vs. consumption-based emissions for the flagged countries →
  **done** in `R/appendix_burden_responsibility.R` (see § Offshoring below);
  Malta's ratio is 3.11, Luxembourg's 1.91.
- [ ] Still open: the *sectoral energy split* (household vs. business, available
  in Eurostat `nrg_bal_c` without a new download) for the same countries.
- [x] Expose/replace the "ShareFossils of *primary production*" weakness →
  **done**: substituted for the demand-side gross-available-energy share. Full
  comparison and justification logged in **§ Fossil-share measure** below.
- [ ] Write the Malta / Luxembourg offshoring vignette (numbers now in
  `data/tidy/burden_responsibility.csv`).

## Fossil-share measure: substitution decision (LOGGED, implemented)

**Question raised:** replace production-based `ShareFossils_PrimEnProd`? With what?
Candidates compared: (a) fossil share of primary **production** (old), (b) fossil
share of **gross available energy** (demand), (c) **import dependency**
(net imports / gross available energy), (d) a **combination** of (a)+(c).

### Comparison (EU-27, 2014–2018 window)

Diagnostics — circularity = corr with the renewable-share validator; income =
corr with log GDP p.c.:

| measure | cor(renew) | cor(logGDP) | verdict |
|---------|-----------|-------------|---------|
| (a) fossil_prod (old) | −0.22 | −0.12 | low circularity/income **but artifact-ridden** |
| (b) **fossil_gross** | −0.72 | **+0.02** | on-concept, income-neutral; circularity intrinsic |
| (c) import_dep | −0.51 | +0.24 | measures *security*, not carbon lock-in |
| (d) prod+import_dep combo | −0.67 | +0.11 | **fails — see below** |

The three measure *different* vulnerabilities (Denmark is the tell: fossil
production 0.73, fossil consumption 0.69, import dependency 0.15 = self-sufficient):
prod = stranded-**production** risk · gross = carbon lock-in of **consumption** ·
import_dep = **energy-security**. They are only weakly related (pairwise 0.34 /
−0.43 / 0.38).

### Why NOT combine prod + import_dep (tested)

- Still circular: combo cor(renew) = −0.67 (barely better than fossil_gross).
- Only half-recovers the concept: `lm(fossil_gross ~ fossil_prod + import_dep)`
  R² = 0.44; `cor(combo, fossil_gross)` = +0.66.
- **Cancellation trap:** prod and import_dep are *substitutes* (make-vs-buy energy,
  r = −0.43). A country that produces *and burns its own* fossil scores low on
  both → the average calls it clean. **Estonia** (oil shale, 86% fossil
  consumption) ranks **#1 cleanest** on the combo vs #21 on fossil_gross; Malta
  ranks #22 vs #27. Both proxies share the same blind spot, so combining compounds
  the error. Comprehensiveness must come from *separate dimensions*, not one merged
  index.

### Why fossil_gross addresses the issues

1. **Artifacts (item 2):** measures fossil share of what a country *consumes*, so
   non-producers are no longer 0. Malta 0.00→0.96, Luxembourg 0.00→0.83, Ireland
   0.37→0.91, Baltics/Slovakia ~0.05→0.62–0.67, oil-shale Estonia →0.86. Full
   EU-27 coverage (Eurostat gross available energy, `ShareFossils_GrossAvEn`).
2. **Income-neutrality (item 1):** cor(logGDP) = **+0.02** (vs −0.12 for prod), the
   best of all candidates — it captures fuel mix, not economic development level.
3. **Cancellation trap:** none — it is a *direct* measure of the concept, not an
   average of substitutes, so it ranks self-sufficient dirty economies correctly
   (Estonia → #21 vulnerable, not #1).
4. **Concept fit:** it is exactly the proposal's Fig. 1 "current inputs to
   production / energy sources & mix" vulnerability.

The one cost — circularity with the renewable validator (−0.72 at variable level)
— **does not materialise in the headline score**: fossil share loads only ~0.14 in
the vulnerability PC1, so `cor(vulnerability score, renew_share) = −0.04`. The
validator stays legitimate. It would only bite under a spec that gives fossil share
high weight (the decomposed spec) — see below.

### Implementation (done)

- [x] `R/functions/indicators.R`: `ShareFossils_normed` now = `ShareFossils_GrossAvEn/100`
  (demand-side); old production share kept as context var `ShareFossilsProd_normed`.
- [x] Rebuilt `taxonomy_indicators.csv`; re-ran `04`, `06`, `07`.
- [x] `07_robustness.R`: added spec "production-based fossil share" for comparison.
- [x] `06_validation.R`: documented the (currently negligible) circularity.
- **Effect on headline map:** only **2/27** countries move vs the old
  production-based map (Finland Winners→Exposed, Netherlands Exposed→Winners; both
  were already flagged borderline). The map is robust; the swap mainly fixes the
  artifacts and makes fossil share meaningful for the decomposed spec.

**Open decisions carried forward:**
- [ ] Whether to fetch the *official* Eurostat import dependency (`nrg_ind_id`) and
  report import_dep as a **separate** energy-security dimension (not merged). Ties
  to item 4.
- [ ] If the decomposed vulnerability spec is promoted (see item 3), swap the
  renewable-share validator in `06` for a renewable-deployment trajectory, because
  circularity returns when fossil share carries high weight.

## 3. Decorrelate vulnerability variables (split GHG/Energy and Energy/VA)

**Feedback:** Use different vulnerability variables; check correlations. Maybe
split Energy/ValueAdded and GHG/Energy for a more comprehensive view.

**Diagnosis:** current vulnerability block is collinear and income-loaded.

| Pair | correlation |
|------|-------------|
| CarbonIntensity (GHG/VA) vs EnergyIntensity (Energy/VA) | **0.68** (same axis) |
| CarbonIntensity vs log GDP | −0.67 |
| EnergyIntensity vs log GDP | −0.71 |

Proposed decomposition is measurably better (near-orthogonal, less income-loaded):

| Pair | correlation |
|------|-------------|
| GHG/Energy vs Energy/VA | **+0.02** (orthogonal) |
| GHG/Energy vs log GDP | **−0.33** (weakly income-linked) |
| Energy/VA vs log GDP | −0.71 |

Identity: GHG/VA ≡ (GHG/Energy) × (Energy/VA). Decomposition separates **carbon
content of energy** (fuel mix / decarbonization — near income-neutral) from
**energy intensity of production** (efficiency — income-linked). Directly answers
item 1 too.

**Implications / TODO:**
- [x] Add `CarbonPerEnergy_normed` (GHG per unit final energy) to the indicator
  table (`R/functions/indicators.R`), rebuild `taxonomy_indicators.csv`.
- [x] Add decomposed-vulnerability spec to `07_robustness.R` and report its
  rank-correlation with the baseline + quadrant changes.
- [x] Report R²(vulnerability ~ log GDP) for baseline vs decomposed spec (item 1).
- **Results — NB now uses the demand-side fossil share** (after the § Fossil-share
  substitution). With `ShareFossils_normed` = gross-available fossil share, the
  decomposed vulnerability `{GHG/energy, energy/VA, fossil_gross}` becomes
  **fully income-neutral: R²(logGDP) = 0.00** (baseline = 0.55, potential = 0.07).
  But it is now nearly orthogonal to the current headline (Spearman 0.12) and
  **re-classifies 18/27 countries** — a fundamentally different map, not a tweak.
  (With the *old* production fossil share the same spec gave R²=0.22, Spearman
  0.85, 6/27 — the demand-side share is what pushes income-loading to zero.)
- [ ] **Decision:** promote the decomposed set to the headline `VULN_VARS`?
  Trade-off is now stark: **fully income-neutral vulnerability (R²=0.00)** — the
  cleanest possible answer to item 1 — **vs. 18/27 countries re-classified** and a
  vulnerability PC that explains less variance (near-orthogonal inputs). Inspect
  the new map's face validity before deciding; this is a bigger call than a
  robustness tweak.
- **SUPERSEDED** by the symmetric four-dimension structure below. The decomposed
  spec (income R²=0.00) was rejected: income *is* relevant and should not be forced
  out, and the map was undifferentiated (eta²(growth model)=0.28, countries bunched,
  story invisible). Kept only as a robustness spec in `07`.

## Symmetric four-dimension structure (ADOPTED as headline, implemented)

**Origin:** the real objection to the flat headline was two *collinear* variables
in one block (carbon vs energy intensity, r=0.68) — apparent double-counting.
Diagnosis: that redundancy is **load-bearing** — the two intensities are what make
vulnerability income-linked *and* growth-model-separated (eta²=0.62); dropping
either flattens the story. They are not two variables but **two indicators of one
construct** (emission intensity of production; GHG/VA = (GHG/energy)×(energy/VA)).

The potential block mirrors this exactly (checked): **GCI↔GCP = 0.78** (the twin
pair, both income-neutral), **green patents** the distinct standalone (income
+0.79). So each block = a two-indicator twin sub-index + one standalone:

| Block | Twin sub-index (2 indicators, PC1) | Standalone | Income structure |
|-------|-----------------------------------|-----------|------------------|
| **Vulnerability** | emission **intensity**: CarbonIntensity + EnergyIntensity (r 0.68) | **fossil** dependency | intensity income-linked · fossil neutral |
| **Potential** | green **complexity**: GCI + GCP (r 0.78) | **innovation** (patents) | complexity neutral · patents income-linked |

**Elegant consequence:** each block holds one income-linked and one income-neutral
part on *opposite* diagonals. Equal-weighting the two parts (not a flat PCA that
over-counts the twins) makes income **present but balanced in both blocks** — the
principled answer to item 1: not income-purged (that was unattractive), but
income no longer the sole driver of vulnerability.

**Implementation (done):**
- [x] `config.R`: `INTENSITY_VARS`, `FOSSIL_VAR`, `COMPLEXITY_VARS`, `INNOV_VAR`;
  `VULN_VARS`/`POT_VARS` kept as flat lists for 07's robustness specs.
- [x] `R/functions/typology.R`: `axis_score()` = twin PC1 sub-index + standalone,
  combined as equal-weight standardised mean.
- [x] `04_typology.R`: headline now uses `axis_score`; `taxonomy_scores.csv` gains
  the four part-scores (intensity/fossil/complexity/innovation).
- [x] `R/appendix_structure_map.R`: side-by-side flat vs structured map +
  reclass table (`plots/appendix_structure_map.*`, `data/tidy/appendix_structure_reclass.csv`).

**Results:**
- Go/no-go: R²(vuln~logGDP)=0.25, R²(pot~logGDP)=0.33 (balanced), cor(vuln,pot)=−0.57.
- Twin sub-indices near-orthogonal to their standalone (intensity–fossil 0.07,
  complexity–patents 0.18) — genuinely two dimensions, no double-counting.
- **Only 8/27 move vs the flat headline** (Finland, Ireland, Luxembourg, Malta,
  Netherlands, Portugal, Slovakia, Slovenia). Core–periphery story SURVIVES: most
  of the Workbench East stays "At risk"; Poland/Czechia stay "Exposed" (coal).

**Open follow-ups:**
- [x] **Realign `07_robustness.R`** — done: baseline = `axis_score` (structured);
  specs now include "flat blocks (single PCA)", twin mean-vs-PCA, robust scaling,
  part weights 2:1, ECI-vs-GCI, renewable-only GCI, production-based fossil.
  Results: flat blocks 8/27 (matches the appendix map), twin mean 0/27, part
  weights 2:1 → 4/27, production fossil 8/27, window shifts 0/27, outlier drops
  1/26. Income relevance: structured 0.25/0.33 vs flat 0.55/0.07.
- [ ] **Validator circularity now active** (`06`): fossil ≈ half of vulnerability,
  so `cor(vulnerability, renew_share) = -0.46` — renew_share is descriptive for
  vulnerability, legitimate only for potential (+0.25). Use gdp_growth / EPS /
  renewable-deployment trajectory to validate vulnerability. (06 comment updated.)
- [ ] Prose + Fig. 1: present the taxonomy as four dimensions (2 per block), each
  block a twin sub-index + standalone; explain the balanced-income diagonal.

## Offshoring: burden vs. responsibility (LOGGED, implemented)

**Question raised:** the headline carbon variable is *production*-based
(`GWP_pba / ValueAdded_pba`), yet the low-vulnerability countries are exactly the
ones with large net embodied-emission imports. A referee can say the Winners look
clean only because the East makes their goods — which would **invert** the
polarization story into "the core exports its burden". Can offshoring instead be
framed as a *finding* that is part of the model?

**Answer: yes — but only the responsibility version of it.** Three results.

### 1. The vulnerability axis is near-invariant to the accounting choice

Swapping `CarbonIntensity_normed` for its consumption-based counterpart
`CarbonIntensityCBA_normed` = (GWP_pba + imports − exports) / value added:

| | production-based | consumption-based |
|---|---|---|
| Spearman(vuln) vs baseline | — | **0.98** |
| R²(vuln ~ log GDP p.c.) | 0.25 | 0.25 |
| eta²(vuln ~ growth model) | 0.37 | 0.41 |
| quadrant changes | — | **2/27** (Ireland, Slovakia) |

So the map is **not** an artifact of production-based accounting. This is the
answer to the objection, and it is what *licenses* the finding below: had the map
flipped, offshoring would be a confound and an accounting would have to be chosen
and defended. It does not, so the offshoring layer can be presented as substance
without destabilising the headline.

### 2. The offshoring correction is real and systematic — just not decisive

| Quadrant | int. PBA | int. CBA | rise | t/cap PBA | t/cap CBA | rise |
|---|---|---|---|---|---|---|
| Winners | 207 | 295 | **+43%** | 8.0 | **11.8** | **+48%** |
| Exposed | 560 | 565 | +6% | 10.2 | 10.8 | +6% |
| Low-stakes | 349 | 486 | +38% | 5.7 | 7.9 | +38% |
| At risk | 560 | 598 | +7% | 8.4 | 9.4 | +11% |

(intensity in g CO₂e per EUR value added.) By growth model the rise is Core +38%,
**Finance +70%**, Periphery +7%, Workbench +7%. The Winners↔At-risk intensity gap
narrows from **2.7× to 2.0×** — about a quarter of it closes — and the ordering
never changes. Largest offshoring ratios (CBA/PBA footprint): Malta 3.11,
Luxembourg 1.91, Sweden 1.84, Belgium 1.69, Italy 1.61, France 1.56.

### 3. The quotable number is a LEVEL, not an intensity

In **production** terms Winners and At-risk countries emit about the same per
capita (8.0 vs 8.4 t). In **consumption** terms it reverses: **11.8 vs 9.4 t**.
The two facts are compatible precisely because one is an intensity (per unit of
value added, where the denominator absorbs the offshoring) and one is a level.

### The framing: burden vs. responsibility

Two different concepts, each with its proper instrument:

- **Vulnerability = adjustment burden** — what a country must physically retool
  (plants, workers, energy system). Properly **production**-based, per unit of
  value added. Germany's transition task does not include decarbonising Polish
  steel. This stays the headline axis.
- **Consumption footprint = responsibility** — whose final demand the emissions
  serve. Properly **consumption**-based, per capita. An interpretive layer.

The **finding is the asymmetry**: the core has low burden *and* high
responsibility; the Workbench East has high burden *and* low responsibility. That
sharpens polarization rather than undercutting it — the East bears the adjustment
cost of consumption occurring in the West.

Net embodied imports p.c. is deliberately **not** promoted to an axis variable:
it correlates **+0.71** with log GDP p.c., so it would reimport exactly the income
confound the per-value-added intensities remove (same argument that kept fossil
consumption p.c. out).

### The claim that CANNOT be made yet

The **mechanism** version — *the core offshores to the European periphery, and
that is how polarization is produced* — is **not verifiable with the data in the
repo**. `data/tidy/TXNY_GWP_Trade.csv` carries only country **totals** of embodied
imports/exports; there is no origin dimension. If Germany's embodied imports come
mainly from China rather than Poland, this is a Europe-vs-rest-of-world story, not
an intra-European one. Until bilateral flows exist, phrase it as an asymmetry
between two accountings, **not** as a transfer between two groups.

**Implementation (done):**
- [x] `R/functions/indicators.R`: added `CarbonIntensityCBA_normed` and
  `GWP_cba_normed`; rebuilt `taxonomy_indicators.csv` (pre-existing values
  bit-identical, two columns added).
- [x] `07_robustness.R`: spec "carbon: consumption-based (CBA)" → 0.98 / 2-of-27.
- [x] `R/appendix_burden_responsibility.R` → `data/tidy/burden_responsibility.csv`,
  `plots/burden_responsibility.{png,pdf}` (paired production→consumption footprint
  per capita, faceted by quadrant). Also delivers the Malta/Luxembourg vignette
  numbers owed under item 2.

**Open follow-ups:**
- [ ] **Scope bilateral EXIOBASE flows.** The external Python script that produced
  `TXNY_GWP_Trade.csv` would need to emit origin×destination embodied GWP. This
  single addition converts a descriptive asymmetry into an actual mechanism and is
  the highest-value remaining data task.
- [ ] Write the burden-vs-responsibility paragraph into the paper, with the
  invariance (0.98 / 2-of-27) stated *first* so the layer reads as a finding
  rather than as a defensive concession.

## 4. Alternative emissions data source (Eurostat instead of EXIOBASE)

**Feedback:** Can I get the trade-embodied / production emissions from a source
other than EXIOBASE (e.g. Eurostat)? (User is running a Perplexity search.)

**Candidates:**
- Eurostat **Air Emissions Accounts** `env_ac_ainah_r2` — production-based GHG by
  industry (native `GWP_pba` analogue).
- Eurostat **consumption-based CO₂ footprint** `env_ac_co2fp` and **FIGARO** MRIO
  — closest Eurostat substitute for EXIOBASE embodied-trade emissions.
- OECD **Trade in Embodied CO₂ (TeCO₂)** — another MRIO alternative.

**Caveat:** different system boundaries (residence + full GHG basket vs.
EXIOBASE MRIO CO₂-eq) → treat cross-source agreement as a robustness check, not a
drop-in swap.

**Implications / TODO:**
- [ ] Await Perplexity results; pick the best Eurostat-native replacement.
- [ ] Add as a robustness source (re-run vulnerability with Eurostat AEA /
  consumption footprint) and report agreement with EXIOBASE.
- [ ] Note discrepancies (boundary, gases, accounting principle) in methods.
