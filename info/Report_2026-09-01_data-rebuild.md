# Report — EXIOBASE 3.10.2 rebuild, patent measure, reference window

**Written:** 2026-09-01 evening · **Updated:** 2026-09-02 with your decisions applied
**Branch:** `develop`, 33 commits ahead of `main`, *still unpushed to origin*

**The one-line version:** the headline is now **2017–2021 on patent applications**;
it moves 2 of 27 countries against the old map and every more-recent window agrees
perfectly. One action still waiting for you (§7.1).

> **Decisions applied 2026-09-02.** Patent measure → **applications** (grants kept
> as a robustness spec). Window → **2017–2021**. `R/config.R` now carries
> `PATENT_MEASURE` and the new window; everything downstream was re-run. The
> framing decision (descriptive typology vs. explicit H1–H3 test) is deliberately
> deferred until the data questions close — §7.2.

---

## 1. What you asked for

Rebuild the EXIOBASE layer on 3.10.2 including bilateral flows, implement all
three green-patent options to see their implications, and scope the window
extension — "honest, most recent, but consistent". All three are done and
committed. Two things came out differently from expectation, both usefully.

---

## 2. The result that matters most: the map is unchanged

Swapping the whole footprint layer from the old 3.8.x vintage to 3.10.2 moves
**zero of 27 countries**. On the 2013–2022 overlap:

| column | new/old | Pearson | Spearman |
|---|---:|---:|---:|
| `GWP_pba` | **1.183** | 0.9954 | 0.9872 |
| `GWP_Exports` | 1.095 | 0.9892 | 0.9693 |
| `GWP_Imports` | 1.013 | 0.9943 | 0.9827 |
| `ValueAdded_pba` | 0.965 | 1.0000 | 0.9997 |
| `Employment_pba` | 1.001 | 0.9937 | 0.9916 |

3.10.2 puts EU production emissions ~18% higher and value added ~3.5% lower —
carbon intensity ~25% higher **in level** — yet the cross-country structure is
nearly identical. Effect on the typology: vulnerability Pearson 0.997 /
Spearman 0.987, largest single move 0.20 z (Estonia −0.20, Hungary +0.20, both
Workbench). Potential, fossil, complexity and innovation are bit-identical
because they use no EXIOBASE input.

Diagnostics barely move: `cor(vuln, pot)` −0.57 → **−0.58**,
R²(vuln~logGDP) 0.25 → **0.28**, R²(pot~logGDP) **0.33** unchanged.

**This is a robustness result in its own right and belongs in the paper**: the
typology is invariant to the MRIO release, across an 18% revision of the
underlying emission accounts. Referees ask about MRIO choice; now there is an
answer.

Two side benefits: the footprint layer is now computed **in-repo** from a named,
checksummed Zenodo source (`R/get_data_exiobase.R`), which closes the external
Python dependency `info/AuditReport_2026-07-14.md` flagged as unreproducible; and
the consumption-based accounting spec in `07` now moves **0/27** (was 2/27).

---

## 3. The bilateral test: the offshoring mechanism claim does not survive

This is what the 49×49 matrix was for. Decomposing each EU bloc's embodied GHG
imports by origin (2014–2018 annual mean):

| destination ↓ | Workbench | Core | Finance | Periphery | China | Other non-EU | RoW agg. |
|---|---:|---:|---:|---:|---:|---:|---:|
| **Core** | **9.2%** | 7.9 | 4.6 | 7.0 | 15.5 | 34.2 | 21.6 |
| Finance | 3.8 | 9.8 | 1.6 | 4.8 | 16.5 | 36.5 | 27.1 |
| Periphery | 5.2 | 8.7 | 2.8 | 8.1 | 12.7 | 32.6 | 29.9 |
| Workbench | 15.5 | 10.2 | 1.7 | 4.8 | 11.0 | 35.0 | 21.9 |

**Only 9.2% of the Core's embodied emission imports originate in the Workbench
East. 71.3% come from outside the EU entirely** (EU-27 as a whole: 73.2%
extra-EU). So "the core's clean profile rests on offshoring to the European East,
and that is the mechanism of polarization" is **not supportable**. The core
offshores to the world, not primarily to its own periphery.

But the direction is not nothing. Net intra-EU flows (Mt CO₂e/yr, + = row is a
net exporter to column):

| | Core | Finance | Periphery | Workbench |
|---|---:|---:|---:|---:|
| **Workbench** | **+39.9** | +3.5 | +26.3 | — |
| Core | — | −12.9 | +13.4 | −39.9 |

**The Workbench East is a net embodied-emission exporter to every other EU bloc**
— the direction the polarization story predicts. It is just an order of magnitude
too small to be the mechanism: +39.9 Mt/yr against 788 Mt of Core embodied
imports, about 5%.

**Suggested framing.** Keep the burden-vs-responsibility asymmetry (§4) as the
finding — it is robust. State the intra-EU transfer honestly as *present, in the
predicted direction, and small*. Do not claim the core's footprint is built on
the European East; a referee with the same data would take that apart. The
stronger and defensible claim is that the Workbench East's high burden is
overwhelmingly its **own** carbon-intensive, low-value-added production, not
production for the West — which makes the polarization structural rather than a
transfer, and arguably harder to fix.

`R/appendix_offshoring_origins.R` · `plots/offshoring_origins.png` ·
`data/tidy/offshoring_origins.csv`

---

## 4. Burden vs responsibility, on the new data

Unchanged in substance, numbers refreshed:

| quadrant | n | carbon int. PBA | CBA | t/cap PBA | t/cap CBA |
|---|---:|---:|---:|---:|---:|
| Winners | 11 | 263 | 342 | 9.5 | **12.6** |
| Exposed | 3 | 639 | 641 | 11.3 | 12.1 |
| Low-stakes | 2 | 436 | 544 | 6.7 | 8.3 |
| At risk | 11 | 671 | 711 | 9.2 | **10.2** |

In production terms Winners and At-risk emit about the same per capita (9.5 vs
9.2 t); in consumption terms it reverses (12.6 vs 10.2). The intensity gap
narrows 2.6× → 2.1× and the ordering never changes. `cor(net embodied imports
p.c., log GDP p.c.)` = +0.65, which is why that variable stays **out** of the
axis.

---

## 5. Green patents: the three options are empirically equivalent

The window was capped at 2018 by EXIOBASE. With that constraint gone the cap
moved to patents — and the cause is **grant lag**, not publication lag:
`sql/get_green_patents.sql` counts EPO *grants* by filing year, and EPO grant lag
is 3–5+ years. Waiting gains ~one usable filing year per year, so a 2019–2023
window on grants needs a ~2028 snapshot.

The OECD ENV-TECH database carries **both** measures on the same underlying EPO
data, so they can be compared without a source change confounding it. Result
(EU-27, per capita, 2014–2018):

| | applications ↔ grants | applications ↔ PATSTAT v1 | grants ↔ PATSTAT v1 |
|---|---:|---:|---:|
| Spearman | **0.993** | **0.988** | **0.994** |

Substituting **any** of them into the potential axis moves **0 of 27** countries.
Meanwhile:

| series | last year within 10% of its 2018 level |
|---|---:|
| OECD applications | **2021** |
| OECD grants | 2019 |
| PATSTAT v1 grants (current headline) | **2018** |

**So option B (applications) is a free swap that buys three years, and option C
(the hybrid) is redundant — there is nothing left to hedge against.** That is the
recommendation, and it rests on a 0/27 result rather than on argument.

*Caveat for the paper:* applications measure filing activity, grants measure
successful innovation. That they rank EU-27 countries alike is an empirical
finding about this sample, not a conceptual identity — say so.

---

## 6. Window: 2017–2021 is reachable, at a cost of two borderline countries

Three constraints, now all measured:

| constraint | last usable year |
|---|---:|
| EXIOBASE emissions (3.10.2, **complete**) | **2022** |
| Green patents — applications | **2021** |
| Green patents — grants (current) | 2018 |
| Eurostat energy / GDP | 2023 |

**2023 and 2024 are unusable**, and not for the reason I expected. Their archives
lack the employment/energy/water extensions, but the real problem is that
`CO2 - combustion - air` — 28.8 of ~45 Gt, the single largest stressor — is
**identically zero** in both, while every other stressor looks normal. World PBA
reads 12 Gt instead of 46. Nothing errors: the arithmetic is valid, the input is
not. There are now two independent guards (a stressor-level check at extraction
and a world-total plausibility check at combine time), and the panel update drops
flagged years. Had I not looked at the totals, this would have silently poisoned
any recent window.

Rebuilding the **entire** typology on each candidate window — complexity
re-pooled from the Atlas, indicators re-averaged, patents re-measured:

| window | cor_vuln | cor_pot | quad changes | R²(vuln~GDP) | R²(pot~GDP) | cor(axes) |
|---|---:|---:|---:|---:|---:|---:|
| 2014–2018 (headline) | 1.00 | 1.00 | — | 0.28 | 0.33 | −0.58 |
| 2014–2018, applications | 1.00 | 1.00 | 0 | 0.28 | 0.33 | −0.59 |
| 2016–2020, applications | 0.99 | 0.99 | **0** | 0.27 | 0.26 | −0.56 |
| 2017–2021, applications | 0.99 | 0.99 | **2** | 0.27 | 0.21 | −0.55 |

Moving to **2017–2021 costs 2 of 27 countries — Ireland and Slovakia, both
already flagged borderline** on the current map. Rank correlations stay at 0.99.
One real change: R²(potential ~ log GDP) falls 0.33 → 0.21, so the income
balance across blocks shifts (0.27 / 0.21 rather than 0.28 / 0.33). Whether that
is an improvement depends on how you want to argue item 1 in `PaperTodos.md`.

**My recommendation: move to 2017–2021 and switch patents to applications.** It
is three years more recent, costs two borderline countries, and every input is
observed rather than nowcast. But this changes the paper's headline numbers, so
I have not done it — §7.2.

`R/appendix_window_options.R` · `plots/window_options.png` ·
`data/tidy/window_options.csv`

---

## 6b. Applied: the new headline, and how much it moved

**2 of 27 countries move** against the former 2014–2018 grants headline — and it
is a straight swap between the two smallest quadrants, both already flagged
borderline:

| country | from | to |
|---|---|---|
| Ireland | At risk | Low-stakes |
| Slovakia | Low-stakes | At risk |

Spearman 0.987 on vulnerability, 0.992 on potential. Diagnostics:
`cor(vuln, pot)` −0.58 → **−0.55**, R²(vuln~logGDP) 0.28 → **0.27**,
R²(pot~logGDP) 0.33 → **0.21**. Cramér's V (quadrant × growth model) 0.45 → 0.50.

### Every window you asked for, whole typology rebuilt on each

Baseline is the new headline; `quad_changes` counts against it.

| window | cor_vuln | cor_pot | quad Δ | R²(v~GDP) | R²(p~GDP) | cor(axes) | note |
|---|---:|---:|---:|---:|---:|---:|---|
| **2017–2021 apps (headline)** | 1.00 | 1.00 | — | 0.27 | 0.21 | −0.55 | |
| 2014–2018 apps | 0.99 | 1.00 | 2 | 0.28 | 0.33 | −0.59 | former window |
| 2014–2018 grants | 0.99 | 1.00 | 2 | 0.28 | 0.33 | −0.59 | former headline |
| **2014–2017 apps** | 0.98 | 0.99 | 2 | 0.28 | 0.37 | −0.59 | **matches your EORA coverage** |
| **2014–2017 grants** | 0.98 | 0.99 | 2 | 0.28 | 0.37 | −0.59 | **matches your EORA coverage** |
| 2019–2021 apps | 0.98 | 0.99 | **0** | 0.26 | 0.19 | −0.55 | last 3y, all inputs clean |
| 2020–2022 apps | 0.97 | 0.98 | **0** | 0.26 | 0.18 | −0.53 | last 3y EXIOBASE supports; apps 2022 ~73% complete |
| 2017–2021 grants | 1.00 | 1.00 | **0** | 0.27 | 0.21 | −0.55 | grants heavily truncated here |

Three things to take from this:

1. **Every recent window agrees perfectly** (0/27 for 2019–2021, 2020–2022 and
   grants-in-window). The only disagreement is with pre-2019 windows, and it is
   always the same two countries. The map is not sensitive to *where* in
   2017–2022 you put the window — only to whether you are before or after 2019.
2. **Grants vs applications is 0/27 even inside 2017–2021**, where grants are
   severely truncated. The truncation is proportional enough across countries
   that the ranking survives it. That is a stronger result than the 2014–2018
   comparison and worth quoting.
3. **R²(potential ~ log GDP) falls monotonically as the window moves forward**:
   0.37 (2014–17) → 0.33 (2014–18) → 0.21 (2017–21) → 0.19 (2019–21) → 0.18
   (2020–22). Green capability is becoming *less* income-dependent over time.
   That is a substantive finding, not a technicality — it is direct evidence on
   whether the catch-up East is closing the green-capability gap, and it deserves
   its own look rather than a footnote.

### For the EORA comparison

`2014–2017` is built and committed on both patent measures. Because the *window*
is held identical to your EORA coverage, recomputing that row on EORA isolates
the **MRIO table choice** from the window choice. You already have one half of
that comparison: EXIOBASE 3.8 → 3.10.2, an 18% revision of the emission accounts,
moved **0/27**. If EORA also lands near zero on the same window, the MRIO choice
is a non-issue and can be dismissed in a sentence.

---

## 7. What I need from you

### 7.1 Run the PATSTAT query (~10 minutes)

```
1. run  sql/get_green_patents_v2.sql  against a current PATSTAT edition
2. save as  data/tidy/patstat_green-patents_v2.csv
3. Rscript R/get_data_patents_patstat.R
```

The ingestion script validates the file, quantifies both defects, cross-checks v2
against v1 on the mature 2014–2018 cohorts, and writes the tidy panel. With the
file absent it prints instructions and exits 0 without changing anything, so
nothing is blocked meanwhile. Once it exists, both appendix scripts pick it up
automatically and add the like-for-like PATSTAT applications series.

**Why it is worth the ten minutes — there is a defect in the current query.**
`sql/get_green_patents.sql` is `COUNT(appln.appln_id)` over joins to
`tls224_appln_cpc` (one row per CPC symbol) and `tls207_pers_appln` (one row per
applicant). Its header comment claims it "considers an id only once"; it has no
`DISTINCT`, so an application with *k* matching Y02/Y04S symbols and *m*
same-country applicants is counted *k×m* times. The inflation is **differential
across countries**, so it does not cancel in a ranking — and green patents are
the most discriminating variable in the potential axis. Circumstantial evidence
that it bites: indexed to 2018, the PATSTAT v1 series reads 141 in 2014 where the
OECD grants series on the same concept reads 106. `get_green_patents_v2.sql`
fixes it with `COUNT(DISTINCT ...)` and returns the old behaviour as `n_raw_join`
so the distortion can be **measured**.

### 7.2 Three decisions that are yours

1. **Switch the patent variable to applications?** Evidence says free (0/27) and
   buys three years. Not done — it changes what the innovation dimension measures.
2. **Move the window to 2017–2021?** Costs 2/27, both borderline. Not done — it
   moves every headline number in the paper.
3. **The EPO-only restriction** (`appln_auth = 'EP'`) excludes national patent
   offices, plausibly understating exactly the small and eastern states in the
   low-potential tail. Already a TODO in the old SQL; worth one robustness run
   without the filter.

---

## 8. Also worth knowing

- **The pre-2013 footprint years are gone** from the panel. The new extract covers
  2013–2024 by design; mixing two EXIOBASE releases inside one column would be
  worse than a documented gap. Nothing in `01`–`07` uses them. To restore, widen
  `YEARS` in `R/get_data_exiobase.R` (~2.5 min/year).
- **The extraction now writes per year and is restartable.** The first run
  discarded ten finished years when 2023 aborted. It cost ~25 minutes and was
  worth finding.
- `data/raw/exiobase` (2.9 GB) and `data/raw/oecd` (213 MB) are gitignored;
  everything derived is committed.

## 9. Reproducing any of this

```bash
data/raw/exiobase/fetch.sh            # 2.9 GB, md5-verified
Rscript R/get_data_exiobase.R         # ~30 min: totals + bilateral, restartable
Rscript R/update_panel_exiobase.R     # swaps the footprint layer only
Rscript R/01_build_indicators.R && Rscript R/04_typology.R
Rscript R/06_validation.R && Rscript R/07_robustness.R
Rscript R/appendix_offshoring_origins.R
Rscript R/appendix_patent_options.R
Rscript R/appendix_window_options.R
```
