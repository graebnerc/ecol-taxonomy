# Open questions and decisions

*Last revised 2026-09-02.*

Everything here is **unresolved**. Nothing in this file should be written into
the paper as settled. Where a decision is needed, the evidence for each option is
given so the choice can be made on evidence rather than taste.

Ordered by how much each one gates the draft.

---

## 1. GATING: descriptive typology, or an explicit test of H1–H3?

**Status: undecided. Nothing else in the draft can be structured until this is.**

H1–H3 are referenced throughout the project material and **written down nowhere**.
The only articulation that exists anywhere is a speaker note:

> H1 catch-up/Workbench more vulnerable · H2 less capable · H3 they combine into
> the polarization quadrant.

**What the evidence supports.** Regressed on growth model with Core as reference:

| vs Core | Finance | Periphery | Workbench |
|---|---:|---:|---:|
| potential | −1.5 (p=.009) | −1.4 (p=.008) | −1.6 (p=.001) |
| vulnerability | +1.1 (p=.047) | +1.1 (p=.035) | +1.6 (p=.0007) |

- **Narrow hypothesis** — *the catch-up East is both more vulnerable and less
  capable than the Core* → **strongly supported**. Would make a clean H1–H3 test.
- **Broad hypothesis** — *different growth models face different transition
  challenges* (plural, differentiated) → **not supported**. Finance and Periphery
  are statistically indistinguishable from Workbench on both axes. The pattern is
  Core vs everyone else.

So the real question is not "descriptive or test" in the abstract. It is whether
the hypothesis worth testing is the narrow one the data supports, or the broad
one it does not. If the intended claim was the broad one, descriptive framing is
the honest route.

**Also relevant:** with n = 27, a hypothesis-testing register invites
significance-testing scrutiny the sample cannot bear. A descriptive typology
framing sidesteps that; a test framing has to confront it explicitly.

## 2. GCI: "green complexity" or "green export diversity"?

In this sample GCI correlates **~0.998** with a plain count of green products
exported with RCA ≥ 1 — so it is essentially green *diversity*, not
*sophistication*. Mealy & Teytelboym concede the point in their footnote 9.

A referee will check this. Decide the wording now so it is consistent from the
first paragraph. Options: call it green export diversity throughout; keep "green
complexity" with an explicit footnote; or report both framings.

## 3. Target journal

*Ecological Economics* / *Socio-Economic Review* / *Cambridge Journal of
Economics*. This determines the reference set and the register (how much
growth-model literature, how much complexity literature), so it should be settled
before drafting rather than after.

## 4. Policy ask — make one or not?

Whether to argue for quadrant-based Cohesion / Just-Transition-Fund targeting, or
stay descriptive. The "Exposed but capable" cell (Czechia, Netherlands, Poland) is
where a policy argument has the most empirical footing: real burden, real latent
capability, missing innovation base — a targetable diagnosis a GDP ranking cannot
give. But n = 3.

## 5. Confirm the scope cuts as one-liners

So they do not reopen mid-draft:

- brown employment dimension — **out**
- clustering — **illustrative / robustness only**, not a headline
- reference window — **2017–2021**
- green list — **OECD CLEG 244 codes**, footnote the 293-code alternative

## 6. Unbalanced quadrants — is the median split the right device?

Quadrants are 11/11/3/2. Because the axes correlate −0.55 the off-diagonal cells
are thin, five countries carry the interesting story, and Low-stakes (Ireland,
Portugal) reads more like a residual than a type. Four countries are borderline.

Consider whether to present the map continuously with quadrants as exposition
only, rather than treating the four cells as types. This is a presentational
decision with substantive consequences for how much weight the quadrant labels
can bear.

## 7. Slovenia is fragile and load-bearing

Slovenia is the single Workbench country in the Winners cell, so it carries
disproportionate weight in the "the map is not just the growth model" argument —
and it is borderline, landing there mainly by sitting just below the vulnerability
median. It needs its own sentence, or a referee will make it one.

## 8. Validator circularity for vulnerability

Fossil dependency is ~half of vulnerability and fossil share ≈ (1 − renewable
share), so `vulnerability ~ renewable share` is partly mechanical. Either find a
non-circular vulnerability validator (renewable-deployment *trajectory*, or lean
on EPS despite its 20/27 coverage) or state plainly that it is descriptive there.

## 9. Open analyses that would strengthen the paper

Not blocking, but each closes a line a referee could open.

- **EORA cross-check.** `evidence/window_options.csv` contains a **2014–2017**
  row on both patent measures, built specifically because that matches available
  EORA coverage. Recomputing it on EORA isolates the **MRIO table choice** from
  the window choice. Half the comparison already exists: EXIOBASE 3.8 → 3.10.2
  moved 0/27. If EORA also lands near zero, MRIO choice becomes a one-sentence
  dismissal.
- **National patent offices.** The patent query counts EPO filings only
  (`appln_auth = 'EP'`), which plausibly understates the small and eastern states
  in the low-potential tail — now more load-bearing since the double-counting
  correction showed how sensitive periphery counts are to query details.
- **Country drill-downs.** The sectoral energy split (households vs business,
  available in Eurostat `nrg_bal_c` without a new download) for the flagged
  countries — Malta and Luxembourg especially.
- **Eurostat alternative to EXIOBASE** (AEA `env_ac_ainah_r2`, FIGARO, OECD
  TeCO₂) as a second emissions source. Different system boundaries, so treat
  agreement as a robustness check, not a drop-in swap.

## 10. A finding that has not been followed up

**R²(potential ~ log GDP p.c.) falls monotonically as the window moves forward:**
0.37 (2014–17) → 0.33 (2014–18) → 0.21 (2017–21) → 0.19 (2019–21) → 0.18
(2020–22).

Green capability is becoming **less income-dependent over time**. That is direct
evidence on whether the catch-up East is closing the green-capability gap, and it
is currently sitting in a robustness table rather than being analysed. It may
deserve to be a result in its own right — possibly a second contribution
alongside the typology. Worth cross-checking against the capability-trajectory
analysis before making anything of it.
