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
  capable than the Core* → **strongly supported**, p = .0002 on both axes in a
  pairwise permutation test. Would make a clean H1–H3 test.
- **Broad hypothesis** — *different growth models face different transition
  challenges* (plural, differentiated) → **not supported**. A four-group model
  does not beat a Core-vs-rest binary (F = 1.15 and 0.11, p = .34 and .88).
  Note this is a limit of *resolution*: the design could only detect non-Core
  differences ≥ 1.26–1.72 z and the observed spreads are 0.20–0.57 z. So the
  broad claim is unsupported, not refuted — but unsupported is enough reason not
  to make it.

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

## 6. ~~Unbalanced quadrants — is the median split the right device?~~ RESOLVED

Tested (see `results-summary.md` §8f, `evidence/quadrant_stability.csv`). The map
is effectively deterministic: 25 of 27 countries hold their quadrant in ≥99.7% of
year-bootstrap resamples. **Quadrants can be presented as types**, provided the
three-country boundary neighbourhood (Ireland, Slovakia, Slovenia) is named and
the two-country Low-stakes cell is not used to carry an argument.

## 7. Slovenia — fragile to the SAMPLE, not to the data

Slovenia is the single Workbench country among the Winners, so it carries weight
in the "the map is not just the growth model" argument. The bootstrap refines
what its fragility actually is: it is **100% stable** under year resampling but
moves in **12 of 27** leave-one-out runs. It sits close to a threshold rather
than having noisy inputs.

That is a milder caveat than "Slovenia is unstable", and it should be written
that way: its position is well-measured, but near a boundary whose location
depends on the other 26 members.

## 8. Validator circularity for vulnerability

Fossil dependency is ~half of vulnerability and fossil share ≈ (1 − renewable
share), so `vulnerability ~ renewable share` is partly mechanical. Either find a
non-circular vulnerability validator (renewable-deployment *trajectory*, or lean
on EPS despite its 20/27 coverage) or state plainly that it is descriptive there.

## 9. Open analyses that would strengthen the paper

All the robustness items are now closed except one optional source check. Each
closed item is left here with its result, because the paper should report that
these were tested rather than silently omit them.

- ~~**EORA cross-check.**~~ **DONE — 0/27** (see `results-summary.md` §8b,
  `evidence/eora_crosscheck.csv`). MRIO choice can now be dismissed in a
  sentence, with the EORA data-quality caveat reported.
- ~~**National patent offices.**~~ **DONE — 0/27** (see `results-summary.md` §8d,
  `evidence/patent_offices.csv`, `figures/patent_offices.png`). The bias is real
  and systematic (EPO share: Workbench 12.8% vs Core 21.2%) but does not move any
  country's quadrant. Present it as conceded-and-quantified rather than
  dismissed.
- **Eurostat alternative to EXIOBASE** (AEA `env_ac_ainah_r2`, FIGARO, OECD
  TeCO₂) as a second emissions source. Different system boundaries, so treat
  agreement as a robustness check, not a drop-in swap.

## 10. ~~A finding that has not been followed up~~ — followed up, and it is not a finding

R²(potential ~ log GDP p.c.) falls 0.45 → 0.21 across rolling windows, which
looked like green capability decoupling from income. **Decomposition kills it**
(see `results-summary.md` §8e):

- complexity was never income-linked (R² 0.02 throughout), so nothing converged;
- the whole fall is in the patent component, and is mostly a skew artifact — on a
  log scale R² falls only 0.77 → 0.68 against 0.73 → 0.46 raw, while skew rises
  1.43 → 1.98;
- the real convergence signal is +0.09 z for the Workbench over five years, on
  heavily overlapping windows.

**Decision needed:** whether to mention it at all. My recommendation is one
sentence noting that green complexity shows no income gradient in any window — a
robustness point about GCI, not a convergence result — and nothing more. You said
you wanted it available but not as a headline; on this evidence it should not be a
section either.
