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

*(Source: a speaker note in `presentation/taxonomy_presentation.qmd`, July 2026.
They have never been written down as formal hypotheses anywhere in the project.)*

**What the evidence supports.** Regressed on development model with Core as reference:

| vs Core | Finance | Periphery | Workbench |
|---|---:|---:|---:|
| potential | −1.5 (p=.009) | −1.4 (p=.008) | −1.6 (p=.001) |
| vulnerability | +1.1 (p=.047) | +1.1 (p=.035) | +1.6 (p=.0007) |

- **Narrow hypothesis** — *the catch-up East is both more vulnerable and less
  capable than the Core* → **strongly supported**, p = .0002 on both axes in a
  pairwise permutation test. Would make a clean H1–H3 test.
- **Broad hypothesis** — *different development models face different transition
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
development-model literature, how much complexity literature), so it should be settled
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
- green list — **OECD CLEG 244 codes**. NB this is *not* a footnote-sized choice:
  a renewable-only GCI correlates only 0.89 with the full version and moves 4/27
  countries. Argue for the list, do not just name it.

## 6. Unit of analysis: quadrants or development models? (RESOLVED, with a twist)

**Recommendation: the two-dimensional MAP is the unit; quadrants are exposition;
development models are validation.** Not four types, and not a development-model paper.

**Why quadrants over development models.** The typology is the contribution; the
development-model classification is someone else's. And development models organise this
data poorly at the level of means — a four-group model does not beat a
Core-vs-rest binary (§5). Cramér's V = 0.50 between quadrants and development models is
exactly the right relationship for a *validation*: high enough that the typology
tracks known structure, low enough that it is not redundant. That is an argument
**for** the typology, not a competing frame. One subsection, not a spine.

**The twist: quadrants are not coherent types either.** Mean within-unit sd across
the four components is **0.75 for quadrants against 0.72 for development models** — the
quadrants are, if anything, marginally *less* coherent. Worse, inside a quadrant
the components are **negatively correlated**: in *At risk*, intensity ↔ fossil
r = −0.58 and complexity ↔ innovation r = −0.53. Members trade one off against
the other, i.e. they share a score, not a situation:

> *At risk* contains **Bulgaria** (emission intensity +2.60, fossil −0.33 — dirty
> production burning its own energy) and **Malta** (intensity −1.19, fossil +1.66
> — clean production running on imported fossil fuel). Nothing in common but the
> label.

**So do not write either classification as a set of types.** Present the map
continuously, use the quadrants as exposition, and make the **four components the
interpretive layer** — naming the *route* by which a country arrives where it is.
That is more informative than either label and it is what the two-part axis design
was built to support.

### How much space for the development models?

**A full subsection — earned by two tested findings, not by authorship.**

The development-model classification is the author's own (Gräbner et al. 2020),
which cuts both ways: there is a legitimate interest in developing it further, and
a referee will watch for an author validating their own prior work. The way to
have both is to let the *evidence* set the space.

There are **two genuine contributions to that literature here**, both permutation-
tested, and neither is a level result:

1. **Composition.** Finance and Workbench have statistically identical green
   potential composed oppositely — complexity +1.05 (p = .011), innovation −1.12
   (p = .0007). The labels capture *different kinds* of capability, not different
   amounts.
2. **Coherence.** The Workbench is a point mass on green innovation (sd 0.07);
   the Periphery is dispersed on complexity (sd 1.48). The labels differ in how
   much work they do.

That is enough for a proper subsection. But **lead it with the null**: a
four-group model does not beat a Core-vs-rest binary, and only the Workbench–Core
contrast is robust. Reporting the unflattering result first is what makes the
positive ones credible when the classification is your own — and finding (2) is
mildly self-critical anyway, since it says the *Periphery* label is doing less
work than the *Workbench* label on this dimension.

**What would be overselling, and should be avoided:**

- *"Different development models face different transition challenges"* — the
  level claim. Not supported (§5).
- Treating this paper as **validation of the classification**. It is not: the
  four-way split adds nothing over Core-vs-rest here. The paper uses the
  classification and refines it; it does not vindicate it.
- Leaning on Cramér's V = 0.50 as agreement. It is moderate. Its value is that
  the typology is *neither redundant with nor unrelated to* the classification —
  which is an argument for the typology, not for the classification.

**Terminology:** they are **development models** (the author's term for this
classification). The adjacent Baccaro–Pontusson literature calls its objects
**growth models**; the two are related but distinct research programmes and the
paper should not blur them.

## 7. Slovenia — fragile to the SAMPLE, not to the data

Slovenia is the single Workbench country among the Winners, so it carries weight
in the "the map is not just the development model" argument. The bootstrap refines
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
