# Literature review briefs (Paperguide)

Paperguide takes **one free-text brief** — "describe the topic, research question
and requirements to generate a literature review" — plus filters for year, SJR
ranking and topic. So this file holds **two briefs**, not a list of searches:

- **Brief A** produces the paper's *literature review section*: the two
  literatures it bridges, and the gap between them.
- **Brief B** produces the material for *methods justification*. Run it only if
  the Methods section needs citations beyond the obvious ones; it is a different
  section of the paper and mixing it into A would make the review sprawl.

**Where results go:** `writing/literature/`. Export BibTeX to
`writing/literature/references.bib`. Anything outside `writing/` is invisible to
the drafting assistant.

**Run Brief A first**, and read its answer to the gap question before drafting the
introduction — if it turns out someone has already done this, the contribution
claim changes.

---

## Brief A — the literature review section

> **Topic.** Whether the European green transition will reinforce or reduce
> economic polarization between EU member states.
>
> **Research question.** Are the EU-27 member states differently positioned for
> the green transition in a way that maps onto existing core–periphery structures?
> I build a two-dimensional national typology — *vulnerability* (emission and
> energy intensity of production per unit of value added, plus fossil dependency
> of the energy mix) against *potential* (green economic complexity plus green
> patenting) — and relate the resulting groups to established European growth-model
> classifications.
>
> **What the review must cover.** Three strands, and the relationship between them:
>
> 1. **Green transition and economic divergence in Europe.** Theoretical and
>    empirical work on whether decarbonisation widens or narrows gaps between
>    European economies; uneven green industrial capabilities; carbon lock-in;
>    the just transition literature where it concerns divergence *between*
>    countries rather than within them. Include work arguing the transition is
>    convergence-friendly — I need the counter-position, not only support.
>
> 2. **European growth models and core–periphery structure.** The comparative
>    political economy literature on European growth regimes and macroeconomic
>    divergence, centred on the core / finance-led / periphery / eastern
>    "workbench" classification of Gräbner, Heimberger, Kapeller and Schütz
>    (2020), and the demand-led growth-regimes debate (Baccaro and Pontusson).
>    Include critiques of these classifications, especially of how they treat the
>    eastern catch-up economies.
>
> 3. **Economic complexity and green complexity.** The product-space and economic
>    complexity framework (Hidalgo and Hausmann), and its green extension — the
>    Green Complexity Index and Green Complexity Potential of Mealy and
>    Teytelboym (2022). Include applications to European countries or regions,
>    and methodological critiques, particularly the argument that complexity
>    indices largely capture export *diversity* rather than sophistication.
>
> **The requirement that matters most.** State explicitly and unambiguously
> whether existing work already **combines** strands 2 and 3 — that is, whether
> anyone has used economic complexity or green complexity alongside comparative
> political economy growth models, or built a country-level typology of green
> transition readiness or vulnerability for the EU. Name the closest existing work
> and say how close it is. Do not pad this with loosely related items: I need to
> know whether my combination is new.
>
> **Framing.** The review should build towards the gap between strands 2 and 3 —
> the growth-model literature describes structural divergence but not
> environmental capability, and the complexity literature measures capability but
> is not connected to political-economy structure. Write it as a synthesis
> leading to that gap, not as three separate summaries.

**Filters.** Year: leave open, or set no earlier than **2005** — the foundational
complexity work is 2009 and the growth-model literature is largely post-2015, but
a hard recent cutoff would drop the foundations. SJR: **Q1–Q2**, but be aware this
may exclude relevant JRC/EEA policy reports and working papers, which are worth a
separate manual look.

---

## Brief B — methods justification (optional, run second)

> **Topic.** Measurement choices in a country-level index of green transition
> vulnerability and capability for the EU-27.
>
> **Purpose.** I am not writing a literature review section from this — I need
> citable support for four specific methodological choices, and the strongest
> published objections to each.
>
> 1. **Production- versus consumption-based carbon accounting.** My vulnerability
>    measure is deliberately production-based, because it captures the domestic
>    *adjustment burden* — the plants, workers and energy system a country must
>    itself retool — rather than consumption *responsibility*. I report the
>    consumption-based footprint separately. I need work that argues which
>    accounting basis suits which question, the standard MRIO references
>    (EXIOBASE, EORA), and empirical work on intra-EU embodied emission transfers.
>
> 2. **Green patents as a capability measure.** The CPC Y02/Y04S climate-mitigation
>    tagging scheme and its validation; known biases, especially patent-office
>    choice (my counts are EPO filings only) and truncation from grant lag;
>    critiques of patents as an innovation indicator for countries with little
>    formal patenting.
>
> 3. **Green product lists.** How much green-complexity results depend on the
>    choice of product list — I use the OECD Combined List of Environmental Goods.
>
> 4. **Composite indicators and typology construction at small n.** Building
>    two-dimensional country maps from PCA or composite indices, and critiques of
>    composite indicators; arguments for and against discretising continuous
>    dimensions into types via median splits; appropriate inference at n = 27
>    (permutation tests, association measures for sparse tables).
>
> For each, give me both the standard reference and the strongest objection.

**Filters.** Year open. SJR Q1–Q2, plus methodological handbooks and JRC/OECD
guidance, which will not carry an SJR ranking.

---

## What NOT to ask it for

Two things are better done by hand than by prompt:

- **The counter-literature sweep** ("what would a referee cite against this").
  A review generator optimises for coherence, so it will under-report work that
  contradicts the framing. Read Brief A's output for what is *missing* rather than
  asking for objections separately.
- **Verification.** AI literature tools fabricate references. Check DOIs before
  anything enters `references.bib`. The drafting assistant cannot verify a
  citation and will use whatever is in that file.
