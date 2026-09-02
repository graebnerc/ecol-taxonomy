# Literature search prompts

For running against Paperguide (or any AI literature assistant). Written against
the paper's actual findings, so the searches return things the argument can use
rather than a generic reading list.

**Where results should land:** `writing/literature/` — the drafting assistant can
only see `writing/`, so anything kept outside it is invisible to the draft. Export
BibTeX to `writing/literature/references.bib` if the tool supports it.

**Order matters.** Prompt 1 can change what the paper claims to contribute, so run
it before the others and before drafting the introduction.

---

## 1. NOVELTY CHECK — run this first

> I am writing a paper that builds a typology of the EU-27 for the green
> transition, on two axes: **vulnerability** (emission and energy intensity of
> production per unit value added, plus fossil dependency of the energy mix) and
> **potential** (green economic complexity in the sense of Mealy & Teytelboym
> 2022, plus green patenting). I then relate the resulting quadrants to the
> Gräbner et al. (2020) European growth-model classification (core, finance,
> periphery, workbench) to ask whether the green transition risks reinforcing
> core–periphery polarization.
>
> Find work that has already done any of the following, and say clearly for each
> whether it overlaps my design or is distinct:
> 1. applied **green economic complexity or green product space** methods to
>    European countries or regions;
> 2. built a **country- or region-level typology of green transition readiness,
>    vulnerability, or exposure** in the EU;
> 3. combined **economic complexity with comparative political economy growth
>    models or varieties-of-capitalism**;
> 4. asked whether **decarbonisation reinforces core–periphery divergence** within
>    the EU.
>
> I need to know whether my combination is genuinely new or whether someone has
> published it. Be blunt about the closest existing work — do not pad the list
> with loosely related items.

## 2. Core framing: green transition and European polarization

> Find literature on whether the European green transition is likely to
> **reinforce or reduce economic divergence between EU member states**. I am
> interested in:
> - theoretical arguments for divergence (uneven capabilities, lock-in,
>   agglomeration in green sectors);
> - empirical work on the distributional effects of decarbonisation across EU
>   countries or regions;
> - the just transition literature where it deals with *between-country* rather
>   than within-country distribution;
> - critiques arguing the transition is convergence-friendly.
>
> I specifically want the counter-position as well as the supporting one.

## 3. Growth models and the Gräbner et al. classification

> Find work using or critiquing the **European growth-model classification** of
> Gräbner, Heimberger, Kapeller and Schütz (2020) — core / finance-led /
> periphery / workbench (eastern catch-up) economies. I want:
> - papers that apply the classification to a new domain (I am applying it to
>   environmental/transition outcomes);
> - critiques of the classification itself, especially of the grouping of the
>   eastern "workbench" economies;
> - the broader comparative political economy debate on European growth models
>   and macroeconomic divergence (Baccaro & Pontusson and the demand-led growth
>   regimes literature).

## 4. Green economic complexity — provenance and critiques

> Find the methodological literature on **green economic complexity**: the Green
> Complexity Index and Green Complexity Potential of Mealy & Teytelboym (2022),
> and the underlying economic complexity / product space framework of Hidalgo and
> Hausmann.
>
> I especially need **critiques and validity concerns**, because one applies
> directly to my results:
> - work showing complexity indices are largely capturing **export diversity**
>   rather than sophistication (in my EU-27 sample the Green Complexity Index
>   correlates 0.997 with a simple count of green products exported with RCA ≥ 1,
>   though only 0.74 with total export diversity);
> - debates over the ECI/method-of-reflections and its interpretation;
> - the choice of **green product lists** (I use the OECD Combined List of
>   Environmental Goods, 244 HS6 codes; Mealy & Teytelboym use 293) and how much
>   results depend on it.

## 5. Production- vs consumption-based accounting

> Find literature on **production-based versus consumption-based carbon
> accounting** and carbon leakage, focused on what I need to justify a
> methodological choice.
>
> My vulnerability axis is deliberately **production-based**, because it measures
> the domestic *adjustment burden* — the plants, workers and energy system a
> country must itself retool — rather than consumption *responsibility*. I report
> the consumption-based footprint separately as an interpretive layer.
>
> I want:
> - the standard references for MRIO consumption-based accounting (EXIOBASE,
>   EORA, Peters, Wiedmann and colleagues);
> - work that explicitly argues **which accounting basis is appropriate for which
>   question**, particularly for transition burden or adjustment cost as opposed
>   to responsibility or fairness;
> - empirical work on **intra-EU** carbon leakage or embodied emission transfers
>   between member states, since I find only ~10% of the core's embodied imports
>   originate in the eastern member states and ~69% come from outside the EU.

## 6. Green patents as a capability measure — and their biases

> Find literature on using **patents to measure green or environmental innovation
> capability**, and on the known biases of doing so. I need this because green
> patents are the single most discriminating variable in my potential axis.
>
> Specifically:
> - the CPC Y02/Y04S climate-change-mitigation tagging scheme and its validation;
> - **home-office and office-choice bias**: my counts are EPO filings only, and I
>   find EPO share of all green filings is 12.8% for eastern member states against
>   21.2% for core countries, so the restriction understates the periphery in
>   levels (though it does not change my country ranking);
> - applications versus grants as the unit of analysis, and truncation from grant
>   lag;
> - critiques of patents as an innovation indicator for countries with little
>   formal patenting activity.

## 7. Typology construction and small-N methods

> Find methodological literature on **constructing country typologies from
> continuous indicators**, relevant to a sample of n = 27.
>
> - the use of **PCA or composite indicators** to build two-dimensional country
>   maps, and critiques of composite indicator construction (the OECD/JRC
>   handbook and its critics);
> - guidance on **median splits and discretising continuous dimensions** into
>   types — including arguments against doing so;
> - appropriate **inference at very small n**: permutation and randomisation tests
>   for group differences, and association measures for sparse contingency tables
>   (Cramér's V, bias correction, Monte-Carlo Fisher);
> - comparative political economy work that builds typologies and how it handles
>   borderline cases.

## 8. Counter-literature — what a referee would cite against this

> Act as a critical referee. Given a paper that (a) builds a two-axis EU-27
> typology from six indicators, (b) uses green economic complexity as a capability
> measure, and (c) concludes the green transition risks reinforcing core–periphery
> polarization, find the literature that would be used to **attack** it:
>
> - work arguing composite indicators and typologies obscure more than they reveal;
> - work questioning whether export-based complexity measures capture productive
>   capability at all;
> - evidence that eastern EU member states are converging rather than diverging on
>   green capability or decarbonisation;
> - arguments that country-level analysis is the wrong unit because transition
>   burdens are regional or sectoral rather than national.
>
> I would rather find these now than in a referee report.

---

## Notes on using the results

- **Prompt 1 is the one that matters most.** If it turns up a paper doing
  substantially the same thing, the contribution claim has to change, and that
  affects the introduction, the framing decision in `open-questions.md` §1, and
  possibly the target journal.
- **Prompts 4, 5 and 6 map onto specific caveats already in `results-summary.md`**
  (§10 on GCI-as-diversity, §8 on burden vs responsibility, §8d on the EPO
  restriction). Each needs a citation to stand as a defended choice rather than an
  admission.
- **Prompt 7 supports §8f** (quadrant stability) and the small-n register.
- Be sceptical of anything the tool returns that you cannot locate independently —
  AI literature tools do fabricate references. Verify DOIs before anything enters
  the bibliography.
