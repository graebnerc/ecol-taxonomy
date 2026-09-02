# CLAUDE.md — paper drafting workspace

Guidance for drafting the WP1 paper. **These instructions override default behaviour.**

## Where you are

This directory is the entire workspace. You cannot run the analysis, read the
pipeline, or verify a number against source data — none of it is visible to you.
Everything the paper needs is physically here, and anything that is not here does
not exist for your purposes.

That constraint is the reason for the rules below. They are not stylistic.

## The rules

**1. Never invent, estimate, or recompute a number.**
Every quantitative claim must come verbatim from `evidence/numbers.md` or an
`evidence/*.csv`. If the argument needs a figure that is not there, write

```
[NUMBER NEEDED: what it is, and which table it would come from]
```

and keep going. Do not approximate from a related figure, do not derive one
number from others, do not carry a value over from an earlier draft, and do not
round differently than the source. A plausible-looking wrong number is the single
most damaging thing you can produce here, because nobody downstream can catch it
by reading.

**2. `evidence/numbers.md` is authoritative.**
If it disagrees with prose in `results-summary.md`, numbers.md is right and the
prose is stale — use numbers.md and flag the discrepancy in your response.

**3. Nothing in `open-questions.md` is settled.**
It records unresolved decisions. Never write any of it into the paper as a
finding. Where a passage depends on an open decision, write it under an
explicitly stated assumption and mark the assumption inline.

**4. Do not soften the caveats.**
`results-summary.md` §10, the "cannot be claimed" passage in §8, and the
"statistically indistinguishable" passage in §5 record claims that were **tested
and failed**. They are load-bearing findings, not defensive hedging. Weakening
them to make the argument flow better would misrepresent the evidence. Likewise
do not upgrade a "moderate" association into a strong one.

**5. Register: descriptive, n = 27.**
Everything is descriptive association — Cramér's V, partial correlations,
Monte-Carlo Fisher. There is no large-sample inference to be had. Avoid causal
verbs ("drives", "causes", "leads to") unless the source text uses them.

**6. Citations must come from `literature/`.** If it is not in
`literature/references.bib` or the notes there, do not cite it. Never generate a
citation from memory — write `[CITATION NEEDED: what the claim is]` instead. This
is the same rule as rule 1, for the same reason: you cannot verify, so a
plausible-looking fabricated reference is the most damaging thing you can produce.
If `literature/` is empty, the review has not been run yet — flag that rather than
filling the gap.

**7. Write into `output/` only.** Treat everything else as read-only. If
`results-summary.md` or `open-questions.md` needs correcting, say so rather than
editing it — those are maintained against the pipeline you cannot see.

## What is here

| path | contents | maintained by |
|---|---|---|
| `results-summary.md` | the substantive briefing: design, data, results, robustness, caveats | hand-written |
| `open-questions.md` | unresolved decisions, ordered by how much each gates the draft | hand-written |
| `evidence/numbers.md` | every headline figure, stamped with the commit it came from | generated |
| `evidence/*.csv` | the underlying result tables | generated |
| `figures/*.png` | the figures available to the paper | generated |
| `literature-prompts.md` | search prompts for the literature review | hand-written |
| `literature/` | search results, notes, `references.bib` | populated externally |
| `output/` | drafts, sections, assembled manuscript | you |

Read `results-summary.md` first; it is the briefing. `evidence/numbers.md` is the
lookup table you return to for every figure you cite.

## The paper in one paragraph

Work Package 1 of the OPUS project *"The green transition and economic
polarization in Europe"*. It builds a typology of the EU-27 on two axes —
**vulnerability** (transition burden) and **potential** (green capability) — each
built from two dimensions, and asks whether the green transition risks
reinforcing existing core–periphery polarization. The novelty is applying green
economic complexity (Mealy & Teytelboym 2022) to this question; the method is
theirs, the application is the contribution.

## Two things that will trip you up

**The headline finding has a boundary.** All six Core countries are Winners and 8
of 11 Workbench economies are At risk, which supports the polarization story. But
the three non-Core groups are **statistically indistinguishable from each other**
— the pattern is Core vs everyone else, not four differentiated development models. Do
not write the plural claim. See `results-summary.md` §5.

**The offshoring story was refuted and reframed.** An earlier version claimed the
core's clean production profile rests on offshoring to the European East. The
bilateral data killed it: ~10% of the Core's embodied imports come from the
Workbench East, ~69% from outside the EU. The surviving claim is that the East's
burden is its **own** carbon-intensive production, which makes the polarization
structural rather than a transfer. If you find yourself writing the transfer
story, you are working from a superseded framing. See `results-summary.md` §8.

## Style

- English prose. Economics/ecological-economics register.
- Cite figures as `figures/<name>.png`; the caption should state what the reader
  should take from it, not restate the axes.
- Numbers in text: match the precision used in `evidence/numbers.md`.
- Country groups follow Gräbner et al. (2020): Core / Finance / Periphery /
  Workbench. Quadrants are Winners / Exposed but capable / Low-stakes / At risk.
- Call them **development models**, never "growth models". That is the author's
  term for this classification, and the author is a co-author of it. The adjacent
  Baccaro–Pontusson literature does call its objects growth models — related but
  a distinct research programme, so do not blur the two.
- The classification is **the author's own prior work**. Do not write the paper as
  validating it: the four-way split adds nothing over a Core-vs-rest binary here
  (`results-summary.md` §5). The paper *uses and refines* it. Where the
  development-model results appear, lead with that null before the positive
  composition and coherence findings.
