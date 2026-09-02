# writing/ — the paper drafting workspace

This folder is **self-contained by design**. The assistant drafting the paper
sees only this directory: it cannot run the analysis, read `data/tidy/`, or
verify a number against the pipeline. Everything the draft needs is therefore
physically here.

## Layout

| path | what it is | edited by |
|---|---|---|
| `results-summary.md` | the substantive briefing: what was found, how confident, where it is vulnerable | **hand-written** |
| `open-questions.md` | unresolved decisions and open analyses, with the evidence for each option | **hand-written** |
| `evidence/numbers.md` | every headline figure, with the commit it came from | **generated** |
| `evidence/*.csv` | the underlying result tables | **generated** |
| `figures/*.png` | the figures available to the paper | **generated** |
| `output/` | drafts, sections, and the assembled manuscript | the writing assistant |

## Rules for drafting

1. **Never invent or estimate a number.** Every quantitative claim must come from
   `evidence/numbers.md` or an `evidence/*.csv`. If a number the argument needs is
   not here, write `[NUMBER NEEDED: description]` and flag it — do not approximate,
   do not recompute from a different table, do not carry a figure over from memory
   of an earlier draft.
2. **`evidence/numbers.md` is authoritative.** If it disagrees with prose in
   `results-summary.md`, numbers.md wins and the prose is stale — flag it.
3. **`open-questions.md` is not settled material.** Nothing in it may be written
   into the paper as a finding. Where the draft needs a decision that is still
   open, write the passage under an explicit stated assumption and mark it.
4. **Do not soften the caveats.** §10 of `results-summary.md` and the "cannot be
   claimed" passage in §8 exist because those claims were tested and failed. They
   are load-bearing, not hedging.
5. **Write drafts into `output/`.** Leave everything else read-only.

## Regenerating the pack

The generated parts are rebuilt from the pipeline by, from the repository root:

```bash
Rscript R/build_writing_pack.R
```

Run it after any change to the analysis. It overwrites `evidence/` and
`figures/` and leaves the hand-written narrative and `output/` untouched. Each
generated file carries the date and commit it came from, so staleness is visible
rather than silent.

## Current specification

- Reference window **2017–2021**; green patents = **EPO applications**;
  emissions/value added = **EXIOBASE 3.10.2**.
- If `evidence/numbers.md` reports a different specification from this README,
  the pack has been regenerated after a pipeline change and this README is the
  stale one.
