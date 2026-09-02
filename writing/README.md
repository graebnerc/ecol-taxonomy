# writing/ — paper drafting workspace

Self-contained pack for drafting the WP1 paper. An assistant working here has
access to this directory alone: it cannot run the analysis or verify a number
against the pipeline, so everything the draft needs is physically present.

**The operative instructions are in [`CLAUDE.md`](CLAUDE.md)** — read that, not
this file. It is loaded automatically by Claude Code when the working directory
is `writing/`.

| path | contents |
|---|---|
| `CLAUDE.md` | drafting rules and orientation (**authoritative**) |
| `results-summary.md` | the substantive briefing |
| `open-questions.md` | unresolved decisions, with the evidence for each |
| `evidence/` | result tables + `numbers.md` (generated) |
| `figures/` | the figures available to the paper (generated) |
| `output/` | drafts |

## Regenerating

From the repository root, after any change to the analysis:

```bash
Rscript R/build_writing_pack.R
```

It rewrites `evidence/` and `figures/` and leaves `CLAUDE.md`,
`results-summary.md`, `open-questions.md` and `output/` untouched. Every
generated file is stamped with the date and commit it came from, so staleness is
visible rather than silent.

Current specification: reference window **2017–2021**, green patents = **EPO
applications** (PATSTAT), emissions/value added = **EXIOBASE 3.10.2**. If
`evidence/numbers.md` says otherwise, the pack was regenerated after a pipeline
change and this file is the stale one.
