# Superseded exploratory scripts (archived 2026-09-03)

Both were written in July 2026 against the 2014–2018 window and the EXIOBASE
3.8.x vintage. Both have since been answered better elsewhere, and leaving them
in `R/` was actively confusing: they carried frozen years in their titles and
figures, so a reader could mistake their output for a current result.

## `appendix_capability_trajectory.R`

Asked whether green capability converged between 2014–18 and 2019–23 by
recomputing GCI on a recent window.

**Superseded by `R/appendix_capability_convergence.R`**, which asks the same
question properly: a rolling series of 5-year windows rather than two endpoints,
the patent measure held fixed across them, and the change decomposed into its
GCI / GCP / patent components. That analysis found the apparent convergence is
**an artifact** — complexity was never income-linked, and the fall in
R²(potential ~ income) is mostly a skew effect in the untransformed patent
variable. The two-endpoint version could not have detected any of that.

## `appendix_vulnerability_drift.R`

Built a *parallel territorial* vulnerability proxy (Eurostat national GHG and
final energy per unit real GDP) because — in its own words — "the paper's
vulnerability axis is frozen at 2019 (EXIOBASE)", so the real axis could not
move over time.

**That premise is now false.** EXIOBASE 3.10.2 gives complete emissions to 2022,
so the actual vulnerability axis can be computed over time and no proxy is
needed. And the question it was built to answer is already settled:
`R/appendix_window_options.R` rebuilds the entire typology on windows through
2020–2022 and finds **0 of 27** countries change quadrant. There is no drift to
detect.

## Their outputs

`capability_trajectory.csv`, `green_complexity_recent.csv`, `map_drift.csv` and
the corresponding plots are archived alongside. None was ever copied into
`writing/`, so nothing in the paper's evidence pack depended on them.
