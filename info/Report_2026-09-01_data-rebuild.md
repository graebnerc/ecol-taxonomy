# Overnight report — EXIOBASE 3.10.2 rebuild, patent options, window scoping

**Date:** 2026-09-01 (evening session) · **Branch:** `develop` · *still unpushed to origin*

Read this first thing; the "What I need from you" section at the end is the short version.

---

## 1. What you asked for, and what happened

Three things: (a) rebuild the EXIOBASE layer on 3.10.2 including bilateral flows,
(b) implement all three green-patent options and see their implications,
(c) scope the reference-window extension. All three are done. One of them —
the patent comparison — came out cleaner than expected and effectively decides
itself.

There is also **one thing waiting for you**: the PATSTAT query. I cannot reach a
PATSTAT instance, so `sql/get_green_patents_v2.sql` and its ingestion script are
written, tested against the absent-file case, and ready to run. Details in §6.

---

## 2. Headline: the patent question answers itself

The window was capped at 2018 by EXIOBASE. With 3.10.2 covering 1995–2024 that
constraint is gone and the cap moved to **green patents** — and the reason is
worse than "truncation": `sql/get_green_patents.sql` counts EPO **grants** by
filing year, and EPO grant lag is 3–5+ years. Waiting gains roughly one usable
filing year per year of waiting, so a 2019–2023 window on grants needs a ~2028
snapshot.

The OECD ENV-TECH database carries **both** applications and grants built on the
same underlying EPO data, so the two can be compared without confounding the
comparison with a source change. Result (EU-27, per capita, 2014–2018):

| | applications ↔ grants | applications ↔ PATSTAT v1 | grants ↔ PATSTAT v1 |
|---|---:|---:|---:|
| Spearman | **0.993** | **0.988** | **0.994** |

And substituting **any** of the three into the potential axis moves **0 of 27
countries** (rank correlation on the axis 0.996).

Last year each series is still within 10% of its 2018 level:

| series | last usable filing year |
|---|---:|
| OECD applications | **2021** |
| OECD grants | 2019 |
| PATSTAT v1 grants (current headline) | **2018** |

**So the three options are empirically equivalent for the map, and applications
buy three extra years of window at no measurable cost.** Option B (applications)
is a safe swap; **option C (the hybrid) is redundant** — there is nothing left to
hedge against. That is the recommendation, and it is unusually well-supported:
the swap is justified by a 0/27 result, not by argument.

*Caveat worth stating in the paper:* applications measure filing activity, grants
measure successful innovation. They happen to rank EU-27 countries alike, which
is an empirical finding about this sample, not a conceptual identity.

`R/appendix_patent_options.R` · `plots/patent_options.png` ·
`data/tidy/patent_options.csv`

---

## 3. A defect in the current patent query

While writing the replacement query I found a real bug in the existing one.
`sql/get_green_patents.sql` is:

```sql
SELECT COUNT(appln.appln_id) ...
FROM tls201_appln appln
  JOIN tls224_appln_cpc cpc ON ...   -- one row PER CPC SYMBOL
  JOIN tls207_pers_appln pa  ON ...  -- one row PER APPLICANT PERSON
```

Its header comment states the query "considers an id only once, even if it has
more than one cpc class". **It does not** — there is no `DISTINCT`. An
application carrying *k* matching Y02/Y04S symbols and held by *m* same-country
applicants is counted *k×m* times.

This matters because the inflation is **differential across countries** (it is
larger where green patents carry more CPC tags or have more co-applicants), so it
does not cancel in a cross-country ranking — and green patents are the single
most discriminating variable in the potential axis.

Circumstantial evidence that it bites: indexed to 2018, the PATSTAT v1 series
reads 141 in 2014 while the OECD grants series on the same concept reads 106.

`sql/get_green_patents_v2.sql` fixes it with `COUNT(DISTINCT ...)` and returns
the old behaviour as `n_raw_join` so the distortion can be **measured** rather
than assumed. Running it is the only way to find out how big it is.

---

## 4. EXIOBASE 3.10.2: rebuilt in-repo, with bilateral flows

`R/get_data_exiobase.R` now computes the whole footprint layer from the official
Zenodo archives (record 20051562, v3.10.2, 1995–2024; 2.9 GB downloaded and
md5-verified). This **removes the external Python dependency** that
`info/AuditReport_2026-07-14.md` flagged as unreproducible — the footprint layer
is now computed from a named, checksummed source by code in this repo.

It produces the old file's five columns **plus** the 49×49 origin×destination
matrix of GHG embodied in final demand.

Three implementation notes worth knowing:

- **Header layout differs between core and extension tables.** pymrio writes 3
  header lines; `Z`/`Y` carry 2 index columns but `F`/`F_Y` carry 1. Getting this
  wrong silently misaligns the matrix rather than erroring, so it is explicit.
- **The emission matrices contain NA cells** (2015: one column, NA across nearly
  every stressor). These are now zeroed explicitly, with an assertion that no
  zeroed cell sits on a sector with non-zero output. The first version only
  worked because `s[x <= 0] <- 0` happened to overwrite them — correct by
  accident, which is not correct.
- **3.10.2 ships no pre-characterised `impacts` folder**, so the GWP100 basket is
  declared explicitly: AR4/IPCC-2007, all Kyoto gases, biogenic CO₂ excluded,
  biogenic CH₄/N₂O included.

All four accounting identities (`CBA = PBA − exports + imports`, world imports =
world exports, row/column sums) are **asserted**, not assumed.

### Why the numbers move

Every candidate GHG basket correlates ≈0.99 with the old extract country by
country, but **none reproduces its level** (best ratio 1.11×; world PBA 2015
44.9 Gt vs 39.2 Gt). So the gap is a genuine **3.8 → 3.10 revision of the
emission accounts**, not a basket choice — matching the old file was not a
sensible target, and the complete Kyoto basket is used instead.

Expect headline numbers to move. That was the accepted cost of the full rebuild.

<!-- RESULTS-REBUILD -->

<!-- RESULTS-OFFSHORING -->

<!-- RESULTS-WINDOW -->

---

## 6. What I need from you

### 6.1 Run the PATSTAT query (10 minutes)

```
1. run  sql/get_green_patents_v2.sql  against a current PATSTAT edition
2. save the result as  data/tidy/patstat_green-patents_v2.csv
3. Rscript R/get_data_patents_patstat.R
```

The ingestion script validates the file, quantifies both defects (the
double-counting and the grant-lag truncation), cross-checks v2 against v1 on the
mature 2014–2018 cohorts, and writes the tidy panel. If the file is absent it
prints instructions and exits without changing anything, so nothing is blocked
meanwhile.

Once it exists, `R/appendix_patent_options.R` and `R/appendix_window_options.R`
pick it up automatically and add the like-for-like PATSTAT applications series —
which is what would let the window reach 2022–2023 rather than 2021.

### 6.2 Decisions that are yours, not mine

1. **Switch the patent variable to applications?** Evidence says the swap is
   free (0/27) and buys three years. I have not made the change — it alters what
   the innovation dimension measures, so it is your call.
2. **Move the reference window?** See §5.3 for what each option costs.
3. **The EPO-only restriction** (`appln_auth = 'EP'`) excludes national patent
   offices, which plausibly understates exactly the small and eastern states in
   the low-potential tail. This was already a TODO in the old SQL. Worth a
   robustness run without the filter.

---

## 7. How to reproduce any of this

```bash
data/raw/exiobase/fetch.sh            # 2.9 GB, md5-verified (gitignored)
Rscript R/get_data_exiobase.R         # ~30 min: totals + bilateral
Rscript R/update_panel_exiobase.R     # swap the footprint layer into the panel
Rscript R/02_complexity.R             # only if the window changes
Rscript R/01_build_indicators.R
Rscript R/04_typology.R && Rscript R/06_validation.R && Rscript R/07_robustness.R
Rscript R/appendix_offshoring_origins.R
Rscript R/appendix_patent_options.R
Rscript R/appendix_window_options.R
```

Raw downloads (`data/raw/exiobase`, `data/raw/oecd`) are gitignored; everything
derived is committed.
