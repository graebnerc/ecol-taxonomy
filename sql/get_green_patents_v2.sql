/*
================================================================================
Green patents at the EPO, EU-27 + comparators -- REPLACEMENT for
get_green_patents.sql. Run this once against a current PATSTAT edition; it
returns everything the taxonomy needs in a single pass.

Save the result as:  data/tidy/patstat_green-patents_v2.csv
Then run:            Rscript R/get_data_patents_patstat.R
(that script validates the file and reports what changed; it fails loudly rather
than silently if a column or year is missing.)

--------------------------------------------------------------------------------
WHY A NEW QUERY -- two independent problems with get_green_patents.sql
--------------------------------------------------------------------------------

(1) DOUBLE COUNTING.  The old query is

        SELECT COUNT(appln.appln_id) ...
        FROM tls201_appln appln
          JOIN tls224_appln_cpc cpc ON ...      -- one row PER CPC SYMBOL
          JOIN tls207_pers_appln pa  ON ...     -- one row PER APPLICANT PERSON
          JOIN tls206_person ps      ON ...

    Its header comment states the query "considers an id only once, even if it
    has more than one cpc class". That is NOT what the SQL does: without
    DISTINCT, an application carrying k matching Y02/Y04S symbols and held by m
    applicants from the same country is counted k*m times. The inflation is
    differential -- it is larger for countries whose green patents carry more
    CPC tags or have more co-applicants -- so it does not cancel out in a
    cross-country ranking. Green patents are the most discriminating variable in
    the potential axis, so this matters.

    Fixed below with COUNT(DISTINCT appln.appln_id). The old behaviour is
    retained as n_raw_join so the size of the distortion can be measured rather
    than assumed.

(2) GRANT-LAG TRUNCATION.  The old query filters `granted = 'Y'` and counts by
    appln_filing_year. EPO grant lag is 3-5+ years, so recent filing cohorts are
    severely undercounted. Measured on the current extract (EU-27 totals):
    2018 = 5204, 2019 = 3781, 2020 = 1863, 2021 = 490, 2022 = 20, 2023 = 0.
    Waiting does not fix it -- roughly one usable filing year is gained per year
    of waiting, so a 2019-2023 window needs a ~2028 snapshot.

    Fixed below by returning applications AND grants side by side, from the same
    scan, so the reference window can move to recent years on the applications
    measure while the grants measure remains available for the headline and for
    validating that the two rank countries alike.

--------------------------------------------------------------------------------
WHAT CHANGED vs get_green_patents.sql  (everything else is deliberately identical
so the two series stay comparable)
--------------------------------------------------------------------------------
  * COUNT(appln.appln_id)  ->  COUNT(DISTINCT appln.appln_id)
  * `granted = 'Y'` moved OUT of the WHERE clause and INTO a conditional
    aggregate, so one row now carries both measures
  * appln_filing_year upper bound 2022 -> 2024
  * added n_raw_join (old, non-distinct behaviour) for the diagnostic
  * added n_applicants / n_cpc_matches to quantify the multiplicity directly

Unchanged: country list, appln_auth = 'EP', pa.applt_seq_nr > 0 (applicants
only, not inventor-only persons), CPC filter Y02* or Y04S*, GROUP BY
filing year x applicant country.

--------------------------------------------------------------------------------
KNOWN LIMITATION, unchanged and still worth discussing (was a TODO in the old
file): appln_auth = 'EP' counts only EPO filings and excludes national patent
offices. This systematically understates countries whose applicants file
domestically rather than at the EPO -- plausibly the smaller and eastern member
states, i.e. exactly the low-potential tail of the map. Consider running the
query a second time without the appln_auth filter as a robustness check.
================================================================================
*/

SELECT
  appln.appln_filing_year                     AS year,
  ps.person_ctry_code                         AS country,
  -- Headline measures: each application counted ONCE per country-year.
  COUNT(DISTINCT appln.appln_id)              AS n_applications,
  COUNT(DISTINCT CASE WHEN appln.granted = 'Y'
                      THEN appln.appln_id END) AS n_granted,
  -- Diagnostics: reproduce and explain the old inflated count.
  COUNT(appln.appln_id)                       AS n_raw_join,
  COUNT(DISTINCT pa.person_id)                AS n_applicants,
  COUNT(DISTINCT cpc.cpc_class_symbol)        AS n_cpc_matches
FROM tls201_appln AS appln
  JOIN tls224_appln_cpc AS cpc ON appln.appln_id = cpc.appln_id
  JOIN tls207_pers_appln AS pa ON appln.appln_id = pa.appln_id
  JOIN tls206_person     AS ps ON pa.person_id   = ps.person_id
WHERE
  ps.person_ctry_code IN ('AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES',
                          'FI', 'FR', 'GR', 'HR', 'HU', 'IE', 'IT', 'LT', 'LU',
                          'LV', 'MT', 'NL', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK',
                          'GB', 'US', 'JP', 'CN', 'CA', 'KR', 'BR', 'IN', 'MX',
                          'RU', 'AU', 'CH', 'TR', 'TW', 'NO', 'ID', 'ZA', 'WF')
  AND pa.applt_seq_nr > 0          -- applicants only, exclude inventor-only persons
  AND appln.appln_filing_year >= 1990
  AND appln.appln_filing_year <= 2024
  AND appln.appln_auth = 'EP'
  AND (cpc.cpc_class_symbol LIKE 'Y02%' OR
       cpc.cpc_class_symbol LIKE 'Y04S%')
GROUP BY appln.appln_filing_year, ps.person_ctry_code
ORDER BY year ASC, country ASC;
