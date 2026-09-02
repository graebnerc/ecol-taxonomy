/*
================================================================================
Green patents WITHOUT the EPO-only restriction -- robustness run.

Save the result as:  data/tidy/patstat_green-patents_allauth.csv
Then run:            Rscript R/appendix_patent_offices.R
(that script compares it against the EPO-only series and re-scores the map; it
exits cleanly with instructions if the file is absent.)

--------------------------------------------------------------------------------
WHY
--------------------------------------------------------------------------------
get_green_patents_v2.sql -- like v1 before it -- filters `appln_auth = 'EP'`, so
it counts only filings at the European Patent Office. The original file already
flagged this as a TODO ("Some countries, e.g. Bulgaria (BG), have some
applications, partly successful, to their national patent offices").

The concern is not that the count is lower; it is that the shortfall is
SYSTEMATIC. Applicants in smaller and eastern member states are likelier to file
domestically than at the EPO, so an EPO-only count understates exactly the
countries in the low-potential tail of the map -- the ones the polarization
finding rests on.

Two things make this worth checking now rather than later:

  * green patents are the most discriminating single variable in the potential
    axis (a ~14-fold spread across quadrants), and
  * the v1 -> v2 correction already demonstrated how sensitive periphery counts
    are to query details: dropping the missing DISTINCT changed counts by 2.40x
    for Slovakia and 2.34x for Greece against 1.49x for the Netherlands.

If the EPO-only restriction biases in the same direction, the two effects
compound.

--------------------------------------------------------------------------------
WHAT CHANGED vs get_green_patents_v2.sql
--------------------------------------------------------------------------------
  * `appln_auth = 'EP'` REMOVED from the WHERE clause and moved into conditional
    aggregates, so one row now carries the EPO-only counts AND the all-offices
    counts side by side. That makes the comparison exact and removes any risk of
    two separately-run queries differing for an unrelated reason.
  * added n_auth, the number of distinct filing authorities per country-year, as
    a descriptive check on how much national filing actually occurs.

Everything else is deliberately identical to v2: country list,
pa.applt_seq_nr > 0, CPC filter Y02*/Y04S*, filing years 1990-2024,
COUNT(DISTINCT ...) rather than the raw join.

--------------------------------------------------------------------------------
CAVEAT to keep in mind when reading the result
--------------------------------------------------------------------------------
All-offices counts are NOT simply "better". A domestic-only filing and an EPO
filing are not equivalent objects: EPO filings clear a higher bar and confer
European-wide protection, which is the concept the potential axis wants. So the
all-offices series is a ROBUSTNESS check on whether the ranking is an artifact of
the office filter -- not a candidate replacement for the headline. If the two
series rank countries alike, the EPO restriction is defensible and can be
footnoted. If they diverge, the divergence itself is the finding and needs
discussing rather than resolving by picking the friendlier series.
================================================================================
*/

SELECT
  appln.appln_filing_year                      AS year,
  ps.person_ctry_code                          AS country,
  -- All filing authorities
  COUNT(DISTINCT appln.appln_id)               AS n_applications_all,
  COUNT(DISTINCT CASE WHEN appln.granted = 'Y'
                      THEN appln.appln_id END) AS n_granted_all,
  -- EPO only -- reproduces get_green_patents_v2.sql exactly
  COUNT(DISTINCT CASE WHEN appln.appln_auth = 'EP'
                      THEN appln.appln_id END) AS n_applications_ep,
  COUNT(DISTINCT CASE WHEN appln.appln_auth = 'EP' AND appln.granted = 'Y'
                      THEN appln.appln_id END) AS n_granted_ep,
  -- How much filing happens outside the EPO at all
  COUNT(DISTINCT appln.appln_auth)             AS n_auth
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
  AND (cpc.cpc_class_symbol LIKE 'Y02%' OR
       cpc.cpc_class_symbol LIKE 'Y04S%')
GROUP BY appln.appln_filing_year, ps.person_ctry_code
ORDER BY year ASC, country ASC;
