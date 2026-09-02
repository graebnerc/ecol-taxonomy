/*
================================================================================
Green patents WITHOUT the EPO-only restriction -- robustness run.

  SAVE THE RESULT AS:  data/raw/get_green_patents_v3.csv
                       (data/tidy/patstat_green-patents_allauth.csv also works)
  THEN RUN:            Rscript R/appendix_patent_offices.R

The ingestion script validates the file, cross-checks it against the v2 extract,
re-scores the map, and exits cleanly with instructions if the file is absent.

--------------------------------------------------------------------------------
WHY
--------------------------------------------------------------------------------
get_green_patents_v2.sql filters `appln_auth = 'EP'`, counting only filings at
the European Patent Office. The original v1 file already flagged this as a TODO
("Some countries, e.g. Bulgaria (BG), have some applications, partly successful,
to their national patent offices").

The concern is not that the count is lower; it is that the shortfall may be
SYSTEMATIC. Applicants in smaller and eastern member states are likelier to file
domestically, so an EPO-only count would understate exactly the countries in the
low-potential tail of the map -- the ones the polarization finding rests on.
Green patents are the most discriminating single variable in the potential axis
(~14-fold spread across quadrants), and the v1 -> v2 correction already showed
how sensitive periphery counts are to query details: fixing the missing DISTINCT
changed counts by 2.40x for Slovakia and 2.34x for Greece against 1.49x for the
Netherlands.

--------------------------------------------------------------------------------
PERFORMANCE -- READ THIS BEFORE RUNNING
--------------------------------------------------------------------------------
Removing `appln_auth = 'EP'` is expensive: it opens the query to every patent
office worldwide. Three things keep it tractable, and all three matter:

  1. EU-27 APPLICANTS ONLY. The comparator countries in the v1/v2 queries
     (US, JP, CN, KR, ...) are not used anywhere in the taxonomy, and they are
     the bulk of global patenting. Dropping them removes most of the scan and
     costs nothing analytically.

  2. FILING YEARS 2010-2024. The taxonomy window is 2017-2021; the earlier years
     are only needed to show the truncation profile. 1990 onwards is 20 extra
     years of scan for no use here.

  3. CPC MATCHED WITH `EXISTS`, NOT A JOIN. Joining tls224_appln_cpc multiplies
     every application by its number of matching Y02/Y04S symbols before the
     aggregation. `EXISTS` stops at the first match instead. The result is
     IDENTICAL -- COUNT(DISTINCT appln_id) collapses the duplicates either way --
     but the intermediate result set is far smaller.

If it still runs long, add `AND appln.appln_filing_year BETWEEN 2016 AND 2022`
and run it in year batches; the ingestion script accepts a partial series and
reports the coverage it found.

--------------------------------------------------------------------------------
CORRECTNESS CHECK BUILT IN
--------------------------------------------------------------------------------
The query returns EPO-only and all-offices counts SIDE BY SIDE from one scan, so
the two cannot drift apart the way two separately-run queries could. The
`n_applications_ep` column should reproduce `n_applications` from
get_green_patents_v2.sql exactly, for every country-year in the overlap.
R/appendix_patent_offices.R asserts precisely that on load: if the two disagree,
something in this query differs from v2 and the comparison is void. That check is
the reason the EPO columns are here at all.

--------------------------------------------------------------------------------
WHAT THIS IS NOT
--------------------------------------------------------------------------------
All-offices counts are NOT "better". A domestic filing and an EPO filing are not
equivalent objects: EPO filings clear a higher bar and confer European-wide
protection, which is the concept the potential axis wants. This is a robustness
check on whether the RANKING is an artifact of the office filter -- not a
candidate replacement for the headline. If the two series diverge, the divergence
is the finding and needs discussing, not resolving by adopting whichever series
is friendlier to the argument.
================================================================================
*/

SELECT
  appln.appln_filing_year                      AS year,
  ps.person_ctry_code                          AS country,
  -- All filing authorities
  COUNT(DISTINCT appln.appln_id)               AS n_applications_all,
  COUNT(DISTINCT CASE WHEN appln.granted = 'Y'
                      THEN appln.appln_id END) AS n_granted_all,
  -- EPO only -- must reproduce get_green_patents_v2.sql exactly
  COUNT(DISTINCT CASE WHEN appln.appln_auth = 'EP'
                      THEN appln.appln_id END) AS n_applications_ep,
  COUNT(DISTINCT CASE WHEN appln.appln_auth = 'EP' AND appln.granted = 'Y'
                      THEN appln.appln_id END) AS n_granted_ep,
  -- How many distinct offices the country's applicants actually use
  COUNT(DISTINCT appln.appln_auth)             AS n_auth
FROM tls201_appln AS appln
  JOIN tls207_pers_appln AS pa ON appln.appln_id = pa.appln_id
  JOIN tls206_person     AS ps ON pa.person_id   = ps.person_id
WHERE
  ps.person_ctry_code IN ('AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES',
                          'FI', 'FR', 'GR', 'HR', 'HU', 'IE', 'IT', 'LT', 'LU',
                          'LV', 'MT', 'NL', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK')
  AND pa.applt_seq_nr > 0          -- applicants only, exclude inventor-only persons
  AND appln.appln_filing_year >= 2010
  AND appln.appln_filing_year <= 2024
  AND EXISTS (
        SELECT 1
        FROM tls224_appln_cpc AS cpc
        WHERE cpc.appln_id = appln.appln_id
          AND (cpc.cpc_class_symbol LIKE 'Y02%' OR
               cpc.cpc_class_symbol LIKE 'Y04S%'))
GROUP BY appln.appln_filing_year, ps.person_ctry_code
ORDER BY year ASC, country ASC;


/*
================================================================================
OPTIONAL SECOND QUERY -- which offices, exactly?

Only worth running if the main query shows a divergence between the EPO-only and
all-offices rankings. It names the authorities behind it, which is what turns
"the ranking changes" into an explanation. Cheap once restricted as below.

Save as data/raw/get_green_patents_v3_by_office.csv; the ingestion script picks
it up automatically if present and reports the top offices per country.
================================================================================

SELECT
  ps.person_ctry_code               AS country,
  appln.appln_auth                  AS office,
  COUNT(DISTINCT appln.appln_id)    AS n_applications
FROM tls201_appln AS appln
  JOIN tls207_pers_appln AS pa ON appln.appln_id = pa.appln_id
  JOIN tls206_person     AS ps ON pa.person_id   = ps.person_id
WHERE
  ps.person_ctry_code IN ('AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES',
                          'FI', 'FR', 'GR', 'HR', 'HU', 'IE', 'IT', 'LT', 'LU',
                          'LV', 'MT', 'NL', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK')
  AND pa.applt_seq_nr > 0
  AND appln.appln_filing_year BETWEEN 2017 AND 2021
  AND EXISTS (
        SELECT 1
        FROM tls224_appln_cpc AS cpc
        WHERE cpc.appln_id = appln.appln_id
          AND (cpc.cpc_class_symbol LIKE 'Y02%' OR
               cpc.cpc_class_symbol LIKE 'Y04S%'))
GROUP BY ps.person_ctry_code, appln.appln_auth
ORDER BY country ASC, n_applications DESC;
*/
