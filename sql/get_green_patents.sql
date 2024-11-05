/*
This downloads data on successful applications for green patents
between 1990 and 2022.

On the issue of duplicates in PATSTAT
========================================
Without any statement on duplicates, an id enters the list more then
once when it has assigned more than one cpc_class that falls into
our search strings. 

Right now the query considers an id only once, even if it is assigned 
more then one cpc class we are interested in because this fits better the goal that we 
have with the "number of green patents". Otherwise, a patent would count as two patents
if it has two fitting cpc class codes.
One example to illustrate the problen is the patent application with id=304.
More details on this see query "illustrate_cpc-duplicates.sql"

On aggregation
===============
This query returns aggregated data. If individual applications were desired, 
the group by and the COUNT() statement would need to be removed.

On patent offices
==================
Some countries, e.g. Bulgaria (BG), have some applications, partly successful, to 
their national patent offices. They are excluded in the query below, which only
considers applications at the European Patent Office. This most likely makes sense,
since what counts is also patent protection at the European level, but it might
be relevant to discuss.
TODO Discuss patent office issue

Further todos
===============
TODO We could check the classification to get a more adequate count of green patents.
*/ 

SELECT 
  COUNT(appln.appln_id) as n_patents,
  appln_filing_year as year,
  ps.person_ctry_code as country
FROM tls201_appln AS appln
  JOIN tls224_appln_cpc AS cpc ON appln.appln_id = cpc.appln_id
  JOIN tls207_pers_appln AS pa ON appln.appln_id = pa.appln_id
  JOIN tls206_person AS ps ON pa.person_id = ps.person_id
WHERE
  ps.person_ctry_code IN ('AT', 'BE', 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'ES', 'FI', 'FR', 'GR', 'HR', 'HU', 'IE', 'IT', 'LT', 'LU', 'LV', 'MT', 'NL', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK', 'GB', 'US', 'JP', 'CN', 'CA', 'KR', 'BR', 'IN', 'MX', 'RU', 'AU', 'CH', 'TR', 'TW', 'NO', 'ID', 'ZA', 'WF') AND
  pa.applt_seq_nr > 0 AND -- limit result to applicants and to exclude persons which are inventors only
  appln_filing_year >= 1990 AND 
  appln_filing_year <= 2022 AND 
  appln_auth = 'EP' AND
  granted = 'Y' AND
  (cpc_class_symbol LIKE 'Y02%' OR 
  cpc_class_symbol LIKE 'Y04S%')
GROUP BY appln_filing_year, ps.person_ctry_code
ORDER BY year ASC;
