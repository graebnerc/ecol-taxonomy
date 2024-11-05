SELECT 
  appln.appln_id as id,
  appln_filing_year as year,
  ps.person_ctry_code as country,
  cpc_class_symbol as cpc_class,
  granted,
  appln_auth
FROM tls201_appln AS appln
  JOIN tls224_appln_cpc AS cpc ON appln.appln_id = cpc.appln_id
  JOIN tls207_pers_appln AS pa ON appln.appln_id = pa.appln_id
  JOIN tls206_person AS ps ON pa.person_id = ps.person_id
WHERE
  ps.person_ctry_code IN ('BG') AND
  appln_filing_year=1995 AND
  pa.applt_seq_nr > 0 AND -- limit result to applicants and to exclude persons which are inventors only
  (cpc_class_symbol LIKE 'Y02%' OR 
  cpc_class_symbol LIKE 'Y04S%')
ORDER BY year ASC;

