/* 
This query simply illustrates the duplicate issue mentioned in
"get_green_patents.sql":
The following patent is associated with more than one CPC class that
is considered "green", so without removing duplicates it would enter
the list of green patents more than once. This is most likely not desired
if we want to get a count of green patents, except we would like to 
'weight' patents by the number of applications. But this is most likely
not a good strategy since it introduces biases and if one wished to weight, 
citations would be the better way to go.
*/

SELECT 
  appln.appln_id as id,
  appln_filing_year as year,
  ps.person_ctry_code as country,
  cpc_class_symbol as cpc_class
FROM tls201_appln AS appln
  JOIN tls224_appln_cpc AS cpc ON appln.appln_id = cpc.appln_id
  JOIN tls207_pers_appln AS pa ON appln.appln_id = pa.appln_id
  JOIN tls206_person AS ps ON pa.person_id = ps.person_id
WHERE
  appln.appln_id=304 AND
  appln_auth = 'EP' AND
  (cpc_class_symbol LIKE 'Y02%' OR 
  cpc_class_symbol LIKE 'Y04S%')
