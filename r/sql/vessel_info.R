# First, get all relevant vessel info
# Get all unique vessels that self-identify as cargo, tanker, or reefer
# Limit options to those that have at least 50 vessels in the data set
sql <- "#standardSQL
SELECT
  CAST(ssvid AS INT64) mmsi,
  year,
  best.best_flag flag,
  best.best_vessel_class vessel_type,
  best.best_length_m length,
  best.best_tonnage_gt tonnage,
  best.best_engine_power_kw engine_power,
  best.best_crew_size crew,
  # From Bren GP, page 130
  # Based on linear regression of known vessels
  # https://www.bren.ucsb.edu/research/documents/whales_report.pdf
  0.1913 * best.best_engine_power_kw + 287.2 aux_engine_power,
  # Add design speed
  # From Bren GP, page 131
  # Based on linear regression of known vessels
  # https://www.bren.ucsb.edu/research/documents/whales_report.pdf
  3.39*POW(10,-4)*best.best_engine_power_kw+2.151*POW(10,-5)*best.best_tonnage_gt-2.742*POW(10,-9)*best.best_engine_power_kw*best.best_tonnage_gt+12.93 design_speed,
  # Add SFC from here:https://www.sciencedirect.com/science/article/pii/S1361920909001072#bib18
  206 main_sfc,
  221 aux_sfc
FROM
  `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
WHERE
  best.best_vessel_class IN('cargo',
    'cargo_or_tanker',
    'tanker',
    'cargo_or_reefer',
    'specialized_reefer',
    'container_reefer',
    'reefer')
AND
year < 2018"

# Upload to big query
bq_table(project = project,table = "vessel_info",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, 
                 destination_table = bq_table(project = project,table = "vessel_info",dataset = "piracy"),
                 allowLargeResults = TRUE)

sql<-"
WITH
master_ais AS(
 SELECT
   CAST (ssvid AS INT64) mmsi,
   year,
   (CASE
       WHEN ais_type.value IN ('AIS.1',  'AIS.2',  'AIS.3') THEN 'A'
       WHEN ais_type.value IN ('AIS.18',
       'AIS.19') THEN 'B'
       ELSE NULL END) type,
   ais_type.count count
 FROM
   `world-fishing-827.gfw_research.vi_ssvid_byyear_v20190430`
 CROSS JOIN
   UNNEST(activity.position_type) AS ais_type ),
  grouped_ais AS(
 SELECT
   mmsi,
   year,
   type,
   SUM(count) count,
   ROW_NUMBER() OVER(PARTITION BY mmsi, year ORDER BY SUM(count) DESC) AS rank
 FROM
   master_ais
 GROUP BY
   mmsi,
   year,
   type),
 final_ais AS(
 SELECT
   mmsi,
   year,
   type ais_type
 FROM
   grouped_ais
 WHERE
   rank = 1),
 shipping_vessels AS(
 SELECT
 mmsi,
 year
 FROM
 `ucsb-gfw.piracy.vessel_info`)
 SELECT * FROM final_ais
 JOIN
 shipping_vessels
 USING(mmsi,year)"

bq_project_query(project,sql, 
                 destination_table = bq_table(project = project,table = "vessel_info_ais_type",dataset = "piracy"),use_legacy_sql = FALSE, allowLargeResults = TRUE,write_disposition = "WRITE_TRUNCATE")
