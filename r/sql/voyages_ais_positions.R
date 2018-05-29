# For every voyage, get all AIS positions along the way 
# save as cargo_AIS_positions_with_ports
# Fuel consumption based on Juan's high seas work
# Lots of engine parameters from https://www.bren.ucsb.edu/research/documents/whales_report.pdf
# Use main load factor of 0.8
# Aux load factor of 0.5
# Main SFC of 206 g/kWh
# Aux SFC of 221 g/kWh
glue::glue("
#standardSQL
  WITH ping_info AS (
SELECT
mmsi,
measure_new_score,
distance_from_shore,
implied_speed,
lat start_lat,
lon start_lon,
timestamp start_timestamp,
next_lat end_lat,
next_lon end_lon,
next_timestamp end_timestamp,
hours,
ais_speed,
avg_distance_km
FROM
`world-fishing-827.gfw_research.pipeline_p_p550_daily`
WHERE
lat < 90
AND lat > -90
AND lon < 180
AND lon >-180
AND mmsi IN (
SELECT
mmsi
FROM
`piracy.vessel_info`)),
voyage_info AS(
SELECT
mmsi voy_mmsi,
EXTRACT(YEAR
FROM
departure_timestamp) AS year,
from_anchorage_id,
from_anchorage_name,
from_port_name,
departure_timestamp,
to_anchorage_id,
to_anchorage_name,
to_port_name,
arrival_timestamp,
vessel_type,
length,
engine_power,
crew,
tonnage,
aux_engine_power,
design_speed,
main_sfc,
aux_sfc
FROM
`ucsb-gfw.piracy.voyages_with_anchorages`),
ais_info AS(
SELECT
*,
0.8 * POW(ais_speed/design_speed, 3) main_load_factor,
0.5 aux_load_factor
FROM
voyage_info
LEFT JOIN
ping_info
ON
voyage_info.voy_mmsi = ping_info.mmsi
AND ping_info.start_timestamp > voyage_info.departure_timestamp
AND ping_info.start_timestamp < voyage_info.arrival_timestamp)
SELECT
*,
(CASE
WHEN measure_new_score > 0.5 AND NOT (distance_from_shore < 1000 AND implied_speed < 1) AND vessel_type = 'fishing' THEN 1
ELSE 0 END) fishing,
(hours*main_load_factor*206*engine_power/1000000 + hours*aux_load_factor*221*aux_engine_power/1000000) total_fuel_consumption,
(CASE
WHEN {cluster_filters2} THEN 1
ELSE 0 END) through_hotspot
FROM
ais_info
"
)

bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)
           