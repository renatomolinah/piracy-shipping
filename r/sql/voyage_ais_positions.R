# For every voyage, get all AIS positions along the way 
# save as cargo_AIS_positions_with_ports
# Fuel consumption based on Juan's high seas work
sql <- 
  "
SELECT
mmsi,
vessel_type,
fishing,
through_hotspot,
start_lat,
start_lon,
start_timestamp,
end_lat,
end_lon,
end_timestamp,
hours,
ais_speed,
avg_distance_km,
from_anchorage,
from_anchorage_id,
departure_timestamp,
to_anchorage,
to_anchorage_id,
arrival_timestamp,
fishing,
(hours*main_load_factor*main_sfc_low*engine_power/1000000 + hours*aux_load_factor*aux_sfc*aux_engine_power/1000000) total_fuel_consumption__low_bound,
(hours*main_load_factor*main_sfc*engine_power/1000000 + hours*aux_load_factor*aux_sfc*aux_engine_power/1000000) total_fuel_consumption__high_bound,
FROM(
SELECT
ais_info.mmsi mmsi,
ais_info.fishing fishing,
(CASE WHEN cluster_info.cluster_filter 
THEN 1
ELSE 0
END) through_hotspot,
ais_info.start_lat start_lat,
ais_info.start_lon start_lon,
ais_info.start_timestamp start_timestamp,
ais_info.end_lat end_lat,
ais_info.end_lon end_lon,
ais_info.end_timestamp end_timestamp,
ais_info.hours hours,
ais_info.ais_speed ais_speed,
ais_info.avg_distance_km avg_distance_km,
ais_info.from_anchorage from_anchorage,
ais_info.from_anchorage_id from_anchorage_id,
ais_info.departure_timestamp departure_timestamp,
ais_info.to_anchorage to_anchorage,
ais_info.to_anchorage_id to_anchorage_id,
ais_info.arrival_timestamp arrival_timestamp,
(CASE WHEN ais_info.measure_new_score > 0.5 AND NOT (ais_info.distance_from_shore < 1000 AND ais_info.implied_speed < 1) AND ais_info.vessel_type = 'fishing'
THEN 1
ELSE 0
END) fishing,
ais_info.vessel_type vessel_type,
ais_info.length length, 
ais_info.engine_power engine_power, 
ais_info.crew crew, 
ais_info.tonnage tonnage, 
ais_info.aux_engine_power aux_engine_power, 
ais_info.design_speed_ihs design_speed_ihs, 
ais_info.main_sfc main_sfc, 
ais_info.main_sfc_low main_sfc_low, 
ais_info.aux_sfc aux_sfc,
(CASE WHEN ais_info.ais_speed > ais_info.design_speed_ihs THEN 0.9*(1 + 0.285)/1.285
ELSE 0.9*(POW(ais_info.ais_speed/ais_info.design_speed_ihs, 3) + 0.285)/1.285
END) main_load_factor,
0.3 aux_load_factor,
FROM(
SELECT
a.mmsi mmsi,
fishing,
start_lat,
start_lon,
start_timestamp,
end_lat,
end_lon,
end_timestamp,
hours,
ais_speed,
avg_distance_km,
from_anchorage,
from_anchorage_id,
departure_timestamp,
to_anchorage,
to_anchorage_id,
arrival_timestamp,
vessel_type,
length, 
engine_power, 
crew, 
tonnage, 
aux_engine_power, 
design_speed_ihs, 
main_sfc, 
main_sfc_low, 
aux_sfc
FROM (
SELECT
mmsi,
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
mmsi IN (
SELECT
mmsi
FROM
`piracy.vessel_info`))) a
JOIN (
SELECT
mmsi,
YEAR(departure_timestamp) year,
from_anchorage_name from_anchorage,
from_anchorage_id,
departure_timestamp,
to_anchorage_name to_anchorage,
to_anchorage_id,
arrival_timestamp,
vessel_type,
length, 
engine_power, 
crew, 
tonnage, 
aux_engine_power, 
design_speed_ihs, 
main_sfc, 
main_sfc_low, 
aux_sfc
FROM
`ucsb-gfw.piracy.voyages_with_anchorages`) anchorages
ON
a.mmsi = anchorages.mmsi
AND a.start_timestamp > anchorages.departure_timestamp
AND a.start_timestamp < anchorages.arrival_timestamp) ais_info
LEFT JOIN (
SELECT
YEAR(year) year,
cluster_filter
FROM
[piracy.cluster_filters]) cluster_info
ON
ais_info.year = cluster_info.year)"

# source for bunker fuel data? http://www.bunkerindex.com/prices/indices.php
# probably want IFO380, IFO180, or composite index
# Fuel description https://www.transport.govt.nz/resources/tmif/transportpriceindices/ti008/

bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)