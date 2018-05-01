# For every voyage, get all AIS positions along the way 
# save as cargo_AIS_positions_with_ports
sql <- 
  "
SELECT
ais_info.mmsi mmsi,
ais_info.vessel_type vessel_type,
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
ais_info.avg_distance_km avg_distance_km,
ais_info.from_anchorage from_anchorage,
ais_info.from_anchorage_id from_anchorage_id,
ais_info.departure_timestamp departure_timestamp,
ais_info.to_anchorage to_anchorage,
ais_info.to_anchorage_id to_anchorage_id,
ais_info.arrival_timestamp arrival_timestamp,
(CASE WHEN ais_info.measure_new_score > 0.5 AND NOT (ais_info.distance_from_shore < 1000 AND ais_info.implied_speed < 1) AND ais_info.vessel_type = `fishing`
THEN 1
ELSE 0
END) fishing,
FROM(
SELECT
a.mmsi mmsi,
vessel_type,
fishing,
start_lat,
start_lon,
start_timestamp,
end_lat,
end_lon,
end_timestamp,
hours,
avg_distance_km,
from_anchorage,
from_anchorage_id,
departure_timestamp,
to_anchorage,
to_anchorage_id,
arrival_timestamp
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
avg_distance_km
FROM
`world-fishing-827.gfw_research.pipeline_p_p550_daily`
WHERE
mmsi IN (
SELECT
mmsi
FROM
`world-fishing-827.gfw_research.vessel_info_20180327`
WHERE
known_geartype = 'cargo'
OR known_geartype = 'tanker'
OR known_geartype = 'supply_vessel'
OR known_geartype = `tug`
OR known_geartype = `passenger`
OR known_geartype = `reefer`
OR known_geartype = `specialized_reefer`
OR known_geartype = `fish_factory`
OR known_geartype = `bunker`
OR on_fishing_list))) a
JOIN (
SELECT
mmsi,
YEAR(departure_timestamp) year,
vessel_type,
from_anchorage_name from_anchorage,
from_anchorage_id,
departure_timestamp,
to_anchorage_name to_anchorage,
to_anchorage_id,
arrival_timestamp
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
ais_info.year = cluster_info.year"

delete_table(project, "piracy", "all_cargo_AIS_positions_with_ports")
job <- insert_query_job(query = sql, project = project, destination_table = "voyage_ais_positions")
wait_for(job)