# First, get all trips for vessels of interest
# Trip defined by beginning and ending anchorage and timestamps
# Save as trips_with_port_labels

sql <-
  "
#standardSQL
SELECT
anchorages.mmsi mmsi,
anchorages.from_anchorage_id from_anchorage_id,
CONCAT(b.label,
'-',
b.anchor_group) from_anchorage_name,
b.label from_port_name,
anchorages.departure_timestamp departure_timestamp,
anchorages.to_anchorage_id to_anchorage_id,
CONCAT(c.label,
'-',
c.anchor_group) to_anchorage_name,
c.label to_port_name,
anchorages.arrival_timestamp arrival_timestamp,
anchorages.vessel_type vessel_type,
anchorages.length length,
anchorages.engine_power engine_power,
anchorages.crew crew,
anchorages.tonnage tonnage,
anchorages.aux_engine_power aux_engine_power,
anchorages.design_speed design_speed,
anchorages.main_sfc main_sfc,
anchorages.aux_sfc aux_sfc
FROM (
SELECT
anchor1.mmsi mmsi,
anchor2.vessel_type vessel_type,
anchor2.length length,
anchor2.engine_power engine_power,
anchor2.crew crew,
anchor2.tonnage tonnage,
anchor2.aux_engine_power aux_engine_power,
anchor2.design_speed design_speed,
anchor2.main_sfc main_sfc,
anchor2.aux_sfc aux_sfc,
anchor2.anchorage_id from_anchorage_id,
anchor2.event_end departure_timestamp,
anchor1.anchorage_id to_anchorage_id,
anchor1.event_start arrival_timestamp
FROM (
SELECT
vessel_1_id mmsi,
event_start,
event_end,
anchorage_id,
ROW_NUMBER() OVER (PARTITION BY vessel_1_id ORDER BY event_start) rn
FROM
`world-fishing-827.gfw_research.voyage_events_all_vessels_20180307`
WHERE
event_type = 'anchorage'
AND vessel_1_id IN (
SELECT
mmsi
FROM
`piracy.vessel_info`)) anchor1
JOIN (
SELECT
*,
rn+1 rn_plus
FROM ( (
SELECT
vessel_1_id mmsi,
EXTRACT(YEAR FROM event_start) AS year,
event_start,
event_end,
anchorage_id,
ROW_NUMBER() OVER (PARTITION BY vessel_1_id ORDER BY event_start) rn
FROM
`world-fishing-827.gfw_research.voyage_events_all_vessels_20180307`
WHERE
event_type = 'anchorage') voy_info
LEFT JOIN (
SELECT
mmsi mmsi_info,
year year_info,
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
`piracy.vessel_info`) ves_info
ON
voy_info.mmsi = ves_info.mmsi_info
AND voy_info.year = ves_info.year_info)) anchor2
ON
anchor1.mmsi = anchor2.mmsi
AND anchor1.rn = anchor2.rn_plus) anchorages
LEFT JOIN (
SELECT
s2id,
label,
anchor_group
FROM
`world-fishing-827.gfw_research.named_anchorages_20171120`) b
ON
anchorages.from_anchorage_id = b.s2id
LEFT JOIN (
SELECT
s2id,
label,
anchor_group
FROM
`world-fishing-827.gfw_research.named_anchorages_20171120`) c
ON
anchorages.to_anchorage_id = c.s2id"

bq_table(project = project,table = "voyages_with_anchorages",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,sql, destination_table = bq_table(project = project,table = "voyages_with_anchorages",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)
