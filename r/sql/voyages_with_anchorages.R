# First, get all trips for vessels of interest
# Trip defined by beginning and ending anchorage and timestamps
# Save as trips_with_port_labels
sql <-
  "
SELECT
anchorages.mmsi mmsi,
anchorages.vessel_type vessel_type,
b.label from_port,
anchorages.from_anchorage_id from_anchorage_id,
anchorages.from_anchorage_name from_anchorage_name,
anchorages.departure_timestamp departure_timestamp,
c.label to_port,
anchorages.to_anchorage_id to_anchorage_id,
anchorages.to_anchorage_name to_anchorage_name,
anchorages.arrival_timestamp arrival_timestamp
FROM (
SELECT
anchor1.mmsi mmsi,
anchor2.vessel_type vessel_type,
anchor2.anchorage_id from_anchorage_id,
anchor2.anchorage_name from_anchorage_name,
anchor2.event_end departure_timestamp,
anchor1.anchorage_id to_anchorage_id,
anchor1.anchorage_name to_anchorage_name,
anchor1.event_start arrival_timestamp
FROM (
SELECT
vessel_1_id mmsi,
event_start,
event_end,
anchorage_id,
anchorage_name,
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
FROM (
(SELECT
vessel_1_id mmsi,
event_start,
event_end,
anchorage_id,
anchorage_name,
ROW_NUMBER() OVER (PARTITION BY vessel_1_id ORDER BY event_start) rn
FROM
`world-fishing-827.gfw_research.voyage_events_all_vessels_20180307`
WHERE
event_type = 'anchorage') voy_info
LEFT JOIN(
SELECT
mmsi mmsi_info,
vessel_type
FROM
`piracy.vessel_info`) ves_info
ON voy_info.mmsi = ves_info.mmsi_info)) anchor2
ON
anchor1.mmsi = anchor2.mmsi
AND anchor1.rn = anchor2.rn_plus) anchorages
LEFT JOIN (
SELECT
s2id,
label
FROM
`world-fishing-827.gfw_research.named_anchorages_20171120`) b
ON
anchorages.from_anchorage_id = b.s2id
LEFT JOIN (
SELECT
s2id,
label
FROM
`world-fishing-827.gfw_research.named_anchorages_20171120`) c
ON
anchorages.to_anchorage_id = c.s2id"

bq_table(project = project,table = "cargo_trips_with_port_labels",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,sql, destination_table = bq_table(project = project,table = "voyages_with_anchorages",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)
