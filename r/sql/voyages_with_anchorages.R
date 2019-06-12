# First, get all trips for vessels of interest
# Trip defined by beginning and ending anchorage and timestamps
# Save as trips_with_port_labels

# New query
sql <-"#standardSQL
  WITH from_anchorage_info AS(
  SELECT
    s2id from_anchorage_id,
    label from_port,
    iso3 from_country,
    lat from_anchorage_lat,
    lon from_anchorage_lon
  FROM
    `world-fishing-827.gfw_research.named_anchorages_v20190307`),
  to_anchorage_info AS(
  SELECT
    s2id to_anchorage_id,
    label to_port,
    iso3 to_country,
    lat to_anchorage_lat,
    lon to_anchorage_lon
  FROM
    `world-fishing-827.gfw_research.named_anchorages_v20190307`),
  master AS(
  SELECT
    mmsi,
    year,
    departure_timestamp,
    arrival_timestamp,
    from_anchorage_id,
    to_anchorage_id,
    trip_id
  FROM
    `ucsb-gfw.piracy.vessel_info`
  JOIN (
    SELECT
      CAST(ssvid AS INT64) mmsi,
      EXTRACT(year
      FROM
        trip_start ) year,
      trip_start departure_timestamp,
      trip_end arrival_timestamp,
      trip_start_anchorage_id from_anchorage_id,
      trip_end_anchorage_id to_anchorage_id,
      trip_id
    FROM
      `world-fishing-827.pipe_production_v20190502.voyages`) USING(mmsi,
      year))
SELECT
  *
FROM
  master
LEFT JOIN
  from_anchorage_info USING(from_anchorage_id)
LEFT JOIN
  to_anchorage_info USING(to_anchorage_id)"

bq_table(project = project,table = "voyages_with_anchorages",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,sql, destination_table = bq_table(project = project,table = "voyages_with_anchorages",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)
