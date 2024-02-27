#standardSQL
WITH
# Get vessel info, which is pre-filtered list of cargo vessels
vessel_info AS(
  SELECT
    mmsi voyage_mmsi
  FROM
    `emlab-gcp.piracy.vessel_info` ),
  # Get anchorage, port, and country info for start of voyage
  from_anchorage_info AS(
  SELECT
    s2id from_anchorage_id,
    label from_port,
    iso3 from_country,
    ST_GEOGPOINT(lon,
      lat) from_anchorage_position
  FROM
    `world-fishing-827.gfw_research.named_anchorages`),
  # Get anchorage, port, and country info for end of voyage
  to_anchorage_info AS(
  SELECT
    s2id to_anchorage_id,
    label to_port,
    iso3 to_country,
    ST_GEOGPOINT(lon,
      lat) to_anchorage_position
  FROM
    `world-fishing-827.gfw_research.named_anchorages`),
    # Get all data from 'current' voyages table, which includes data for the last 10 years
  # At the time of this query in February 2024, this archive version includes 2014-2024 data
  # Using highest confidence voyages - see https://github.com/GlobalFishingWatch/bigquery-documentation-wf827/wiki/Anchorages-and-voyages
  voyages_base AS (
  SELECT
    ssvid voyage_mmsi,
    trip_start departure_timestamp,
    trip_end arrival_timestamp,
    trip_start_anchorage_id from_anchorage_id,
    trip_end_anchorage_id to_anchorage_id,
    trip_id
  FROM
    `world-fishing-827.pipe_production_v20201001.proto_voyages_c4`
  WHERE
    trip_start_confidence = 4
    AND trip_end_confidence = 4
   AND trip_start >= '2013-01-01'
   AND trip_end <= '2021-12-31'),
  # Get all voyage data from 'archive' table, which includes data prior to last 10 years
  # At the time of this query in February 2024, this archive version includes 2013 data
voyages_base_archive AS (
  SELECT
    CAST(ssvid AS INT64) voyage_mmsi,
    trip_start departure_timestamp,
    trip_end arrival_timestamp,
    trip_start_anchorage_id from_anchorage_id,
    trip_end_anchorage_id to_anchorage_id,
    trip_id
  FROM
    `world-fishing-827.pipe_production_v20201001.archive_proto_voyages_c4`
  WHERE
    trip_start_confidence = 4
    AND trip_end_confidence = 4
    ND trip_start >= '2013-01-01'
   AND trip_end <= '2021-12-31'),
  all_voyages AS(
    SELECT
    *
    FROM
    voyages_base
    UNION ALL
    (SELECT * FROM voyages_base_archive)
  )
  # Combine voyage info with anchorages, ports, and countries
SELECT
  * EXCEPT(from_anchorage_position,to_anchorage_position),
    ST_DISTANCE(from_anchorage_position,
      to_anchorage_position)/1000 total_haversine_distance_km
FROM
  all_voyages
JOIN
vessel_info
USING(voyage_mmsi)
LEFT JOIN
  from_anchorage_info
USING
  (from_anchorage_id)
LEFT JOIN
  to_anchorage_info
USING
  (to_anchorage_id)
WHERE
  NOT from_port = to_port