#standardSQL
WITH
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
  # Get voyage info
  # Using highest confidence voyages - see https://github.com/GlobalFishingWatch/bigquery-documentation-wf827/wiki/Anchorages-and-voyages
  voyages_base AS (
  SELECT
    CAST(ssvid AS INT64) voyage_mmsi,
    trip_start departure_timestamp,
    trip_end arrival_timestamp,
    trip_start_anchorage_id from_anchorage_id,
    trip_end_anchorage_id to_anchorage_id,
    trip_id
  FROM
    `world-fishing-827.pipe_production_v20201001.proto_voyages_c4`
  WHERE
    trip_start_confidence = 4
    AND trip_end_confidence = 4)
  # Combine voyage info with anchorages, ports, and countries
SELECT
  * EXCEPT(from_anchorage_position,to_anchorage_position),
    ST_DISTANCE(from_anchorage_position,
      to_anchorage_position)/1000 total_haversine_distance_km
FROM
  voyages_base
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