# Description:
# This generates all of the necessary data on each voyage (so each row is a trip_id), 
# and only for those vessels found in vessel_info. 
#standardSQL
WITH
  # Get vessel info, which is pre-filtered list of cargo vessels
  vessel_info AS(
  SELECT
    mmsi
  FROM
    `emlab-gcp.piracy.{vessel_info_table}` ),
  # Get anchorage, port, and country info for starting anchorage of voyage
  from_anchorage_info AS(
  SELECT
    s2id from_anchorage_id,
    label from_port,
    iso3 from_country,
    ST_GEOGPOINT(lon, lat) from_anchorage_position
  FROM
    `global-fishing-watch.anchorages.named_anchorages`),
  # Get anchorage, port, and country info for ending anchorage of voyage
  to_anchorage_info AS(
  SELECT
    s2id to_anchorage_id,
    label to_port,
    iso3 to_country,
    ST_GEOGPOINT(lon, lat) to_anchorage_position
  FROM
    `global-fishing-watch.anchorages.named_anchorages`),
  # Get all voyage data
  # Use highest confidence voyages - see https://github.com/GlobalFishingWatch/bigquery-documentation-wf827/wiki/Anchorages-and-voyages
  voyages_base AS (
  SELECT
    ssvid mmsi,
    trip_start departure_timestamp,
    trip_end arrival_timestamp,
    trip_start_anchorage_id from_anchorage_id,
    trip_end_anchorage_id to_anchorage_id,
    trip_id
  FROM
    `global-fishing-watch.pipe_ais_v3_published.voyages_c4`
  WHERE
    trip_start_confidence = 4
    AND trip_end_confidence = 4
    # Only pull trips for our study period
    AND trip_start >= '{study_period_starting_date}'
    AND trip_start <= '{study_period_ending_date}'
    AND trip_end <= '{study_period_ending_date}')
  # Join the voyage info with vessel info,
  # as well as starting anchorage info and ending achorage info
SELECT
# There are some duplicates in  archive_proto_voyages_c4
# Ensure no duplicates enter the final table, and that there is only one row per trip_id
  DISTINCT * EXCEPT(from_anchorage_position,
    to_anchorage_position),
  ST_DISTANCE(from_anchorage_position,
    # set use_spheroid as TRUE so that the function measures distance on the surface of the WGS84 spheroid
    to_anchorage_position, TRUE)/1000 total_haversine_distance_km
FROM
  voyages_base
JOIN
  vessel_info
USING
  (mmsi)
LEFT JOIN
  from_anchorage_info
USING
  (from_anchorage_id)
LEFT JOIN
  to_anchorage_info
USING
  (to_anchorage_id)
  # Filter to only voyages that have different starting and ending ports
WHERE
  NOT (from_port = to_port
    AND from_country = to_country)