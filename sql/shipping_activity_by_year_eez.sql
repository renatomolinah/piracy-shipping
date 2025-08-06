# Description
# This gives gives aggregate shipping activity statistics by year and EEZ
# which is necessary for the IV analysis
WITH
  base AS(
  SELECT
    FORMAT("lon:%+07.2f_lat:%+07.2f", ROUND(lon/0.01)*0.01, ROUND(lat/0.01)*0.01) AS gridcode,
    mmsi,
    hours,
    distance_km,
    EXTRACT(YEAR
    FROM
      timestamp) year
  FROM
    `emlab-gcp.piracy.{ungridded_data_table}`
  # Only keep data from list of filtered trips
  JOIN(SELECT trip_id FROM `emlab-gcp.piracy.{keep_these_trips_table}`) USING(trip_id)),
  eez AS(
  SELECT
    gridcode,
    IFNULL(eez_id, 'high_seas') eez_id
  FROM
    world-fishing-827.pipe_static.spatial_measures_20201105
  LEFT JOIN
    UNNEST(regions.eez) eez_id),
  # Since grids are at 0.01x0.01 degree resolution, some of them match to multiple EEZs
  # for each gridcode, count the number of EEZs
  # We will then partition out hours and distance_km equally across EEZs for each gridcode
  number_eez_per_gridcode AS(
  SELECT
    gridcode,
    COUNT(*) number_eezs
  FROM
    eez
  GROUP BY
    gridcode ),
  eez_info AS(
  SELECT
    CAST(eez_id AS STRING) eez_id,
    sovereign1_iso3,
    sovereign2_iso3,
    sovereign3_iso3,
    territory1_iso3,
    territory2_iso3,
    territory3_iso3
  FROM
    `world-fishing-827.gfw_research.eez_info` ),
  joined AS(
  SELECT
    *
  FROM
    base
  JOIN
    eez
  USING
    (gridcode)
  LEFT JOIN
    eez_info
  USING
    (eez_id)
  LEFT JOIN
    number_eez_per_gridcode
  USING
    (gridcode))
SELECT
  year,
  eez_id,
  sovereign1_iso3,
  sovereign2_iso3,
  sovereign3_iso3,
  territory1_iso3,
  territory2_iso3,
  territory3_iso3,
  SUM(hours/number_eezs) hours,
  SUM(distance_km/number_eezs) distance_km,
  COUNT(DISTINCT(mmsi)) distinct_vessels
FROM
  joined
GROUP BY
  year,
  eez_id,
  sovereign1_iso3,
  sovereign2_iso3,
  sovereign3_iso3,
  territory1_iso3,
  territory2_iso3,
  territory3_iso3