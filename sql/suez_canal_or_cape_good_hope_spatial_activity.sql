# standardSQL
CREATE TEMPORARY FUNCTION pixel_size()
AS (
  {pixel_size}
);

SELECT
  EXTRACT(YEAR FROM timestamp) year,
  trip_location_flag,
  FLOOR(lat / pixel_size()) * pixel_size() lat_bin,
  FLOOR(lon / pixel_size()) * pixel_size() lon_bin,
  SUM(hours) hours,
  SUM(distance_km) distance_km,
  COUNT(DISTINCT mmsi) distinct_vessels,
  COUNT(DISTINCT trip_id) distinct_trips
FROM `emlab-gcp.piracy.{suez_canal_or_cape_good_hope_pings_table}`
WHERE EXTRACT(YEAR FROM timestamp) IN (2012,2023)
GROUP BY
  year,
  trip_location_flag,
  lat_bin,
  lon_bin
