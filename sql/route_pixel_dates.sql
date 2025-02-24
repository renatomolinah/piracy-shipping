CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
WITH
  voyage_pixel_dates AS(
  SELECT
    date,
    trip_id,
    FLOOR(lat_bin/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon_bin/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.gridded_data_0_5_v_20250210`
  # Only keep data from list of filtered trips
  JOIN(SELECT trip_id FROM `emlab-gcp.piracy.keep_these_trips_v_20250210`) USING(trip_id)
  WHERE hours > 0 AND distance_km > 0
  GROUP BY
    date,
    trip_id,
    lat_bin,
    lon_bin),
  route_info AS(
  SELECT
    trip_id,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    `emlab-gcp.piracy.voyage_info_v_20250210` ),
  route_pixel_dates AS(
  SELECT
    date,
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    voyage_pixel_dates
  JOIN
    route_info
  USING
    (trip_id)
  GROUP BY
    date,
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country)
SELECT
  *
FROM
  route_pixel_dates