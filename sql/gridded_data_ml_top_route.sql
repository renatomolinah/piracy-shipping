WITH
  top_routes AS(
  SELECT
    from_port,
    to_port,
    COUNT(*) number_trips
  FROM
    `emlab-gcp.piracy.gridded_data_ml`
  GROUP BY
    from_port,
    to_port
  ORDER BY
    number_trips DESC),
  top_route AS(
  SELECT
    from_port,
    to_port
  FROM
    top_routes
  LIMIT
    1),
  all_route_data AS (
  SELECT
    *
  FROM
    `emlab-gcp.piracy.gridded_data_ml`
  JOIN
    top_route
  USING
    (from_port,
      to_port)),
  all_route_data_first_day AS(
  SELECT
    trip_id,
    lat_bin,
    lon_bin,
    MIN(date) date
  FROM
    all_route_data
  GROUP BY
    trip_id,
    lat_bin,
    lon_bin)
SELECT
  *
FROM
  all_route_data
JOIN
  all_route_data_first_day
USING
  (date,
    trip_id,
    lat_bin,
    lon_bin)