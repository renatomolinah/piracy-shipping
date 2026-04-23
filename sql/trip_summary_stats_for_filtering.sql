WITH
  trip_info AS(
  SELECT
    trip_id,
    from_port,
    to_port,
    from_country,
    to_country,
    total_haversine_distance_km
  FROM
    `emlab-gcp.piracy.{voyage_info_table}` ),
  total_distance_by_trip AS(
  SELECT
    trip_id,
    from_port,
    to_port,
    from_country,
    to_country,
    SUM(distance_km) distance_km,
    SUM(hours) hours,
    COUNT(*) number_of_pings,
    total_haversine_distance_km
  FROM
    `emlab-gcp.piracy.{ungridded_data_table}`
  LEFT JOIN
    trip_info
  USING
    (trip_id)
  GROUP BY
    trip_id,
    from_port,
    to_port,
    from_country,
    to_country,
    total_haversine_distance_km),
  mean_distance_per_route AS(
  SELECT
    AVG(distance_km) average_route_distance_km,
    from_port,
    to_port,
    from_country,
    to_country
  FROM
    total_distance_by_trip
  GROUP BY
    from_port,
    to_port,
    from_country,
    to_country)
SELECT
  *
FROM
total_distance_by_trip
JOIN
  mean_distance_per_route
USING
  (from_port,
    to_port,
    from_country,
    to_country)