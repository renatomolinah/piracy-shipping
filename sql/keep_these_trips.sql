WITH
  trip_info AS(
  SELECT
    trip_id,
    from_port,
    to_port,
    from_country,
    to_country
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
    SUM(hours) hours
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
    to_country),
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
  trip_id
FROM
  total_distance_by_trip
JOIN
  mean_distance_per_route
USING
  (from_port,
    to_port,
    from_country,
    to_country)
WHERE
  # Remove trips that have 0 distance
  distance_km > 0
  # Remove trips that have 0 hours
  AND hours > 0
  # Remove trips longer than 60 days
  AND (hours / 24 <= 60)
  # Remove trips with a distance greater than the earth's circumference
  # Circumfrence measured at equato (source: https://en.wikipedia.org/wiki/Earth%27s_circumference)
  AND (distance_km <= 40075.017)
  # Remove trips have a distance greater than 4x the mean distance for that port-to-port route
  AND distance_km <= average_route_distance_km * 4