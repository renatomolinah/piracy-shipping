CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
WITH
  # For each trip, get the unique lat_bin/lon_bin pixels it passed through
  voyage_pixel_dates AS(
  SELECT
    trip_id,
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.ungridded_data_v_20250210`
  # Only keep data from list of filtered trips
  JOIN(SELECT trip_id FROM `emlab-gcp.piracy.keep_these_trips_v_20250210`) USING(trip_id)
  WHERE
    hours > 0
    AND distance_km > 0
  GROUP BY
    trip_id,
    lat_bin,
    lon_bin),
  # For each trip, get it's port-to-port route info
  route_info AS(
  SELECT
    trip_id,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    `emlab-gcp.piracy.voyage_info_v_20240228` ),
  # For each port-to-port route, summarize the distict number of trips that passed along that route
  routes AS(
  SELECT
    from_port,
    from_country,
    to_port,
    to_country,
    COUNT(DISTINCT trip_id) trips_passing_through_route
  FROM
    route_info
  GROUP BY
    from_port,
    from_country,
    to_port,
    to_country),
  # For each pixel along each port-to-port route, summarize the distict number of trips that passed along that route-pixel
  route_pixels AS(
  SELECT
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country,
    COUNT(DISTINCT trip_id) trips_passing_through_route_pixel
  FROM
    voyage_pixel_dates
  JOIN
    route_info
  USING
    (trip_id)
  GROUP BY
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country)
# Now for each pixel along each port-to-port route, summarize the fraction of the total trips along that route
# that passed through the pixel
SELECT
  lon_bin,
  lat_bin,
  from_port,
  from_country,
  to_port,
  to_country,
  trips_passing_through_route_pixel,
  trips_passing_through_route,
  trips_passing_through_route_pixel / trips_passing_through_route fraction_route_trips_through_pixel
FROM
  route_pixels
LEFT JOIN
  routes
USING
  (from_port,
    from_country,
    to_port,
    to_country)