#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
WITH
  # For all attacks, get lat/lon into lat_bin/lon_bin using pixel_size
  gridded_attack_info AS(
  SELECT
    date attack_date,
    reference,
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.{asam_data_table}`),
  # For each trip_id, get the route departure port/country and arrival port/country
  voyage_info AS(
  SELECT
    trip_id,
    DATE(departure_timestamp) voyage_departure_date,
    from_port,
    from_country,
    to_port,
    to_country,
  FROM
    `emlab-gcp.piracy.{voyage_info_table}`
  JOIN(SELECT trip_id FROM `emlab-gcp.piracy.{keep_these_trips_table}`) USING(trip_id)),
  # For each route and departure date, get all of the unique lat_bin/lon_bin grids that all trips pass through
  # For any given route and date window, this set of grids represents the spatial area over which ship captains will look at recent attacks
  grids_per_trip_departure_date_and_route AS(
  SELECT
    departure_date,
    from_port,
    from_country,
    to_port,
    to_country,
    lat_bin,
    lon_bin
  FROM
    `emlab-gcp.piracy.{gridded_data_table}`
  LEFT JOIN
    voyage_info
  USING
    (trip_id)
  GROUP BY
    departure_date,
    from_port,
    from_country,
    to_port,
    to_country,
    lat_bin,
    lon_bin ),
  # For each trip_id, find all previous lat_bin/lon_bin grids that trips along that route passed through
  # Also include grids that the trip actually passes through
  # And for each grid, calculate the most recent number of days since the grid was passed through
  # This will allow us to filter to only those grids that fall within a rolling temporal window
  all_route_grids_prior_to_trip AS(
  SELECT
    trip_id,
    departure_date,
    MIN(DATE_DIFF(voyage_info.voyage_departure_date, grids_per_trip_departure_date_and_route.departure_date, DAY)) days_since_grid_was_passed_through,
    lat_bin,
    lon_bin
  FROM
    voyage_info
  JOIN
    grids_per_trip_departure_date_and_route
  ON
    grids_per_trip_departure_date_and_route.departure_date <= voyage_info.voyage_departure_date
    AND voyage_info.from_port = grids_per_trip_departure_date_and_route.from_port
    AND voyage_info.to_port = grids_per_trip_departure_date_and_route.to_port
    AND voyage_info.from_country = grids_per_trip_departure_date_and_route.from_country
    AND voyage_info.to_country = grids_per_trip_departure_date_and_route.to_country
  GROUP BY
    trip_id,
    departure_date,
    lat_bin,
    lon_bin),
  # For each trip_id, find all grids that trips along that route passed through in the past 15 days
  # Then count up the number of unique attacks that occurred within those grids in the 15 days prior to the trip's departure date
    total_unique_attacks_in_route_grids_prior_to_trip_past_15_days AS (
  SELECT
    trip_id,
    COUNT(DISTINCT(reference)) total_route_attacks_last_15_days
  FROM
    all_route_grids_prior_to_trip
  LEFT JOIN
    gridded_attack_info
  USING
    (lat_bin,
      lon_bin)
  WHERE
    # Only count attacks occurred prior to departure date
    attack_date < departure_date
    # Only count attacks that happened at most 15 days before the trip's departure date
    AND DATE_DIFF(departure_date, attack_date, DAY) <= 15
    # And count attacks in route grids that were passed through at most 15 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 15
  GROUP BY
    trip_id),
  total_unique_attacks_in_route_grids_prior_to_trip_past_1_month AS (
  SELECT
    trip_id,
    COUNT(DISTINCT(reference)) total_route_attacks_last_1_month
  FROM
    all_route_grids_prior_to_trip
  LEFT JOIN
    gridded_attack_info
  USING
    (lat_bin,
      lon_bin)
  WHERE
    # Only count attacks occurred prior to departure date
    attack_date < departure_date
    # Only count attacks that happened at most 30 days before the trip's departure date
    AND DATE_DIFF(departure_date, attack_date, DAY) <= 30
    # And count attacks in route grids that were passed through at most 30 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 30
  GROUP BY
    trip_id),
  # Same as above, but for 3 months
  total_unique_attacks_in_route_grids_prior_to_trip_past_3_months AS (
  SELECT
    trip_id,
    COUNT(DISTINCT(reference)) total_route_attacks_last_3_months
  FROM
    all_route_grids_prior_to_trip
  LEFT JOIN
    gridded_attack_info
  USING
    (lat_bin,
      lon_bin)
  WHERE
    # Only count attacks occurred prior to departure date
    attack_date < departure_date
    # Only count attacks that happened at most 90 days before the trip's departure date
    AND DATE_DIFF(departure_date, attack_date, DAY) <= 90
    # And count attacks in route grids that were passed through at most 90 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 90
  GROUP BY
    trip_id),
  # Same as above, but for 6 months
  total_unique_attacks_in_route_grids_prior_to_trip_past_6_months AS (
  SELECT
    trip_id,
    COUNT(DISTINCT(reference)) total_route_attacks_last_6_months
  FROM
    all_route_grids_prior_to_trip
  LEFT JOIN
    gridded_attack_info
  USING
    (lat_bin,
      lon_bin)
  WHERE
    # Only count attacks occurred prior to departure date
    attack_date < departure_date
    # Only count attacks that happened at most 180 days before the trip's departure date
    AND DATE_DIFF(departure_date, attack_date, DAY) <= 180
    # And count attacks in route grids that were passed through at most 180 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 180
  GROUP BY
    trip_id),
  # Same as above, but for 12 months
  total_unique_attacks_in_route_grids_prior_to_trip_past_12_months AS (
  SELECT
    trip_id,
    COUNT(DISTINCT(reference)) total_route_attacks_last_12_months
  FROM
    all_route_grids_prior_to_trip
  LEFT JOIN
    gridded_attack_info
  USING
    (lat_bin,
      lon_bin)
  WHERE
    # Only count attacks occurred prior to departure date
    attack_date < departure_date
    # Only count attacks that happened at most 365 days before the trip's departure date
    AND DATE_DIFF(departure_date, attack_date, DAY) <= 365
    # And count attacks in route grids that were passed through at most 365 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 365
  GROUP BY
    trip_id)
# Now build all indicators
# Start with voyage_info, so we get full suite of indicators for every trip_id
# We can replace NULL values with 0s, since even trips with no previous trips along that route
# can count attacks over the grids that the trip actually passes through. If no attacks are counted,
# it's a true 0
SELECT
  trip_id,
  IFNULL(total_route_attacks_last_15_days, 0) total_route_attacks_last_15_days_{pixel_size}_degrees,
  IFNULL(total_route_attacks_last_1_month, 0) total_route_attacks_last_1_month_{pixel_size}_degrees,
  IFNULL(total_route_attacks_last_3_months, 0) total_route_attacks_last_3_months_{pixel_size}_degrees,
  IFNULL(total_route_attacks_last_6_months, 0) total_route_attacks_last_6_months_{pixel_size}_degrees,
  IFNULL(total_route_attacks_last_12_months,0) total_route_attacks_last_12_months_{pixel_size}_degrees
FROM
  voyage_info
LEFT JOIN
  total_unique_attacks_in_route_grids_prior_to_trip_past_15_days
USING
  (trip_id)
LEFT JOIN
  total_unique_attacks_in_route_grids_prior_to_trip_past_1_month
USING
  (trip_id)
LEFT JOIN
  total_unique_attacks_in_route_grids_prior_to_trip_past_3_months
USING
  (trip_id)
LEFT JOIN
  total_unique_attacks_in_route_grids_prior_to_trip_past_6_months
USING
  (trip_id)
LEFT JOIN
  total_unique_attacks_in_route_grids_prior_to_trip_past_12_months
USING
  (trip_id)