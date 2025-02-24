#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
WITH
  # For all attacks, get lat/lon into lat_bin/lon_bin using pixel_size
  # Then for each grid and attack date, summarize number of attacks
  gridded_attack_info AS(
  SELECT
    date attack_date,
    COUNT(DISTINCT asam_reference) number_attacks,
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.asam_data_v_20250210`
  WHERE
    # Do not need any attacks before 2012, since 12 month is our longest attack window
    # and 2013-01-01 is the start of our study
   date >= '2012-01-01'
  GROUP BY
  attack_date,
  lat_bin,
  lon_bin),
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
    `emlab-gcp.piracy.voyage_info_v_20250210`
  JOIN(SELECT trip_id FROM `emlab-gcp.piracy.keep_these_trips_v_20250210`) USING(trip_id)),
  # For each route and departure date, get all of the unique lat_bin/lon_bin grids that all previous trips pass through, by previous_trip_id
  # For any given route and date window, this set of trip-by-grids represents the spatial areas over which each recent trip will add up recent attacks
  grids_per_trip_departure_date_and_route AS(
  SELECT
    trip_id previous_trip_id,
    departure_date,
    from_port,
    from_country,
    to_port,
    to_country,
    lat_bin,
    lon_bin
  FROM
    {ifelse(pixel_size == 5,'`emlab-gcp.piracy.gridded_data_5_v_20250210`','`emlab-gcp.piracy.gridded_data_3_v_20250210`')}
  LEFT JOIN
    voyage_info
  USING
    (trip_id)
  GROUP BY
    previous_trip_id,
    departure_date,
    from_port,
    from_country,
    to_port,
    to_country,
    lat_bin,
    lon_bin ),
  # For each trip_id, find all previous lat_bin/lon_bin grids, by previous_trip_id, that trips along that route passed through
  # And for each grid and previous_trip_id, calculate the most recent number of days since the grid was passed through
  # This will allow us to filter to only those grids and previous_trip_id that fall within a rolling temporal window
  all_route_grid_trips_prior_to_trip AS(
  SELECT
    trip_id,
    previous_trip_id,
    departure_date,
    MIN(DATE_DIFF(voyage_info.voyage_departure_date, grids_per_trip_departure_date_and_route.departure_date, DAY)) days_since_grid_was_passed_through,
    lat_bin,
    lon_bin
  FROM
    voyage_info
  JOIN
    grids_per_trip_departure_date_and_route
  ON
    # Unlike the total_rolling_attack_* query, here we're only interested in the grids from previous trips, not the current trip
    grids_per_trip_departure_date_and_route.departure_date < voyage_info.voyage_departure_date
    # Don't need to get trips that were more than 12 months prior - our maximum rolling window for attacks is 12 months
    AND DATE_DIFF(voyage_info.voyage_departure_date, grids_per_trip_departure_date_and_route.departure_date, DAY) <= 365
    AND voyage_info.from_port = grids_per_trip_departure_date_and_route.from_port
    AND voyage_info.to_port = grids_per_trip_departure_date_and_route.to_port
    AND voyage_info.from_country = grids_per_trip_departure_date_and_route.from_country
    AND voyage_info.to_country = grids_per_trip_departure_date_and_route.to_country
  GROUP BY
    trip_id,
    previous_trip_id,
    departure_date,
    lat_bin,
    lon_bin),
  # Now add attack info to this
  all_route_grid_trips_prior_to_trip_with_attacks AS(
  SELECT
    * EXCEPT(number_attacks),
    # Replace missing values with 0, since these are true 0s
    IFNULL(number_attacks,0) number_attacks
  FROM
    all_route_grid_trips_prior_to_trip
  LEFT JOIN
    gridded_attack_info
  USING
    (lat_bin,
      lon_bin)
  WHERE
    # Only count attacks occurred prior to departure date
    attack_date < departure_date ),
  # For each trip_id and previous_trip_id, find all grids that previous trips along that route passed through in the past 3 months
  # Then count up the number of unique attacks that occurred within those previous trip in the 3 months prior to the trip's departure date
  # We can replace NULL values with 0s
  total_unique_attacks_in_route_grids_prior_to_trip_past_3_months_by_previous_trip_id AS (
  SELECT
    trip_id,
    previous_trip_id,
    SUM(number_attacks) total_route_attacks_last_3_months
  FROM
    all_route_grid_trips_prior_to_trip_with_attacks
  WHERE
    # Only count attacks that happened at most 90 days before the trip's departure date
    DATE_DIFF(departure_date, attack_date, DAY) <= 90
    # And count attacks in route grids that were passed through at most 90 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 90
  GROUP BY
    trip_id,
    previous_trip_id),
  # Same as above, but for 6 months
  total_unique_attacks_in_route_grids_prior_to_trip_past_6_months_by_previous_trip_id AS (
  SELECT
    trip_id,
    previous_trip_id,
    SUM(number_attacks) total_route_attacks_last_6_months
  FROM
    all_route_grid_trips_prior_to_trip_with_attacks
  WHERE
    # Only count attacks that happened at most 180 days before the trip's departure date
    DATE_DIFF(departure_date, attack_date, DAY) <= 180
    # And count attacks in route grids that were passed through at most 180 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 180
  GROUP BY
    trip_id,
    previous_trip_id),
  # Same as above, but for 12 months
  total_unique_attacks_in_route_grids_prior_to_trip_past_12_months_by_previous_trip_id AS (
  SELECT
    trip_id,
    previous_trip_id,
    SUM(number_attacks) total_route_attacks_last_12_months
  FROM
    all_route_grid_trips_prior_to_trip_with_attacks
  WHERE
    # Only count attacks that happened at most 365 days before the trip's departure date
    DATE_DIFF(departure_date, attack_date, DAY) <= 365
    # And count attacks in route grids that were passed through at most 365 days before the trip's departure date
    AND days_since_grid_was_passed_through <= 365
  GROUP BY
    trip_id,
    previous_trip_id),
  # Now for each trip_id, find average number of attacks across previous_trip_ids
  # Start with previous 3 month indicator
  average_unique_attacks_in_route_grids_prior_to_trip_past_3_months AS(
  SELECT
    trip_id,
    AVG(total_route_attacks_last_3_months) average_route_attacks_last_3_months
  FROM
    total_unique_attacks_in_route_grids_prior_to_trip_past_3_months_by_previous_trip_id
  GROUP BY
    trip_id ),
  # Now do same for 6 months
  average_unique_attacks_in_route_grids_prior_to_trip_past_6_months AS(
  SELECT
    trip_id,
    AVG(total_route_attacks_last_6_months) average_route_attacks_last_6_months
  FROM
    total_unique_attacks_in_route_grids_prior_to_trip_past_6_months_by_previous_trip_id
  GROUP BY
    trip_id ),
  # Now do same for 26 months
  average_unique_attacks_in_route_grids_prior_to_trip_past_12_months AS(
  SELECT
    trip_id,
    AVG(total_route_attacks_last_12_months) average_route_attacks_last_12_months
  FROM
    total_unique_attacks_in_route_grids_prior_to_trip_past_12_months_by_previous_trip_id
  GROUP BY
    trip_id )
  # Now build all indicators
  # Start with voyage_info, so we get full suite of indicators for every trip_id
SELECT
  trip_id,
  # Replace NULLs with 0s, since they're true zeros
  IFNULL(average_route_attacks_last_3_months,0) average_route_attacks_last_3_months_{pixel_size}_degrees,
  IFNULL(average_route_attacks_last_6_months,0) average_route_attacks_last_6_months_{pixel_size}_degrees,
  IFNULL(average_route_attacks_last_12_months,0) average_route_attacks_last_12_months_{pixel_size}_degrees
FROM
  voyage_info
LEFT JOIN
  average_unique_attacks_in_route_grids_prior_to_trip_past_3_months
USING
  (trip_id)
LEFT JOIN
  average_unique_attacks_in_route_grids_prior_to_trip_past_6_months
USING
  (trip_id)
LEFT JOIN
  average_unique_attacks_in_route_grids_prior_to_trip_past_12_months
USING
  (trip_id)