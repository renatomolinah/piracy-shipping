#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS (5);
WITH
  # For all attacks, get lat/lon into lat_bin/lon_bin using pixel_size
  gridded_attack_info AS(
  SELECT
    date attack_date,
    asam_reference,
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.asam_data`
    # Select all relevant attacks for analysis (i.e., all except suspicious approaches)
  WHERE
    encounter_type != 'Suspicious Approach'),
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
    `emlab-gcp.piracy.voyage_info_v_20240228`
    # For testing, let's filter to only voyages leaving in 2021
  WHERE
    EXTRACT(YEAR
    FROM
      departure_timestamp) = 2021),
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
    `emlab-gcp.piracy.gridded_data_5_v_20240307`
  LEFT JOIN
    voyage_info
  USING
    (trip_id)
     # For testing, let's filter to only voyages leaving in 2021
  WHERE
    year = 2021
  GROUP BY
    departure_date,
    from_port,
    from_country,
    to_port,
    to_country,
    lat_bin,
    lon_bin ),
  # For each trip_id, find all previous lat_bin/lon_bin grids that trips along that route passed through
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
    # Require that grids were passed through before the trip's departure date, using '<'
    # If instead we wanted to also include grids passed through the trip, we'd use '<='
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
  # For each trip_id, find all grids that trips along that route passed through in the past 12 months
  # Then count up the number of unique attacks that occurred within those grids in the 12 months prior to the trip's departure date
  total_unique_attacks_in_route_grids_prior_to_trip_past_12_months AS (
  SELECT
    trip_id,
    COUNT(DISTINCT(asam_reference)) total_route_attacks_last_12_months
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
SELECT
  *
FROM
  total_unique_attacks_in_route_grids_prior_to_trip_past_12_months