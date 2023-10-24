WITH
  # for each trip occurring on each departure date and route, calculate the total number of attacks that occurred along the grids within the last 12 months
  total_attacks_by_trips_on_date AS(
  SELECT
    date,
    trip_id,
    from_port,
    from_country,
    to_port,
    to_country,
    SUM(number_previous_attacks_grid_12_months) number_previous_attacks_grid_12_months
  FROM
    `emlab-gcp.piracy.gridded_data_5`
  GROUP BY
    date,
    trip_id,
    from_port,
    from_country,
    to_port,
    to_country),
  # For each deparature date and route, calculate the average number of attacks (across all trips) that occurred along the grids within the last 12 months
  average_attacks_by_route_on_date AS(
  SELECT
    date,
    from_port,
    from_country,
    to_port,
    to_country,
    AVG(number_previous_attacks_grid_12_months) average_number_previous_attacks_grid_12_months
  FROM
    total_attacks_by_trips_on_date
  GROUP BY
    date,
    from_port,
    from_country,
    to_port,
    to_country),
  # Get trip_ids and route info
  trips AS(
  SELECT
    trip_id,
    date,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    `emlab-gcp.piracy.gridded_data_5`),
  # For each trip, get the average number of attacks along the route that occurred on each departure date prior to this trip's departure date
  joined AS(
  SELECT
    trip_id,
    average_attacks_by_route_on_date.average_number_previous_attacks_grid_12_months,
    DATE_DIFF(trips.date, average_attacks_by_route_on_date.date, DAY) days_since_average_attack_calculation
  FROM
    trips
  JOIN
    average_attacks_by_route_on_date
  ON
    average_attacks_by_route_on_date.date <= trips.date
    AND trips.from_port = average_attacks_by_route_on_date.from_port
    AND trips.to_port = average_attacks_by_route_on_date.to_port
    AND trips.from_country = average_attacks_by_route_on_date.from_country
    AND trips.to_country = average_attacks_by_route_on_date.to_country )
  # Finally, for each trip, summarize the average number of attacks that occurred along the route
  # for trips departing within the last 3, 6, 9, 12, and 24 months
SELECT
  trip_id,
  AVG(
  IF
    (days_since_average_attack_calculation <= 30*3,average_number_previous_attacks_grid_12_months,0)) average_route_attacks_last_3_months,
  AVG(
  IF
    (days_since_average_attack_calculation <= 30*6,average_number_previous_attacks_grid_12_months,0)) average_route_attacks_last_6_months,
  AVG(
  IF
    (days_since_average_attack_calculation <= 30*9,average_number_previous_attacks_grid_12_months,0)) average_route_attacks_last_9_months,
  AVG(
  IF
    (days_since_average_attack_calculation <= 30*12,average_number_previous_attacks_grid_12_months,0)) average_route_attacks_last_12_months,
  AVG(
  IF
    (days_since_average_attack_calculation <= 30*24,average_number_previous_attacks_grid_12_months,0)) average_route_attacks_last_24_months
FROM
  joined
GROUP BY
  trip_id