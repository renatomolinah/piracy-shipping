WITH
  # Create table with all dates in our study period
  all_dates AS(
  SELECT
    *
  FROM
    UNNEST(GENERATE_DATE_ARRAY('{study_period_starting_date}', '{study_period_ending_date}', INTERVAL 1 DAY)) AS date ),
  # For each route, find the pixels they passed through at any point in time
  route_pixels_all_time AS(
  SELECT
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    `emlab-gcp.piracy.{route_pixels_all_time_table}`
  WHERE
    fraction_route_trips_through_pixel >= {fraction_route_trips_through_pixel_min_threshold})
  ),
  # Find the date and pixels that have an attack
  attack_pixel_dates AS(
  SELECT
    date,
    lat_bin,
    lon_bin,
    1 pixel_date_has_attack
  FROM
    `emlab-gcp.piracy.{gridded_attack_table}`
  WHERE
    days_since_attack = 0
        GROUP BY
  date,
  lat_bin,
  lon_bin)
SELECT
  date,
  from_port,
  from_country,
  to_port,
  to_country,
# If any pixel along the route on the date has an attack, give a value of TRUE
IF
  (SUM(pixel_date_has_attack) > 0, TRUE, FALSE) route_has_attack
FROM
  all_dates
# Get all combinations of dates and route pixels
CROSS JOIN
  route_pixels_all_time
# Add attack info by date and pixel
LEFT JOIN
  attack_pixel_dates
USING
  (date,
    lat_bin,
    lon_bin)
GROUP BY
  date,
  from_port,
  from_country,
  to_port,
  to_country