WITH
  # Find all pixels where attacks every happened
  attack_pixels AS(
  SELECT
    lat_bin,
    lon_bin
  FROM
    `emlab-gcp.piracy.gridded_pirate_attacks_5_v_20250210`
  WHERE
    days_since_attack = 0
  GROUP BY
    lat_bin,
    lon_bin ),
  # Find the earliest date for all route-pixels
  route_pixel_earliest_date AS(
  SELECT
    MIN(date) earliest_date,
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    `emlab-gcp.piracy.route_pixel_dates_v_20250210`
  GROUP BY
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country ),
  # Create table with all dates in our study period
  all_dates AS(
  SELECT
    *
  FROM
    UNNEST(GENERATE_DATE_ARRAY('2013-01-01', '2022-12-31', INTERVAL 1 DAY)) AS date ),
  # For each route and date, find all the pixels that were passed through before that date (and which ever had an attack)
  route_prior_date_pixels AS(
  SELECT
    all_dates.date,
    lon_bin,
    lat_bin,
    from_port,
    from_country,
    to_port,
    to_country
  FROM
    all_dates
    # Get all combinations of dates and route pixels
    # Only use pixels that were passed through for each route on or before each date
  LEFT JOIN
    route_pixel_earliest_date
  ON
    all_dates.date >= route_pixel_earliest_date.earliest_date),
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
  route_prior_date_pixels
  # Get all combinations of dates and route pixels
  # Only use pixels that were passed through for each route on or before each date
LEFT JOIN
  attack_pixel_dates
USING
  (date,
    lon_bin,
    lat_bin)
GROUP BY
  date,
  from_port,
  from_country,
  to_port,
  to_country