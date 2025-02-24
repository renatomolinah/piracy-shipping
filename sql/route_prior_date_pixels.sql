WITH
# Create table with all dates in our study period
  all_dates AS(
  SELECT
    *
  FROM
    UNNEST(GENERATE_DATE_ARRAY('2013-01-01', '2022-12-31', INTERVAL 1 DAY)) AS date ),
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
    to_country )
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
  all_dates.date >= route_pixel_earliest_date.earliest_date