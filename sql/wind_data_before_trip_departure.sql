WITH
trip_info AS(
SELECT
  trip_id,
  departure_date,
  lon_bin,
  lat_bin,
  heading
FROM `emlab-gcp.piracy.{gridded_data_5_table}`
),
wind_data AS(
  SELECT
  date,
  lon_bin,
  lat_bin,
  wind_speed_scalar_ms,
  wind_speed_vector_ms,
  wind_direction_degrees
  FROM `emlab-gcp.piracy.{wind_data_daily_table}`
),
wind_data_on_week_prior_to_departure AS (
-- Get all wind data, by pixel, for the pixels each trip passes through,
-- for the 7 days prior to the departure date
SELECT
*
FROM
trip_info
JOIN
wind_data
USING(lon_bin,lat_bin)
WHERE DATE_DIFF(departure_date, date, DAY) BETWEEN 1 AND 7)
-- Now for each trip, simply take the average wind speed for those 7 days prior to the departure date
SELECT
trip_id,
AVG(wind_speed_scalar_ms) avg_wind_speed_scalar_ms_7_days_prior,
AVG(wind_speed_vector_ms) avg_wind_speed_vector_ms_7_days_prior,
AVG(wind_direction_degrees) avg_wind_direction_degrees_ms_7_days_prior
FROM
wind_data_on_week_prior_to_departure
GROUP BY
trip_id