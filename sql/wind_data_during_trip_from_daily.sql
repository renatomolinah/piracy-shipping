# standardSQL
CREATE TEMPORARY FUNCTION pixel_size()
AS (
  {{pixel_size}}
);

CREATE TEMP FUNCTION RADIANS(x FLOAT64)
AS (
  ACOS(-1) * x / 180
);

-- Pull AIS positions from ungridded_data
WITH
  ais_positions AS (
    SELECT
      trip_id,
      heading,
      -- Assign lat and lon bins based on pixel size
      FLOOR(lat / pixel_size()) * pixel_size() lat_bin,
      FLOOR(lon / pixel_size()) * pixel_size() lon_bin,
      -- We will add wind and wave data at the actual date on which activity occurred, not the departure date
      DATE_TRUNC(DATE(timestamp), MONTH) date
    FROM
      `emlab-gcp.piracy.{ungridded_data_table}`
    -- Only keep data from list of filtered trips
    JOIN (SELECT trip_id FROM `emlab-gcp.piracy.{keep_these_trips_table}`)
      USING (trip_id)
  ),
  daily_wind_data AS (
    SELECT
      date,
      lat_bin,
      lon_bin,
      wind_speed_scalar_ms,
      wind_speed_vector_ms,
      wind_direction_degrees
    FROM
      `emlab-gcp.piracy.{wind_data_daily_table}`
  ),
  -- Now add wind and wave data to AIS messages by appropriate location, month and year
  ais_positions_with_wind AS (
    SELECT
      *,
      -- Calculate wind vector, which combines wind speed and vessel heading
      COS(RADIANS(wind_direction_degrees - heading))
        * wind_speed_vector_ms wind_vector
    FROM
      ais_positions
    LEFT JOIN
      daily_wind_data
      USING (lat_bin, lon_bin, date)
  )
SELECT
  trip_id,
  AVG(wind_speed_scalar_ms) wind_speed_scalar_ms,
  AVG(wind_speed_vector_ms) wind_speed_vector_ms,
  AVG(wind_direction_degrees) wind_direction_degrees,
  AVG(heading) heading,
  AVG(wind_vector) wind_vector
FROM
  ais_positions_with_wind
GROUP BY
  trip_id
