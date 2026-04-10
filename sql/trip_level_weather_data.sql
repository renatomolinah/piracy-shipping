-- Description:
-- Here we aggregate the data up to 5x5 degree grids
-- we aggregate shipping activity at the vessel level based on voyage departure date, 
-- and determine our pirate attack indicators based on that voyage departure date 
-- This dataset will be the one that gets aggregated to the voyage-level dataset. 
#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
-- Pull AIS positions from ungridded_data
WITH
  ais_positions AS(
  SELECT
    trip_id,
    -- Wind and wave data are at the monthly level, so assign a truncated date for joining
    -- We will use weather data during month activity happened
    DATE_TRUNC(DATE(timestamp),MONTH) truncated_date,
    heading,
    -- Assign lat and lon bins based on pixel size
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.{ungridded_data_table}`
    -- Only keep data from list of filtered trips
  JOIN(SELECT trip_id FROM `emlab-gcp.piracy.{keep_these_trips_table}`) USING(trip_id)),
  -- Summarize hours, distance, and message by vessel-by-trip-by-departure_date-by-grid
  gridded_data AS(
  SELECT
    trip_id,
    lat_bin,
    lon_bin,
    truncated_date,
    AVG(heading) heading
  FROM
    ais_positions
  GROUP BY
    trip_id,
    lat_bin,
    lon_bin,
    truncated_date),
     -- Load 5x5 degree wind data
  wind_info AS(
  SELECT
    DATE_TRUNC(date,MONTH) truncated_date,
    lat_bin,
    lon_bin,
    wind_speed_ms,
    wind_direction_degrees
  FROM
    `emlab-gcp.piracy.{wind_table}`),
      -- Load 5x5 degree wave data
  wave_info AS(
  SELECT
    DATE_TRUNC(date,MONTH) truncated_date,
    lat_bin,
    lon_bin,
    surface_wave_height_m
  FROM
    `emlab-gcp.piracy.{wave_table}`),
gridded_data_with_weather AS(
    -- Now add wind and wave data to gridded data by appropriate location, month and year
    SELECT
    *,
    -- Calculate wind vector, which combines wind speed and vessel heading
    COS(RADIANS(wind_direction_degrees - heading)) * wind_speed_ms wind_vector
    FROM
    gridded_data
    LEFT JOIN
    wind_info
    USING(lat_bin,lon_bin,truncated_date)
    LEFT JOIN
    wave_info
    USING(lat_bin,lon_bin,truncated_date))
    SELECT
    trip_id,
    AVG(wind_vector) wind_vector,
    AVG(wind_speed_ms) wind_speed_ms,
    AVG(surface_wave_height_m) surface_wave_height_m
FROM
gridded_data_with_weather
GROUP BY
trip_id