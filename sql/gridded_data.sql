#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
WITH
  voyages_ais_positions AS(
  SELECT
    * EXCEPT(lat,
      lon,
      avg_distance_km),
    avg_distance_km distance_km,
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin,
    EXTRACT(MONTH
    FROM
      departure_timestamp) month
  FROM
    `emlab-gcp.piracy.ungridded_data` ),
  # Summarize by all columns except a few https://stackoverflow.com/questions/54792360/bigquery-group-by-all-columns-except-a-few
  binned AS(
  SELECT
    DISTINCT * EXCEPT(hours,
      distance_km,
      heading,
      main_fuel_consumption_mt_inst,
      aux_fuel_consumption_mt_inst),
    SUM(hours) OVER(PARTITION BY mmsi, date, trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) hours,
    SUM(distance_km) OVER(PARTITION BY mmsi, date, trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) distance_km,
    COUNT(*) OVER(PARTITION BY mmsi, date, trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) ais_messages,
    AVG(heading) OVER(PARTITION BY mmsi, date, trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) heading,
    SUM(main_fuel_consumption_mt_inst) OVER(PARTITION BY mmsi, date, trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) OVER(PARTITION BY mmsi, date, trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) aux_fuel_consumption_mt_inst
  FROM
    voyages_ais_positions),
  # Select attack info - this was generated in full_analysis.R in the GitHub repo
  attack_info AS(
  SELECT
    DATE(date) date,
    lon_bin,
    lat_bin,
    days_since_attack,
    attacks_window_last_1_month,
    attacks_window_last_2_month,
    attacks_window_last_3_month,
    attacks_window_last_4_month,
    attacks_window_last_5_month,
    attacks_window_last_6_month
  FROM
    `emlab-gcp.piracy.{attack_table_location}`)
SELECT
  *
FROM
  binned
LEFT JOIN
  attack_info
USING
  (date,
    lat_bin,
    lon_bin)