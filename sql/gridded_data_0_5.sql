-- Description:
-- Here we aggregate the data up to 0.5x0.5 degree grids for JC's grid-level analysis. 
-- For the 0.5x0.5 degree aggregation, we aggregate shipping activity at the daily vessel level, 
-- and determine our pirate attack indicators based on that date. 
#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
-- Pull AIS positions from ungridded_data
WITH
  ais_positions AS(
  SELECT
    mmsi,
    trip_id,
    -- For 0.5x0.5 voyage-level analysis, we use activity date for determining which attacks occurred before that
    DATE(timestamp) date,
    hours,
    distance_km,
    heading,
    main_fuel_consumption_mt_inst,
    aux_fuel_consumption_mt_inst,
    -- Assign lat and lon bins based on pixel size
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.{ungridded_data_table}`
  -- Only keep data from list of filtered trips
  JOIN(SELECT trip_id FROM `emlab-gcp.piracy.{keep_these_trips_table}`) USING(trip_id)),
  -- Summarize hours, distance, and message by vessel-by-trip-by-date-by-grid
  binned AS(
  SELECT
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date,
    SUM(hours) hours,
    SUM(distance_km) distance_km,
    COUNT(*) ais_messages,
    AVG(heading) heading,
    SUM(main_fuel_consumption_mt_inst) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) aux_fuel_consumption_mt_inst
  FROM
    ais_positions
  GROUP BY
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date),
  -- Select attack info
  -- Select all rows except those encounters that are labeled as suspicious approaches
  -- Then aggregate by summing the number of encounters per lat_bin/lon_bin/attack_date
  attack_info_base AS(
  SELECT
    DATE(date) attack_date,
    COUNT(DISTINCT(reference)) number_attacks,
    FLOOR(lat/pixel_size()) * pixel_size() attack_lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() attack_lon_bin
  FROM
    `emlab-gcp.piracy.{asam_data_table}`
  GROUP BY
    attack_date,
    attack_lat_bin,
    attack_lon_bin),
  -- Select attack info for all encounter types
  -- Then aggregate by summing the number of encounters per lat_bin/lon_bin/attack_date
  attack_info_base_all_encounters AS(
  SELECT
    DATE(date) attack_date,
    COUNT(DISTINCT(reference)) number_attacks,
    FLOOR(lat/pixel_size()) * pixel_size() attack_lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() attack_lon_bin
  FROM
    `emlab-gcp.piracy.{asam_data_table}`
  GROUP BY
    attack_date,
    attack_lat_bin,
    attack_lon_bin),
  attack_info_base_in_study_period AS(
  SELECT
    attack_lat_bin lat_bin,
    attack_lon_bin lon_bin,
    TRUE grid_attacked_in_study_period
  FROM
    attack_info_base
  WHERE
    number_attacks >0
    AND attack_date >= '{study_period_starting_date}'
    AND attack_date <= '{study_period_ending_date}'
  GROUP BY
    lat_bin,
    lon_bin),
  -- Each row will be a vessel-by-trip-by-date-by-grid-by-attack
  by_voyage_date_grid_attack AS(
  SELECT
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date,
    number_attacks,
    DATE_DIFF(date, attack_date, DAY) days_since_attack,
    -- From GFW: https://github.com/GlobalFishingWatch/bigquery-documentation-wf827/blob/master/queries/fishing_hours_gridded.sql
    -- At the equator, a one degree cell is approximately 111 km2 on a side
    -- Moving away from the poles, the distance of one degree of longitude changes
    -- due to the shape of a globe. Thus, we need to adjust the distance in km of
    -- longitude based on latitude using the formula cos(radians(latitude)).
    -- We also multiply 111 by the final resolution of the grid cell
    COS(RADIANS(binned.lat_bin)) * (111*pixel_size()) * (111*pixel_size()) grid_area_km2
  FROM
    binned
  LEFT JOIN
    attack_info_base
  ON
    binned.lat_bin = attack_info_base.attack_lat_bin
    AND binned.lon_bin = attack_info_base.attack_lon_bin
    AND binned.date > attack_info_base.attack_date),
  -- Do the same thing, but look for future attacks that haven't happened yet, for a placebo test
  by_voyage_date_grid_attack_future AS(
  SELECT
    mmsi,
    trip_id,
    date,
    lon_bin,
    lat_bin,
    DATE_DIFF(attack_date, date, DAY) days_until_attack,
    number_attacks
  FROM
    binned
  LEFT JOIN
    attack_info_base
  ON
    binned.lat_bin = attack_info_base.attack_lat_bin
    AND binned.lon_bin = attack_info_base.attack_lon_bin
    AND binned.date < attack_info_base.attack_date),
  -- Each row will be a vessel-by-trip-by-date-by-grid-by-attack
  -- For all encounter types
  by_voyage_date_grid_attack_all_encounters AS(
  SELECT
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date,
    number_attacks,
    DATE_DIFF(date, attack_date, DAY) days_since_attack
  FROM
    binned
  LEFT JOIN
    attack_info_base_all_encounters
  ON
    binned.lat_bin = attack_info_base_all_encounters.attack_lat_bin
    AND binned.lon_bin = attack_info_base_all_encounters.attack_lon_bin
    AND binned.date > attack_info_base_all_encounters.attack_date),
  -- Now summarize attacks where each row is vessel-by-trip-by-date-by-grid
  by_voyage_date_grid AS(
  SELECT
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin,
    grid_area_km2,
    SUM(IFNULL(number_attacks,0)) number_previous_attacks_grid_all_time,
    MIN(days_since_attack) days_since_attack,
    SUM(
    IF
      (days_since_attack <= 3 * 30,number_attacks,0)) number_previous_attacks_grid_3_months,
    SUM(
    IF
      (days_since_attack <= 6 * 30,number_attacks,0)) number_previous_attacks_grid_6_months,
    SUM(
    IF
      (days_since_attack <= 9 * 30,number_attacks,0)) number_previous_attacks_grid_9_months,
    SUM(
    IF
      (days_since_attack <= 365,number_attacks,0)) number_previous_attacks_grid_12_months,
    SUM(
    IF
      (days_since_attack <= 2 * 365,number_attacks,0)) number_previous_attacks_grid_24_months
  FROM
    by_voyage_date_grid_attack
  GROUP BY
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin,
    grid_area_km2),
  -- Now do the same, but for future attacks
  by_voyage_date_grid_future AS(
  SELECT
    mmsi,
    date,
    lon_bin,
    lat_bin,
    trip_id,
    SUM(
    IF
      (days_until_attack <= 7,number_attacks,0)) number_future_attacks_grid_7_days,
    SUM(
    IF
      (days_until_attack <= 15,number_attacks,0)) number_future_attacks_grid_15_days,
    SUM(
    IF
      (days_until_attack <= 1 * 30,number_attacks,0)) number_future_attacks_grid_1_month,
    SUM(
    IF
      (days_until_attack <= 3 * 30,number_attacks,0)) number_future_attacks_grid_3_months,
    SUM(
    IF
      (days_until_attack <= 6 * 30,number_attacks,0)) number_future_attacks_grid_6_months,
    SUM(
    IF
      (days_until_attack <= 9 * 30,number_attacks,0)) number_future_attacks_grid_9_months,
    SUM(
    IF
      (days_until_attack <= 365,number_attacks,0)) number_future_attacks_grid_12_months,
    SUM(
    IF
      (days_until_attack <= 2 * 365,number_attacks,0)) number_future_attacks_grid_24_months
  FROM
    by_voyage_date_grid_attack_future
  GROUP BY
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin),
  -- Now summarize attacks where each row is vessel-by-trip-by-date-by-grid
  -- For all encounter types
  by_voyage_date_grid_all_encounters AS(
  SELECT
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin,
    SUM(
    IF
      (days_since_attack <= 365,number_attacks,0)) number_previous_attacks_grid_12_months_all_encounters
  FROM
    by_voyage_date_grid_attack_all_encounters
  GROUP BY
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin),
-- Get vessel info, which is pre-filtered list of cargo vessels
-- This provides binaries so that the dataset can be filtered by on the vessel class selection criteria
vessel_info AS(
  SELECT
    mmsi,
    best_vessel_type_cargo,
    registry_vessel_type_any_cargo,
    registry_vessel_type_always_cargo
  FROM
    `emlab-gcp.piracy.{vessel_info_table}` )
SELECT
    * EXCEPT(grid_attacked_in_study_period),
    EXTRACT(YEAR from date) AS year,
  IFNULL(grid_attacked_in_study_period,FALSE) grid_attacked_in_study_period,
IF
  (number_previous_attacks_grid_all_time >0,TRUE,FALSE) grid_has_previous_attacks,
  {hotspots_sql}
FROM
binned
-- Add attack info for all encounters except suspicious encounters
LEFT JOIN
  by_voyage_date_grid
USING
  (mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin)
  -- Add attack info for future attacks
  LEFT JOIN
  by_voyage_date_grid_future
USING
  (mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin)
    LEFT JOIN
-- Add attack info for all encounters
  by_voyage_date_grid_all_encounters
USING
  (mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin)
-- Add info for whether there were attacks in the study period
LEFT JOIN
  attack_info_base_in_study_period
USING
  (lon_bin,
    lat_bin)
-- Add vessel info
LEFT JOIN
vessel_info
USING(mmsi)