#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
WITH
  ais_positions AS(
  SELECT
    mmsi,
    trip_id,
    # For voyage-level analysis, we use trip deparature date for determining which attacks occurred before that
    # for grid-level analysis, we use actual activity date for determining which attacks occurred before that
    {ifelse(voyage_level,
      'DATE(departure_timestamp)',
      'DATE(timestamp)')} date,
    DATE(departure_timestamp) departure_date,
    hours,
    distance_km,
    heading,
    main_fuel_consumption_mt_inst,
    aux_fuel_consumption_mt_inst,
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.ungridded_data`),
  # Summarize hours, distance, and message by vessel-by-trip-by-date-by-grid
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
    WHERE date <= '2022-12-31' AND date >= '2013-01-01'
  GROUP BY
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date),
  # Select attack info
  # Only select rows that correspond to anything except suspicious approaches
  attack_info_base AS(
  SELECT
    DATE(date) attack_date,
    COUNT(DISTINCT(asam_reference)) number_attacks,
    FLOOR(lat/pixel_size()) * pixel_size() attack_lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() attack_lon_bin
  FROM
    `emlab-gcp.piracy.asam_data`
  WHERE
    encounter_type != 'Suspicious Approach'
  GROUP BY
    attack_date,
    attack_lat_bin,
    attack_lon_bin),
  # Select attack info
  # Only select rows that correspond to anything except suspicious approaches
  # For all encounter types
  attack_info_base_all_encounters AS(
  SELECT
    DATE(date) attack_date,
    COUNT(DISTINCT(asam_reference)) number_attacks,
    FLOOR(lat/pixel_size()) * pixel_size() attack_lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() attack_lon_bin
  FROM
    `emlab-gcp.piracy.asam_data`
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
    AND attack_date >= '2013-01-01'
    AND attack_date <= '2022-12-31'
  GROUP BY
    lat_bin,
    lon_bin),
  # Each row will be a vessel-by-trip-by-date-by-grid-by-attack
  by_voyage_date_grid_attack AS(
  SELECT
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date,
    number_attacks,
    DATE_DIFF(date, attack_date, DAY) days_since_attack,
    # From GFW: https://github.com/GlobalFishingWatch/bigquery-documentation-wf827/blob/master/queries/fishing_hours_gridded.sql
    # At the equator, a one degree cell is approximately 111 km2 on a side
    # Moving away from the poles, the distance of one degree of longitude changes
    # due to the shape of a globe. Thus, we need to adjust the distance in km of
    # longitude based on latitude using the formula cos(radians(latitude)).
    # We also multiply 111 by the final resolution of the grid cell
    COS(RADIANS(binned.lat_bin)) * (111*pixel_size()) * (111*pixel_size()) grid_area_km2
  FROM
    binned
  LEFT JOIN
    attack_info_base
  ON
    binned.lat_bin = attack_info_base.attack_lat_bin
    AND binned.lon_bin = attack_info_base.attack_lon_bin
    AND binned.date > attack_info_base.attack_date),
  # Each row will be a vessel-by-trip-by-date-by-grid-by-attack
  # For all encounter types
  by_voyage_date_grid_attack_all_encounters AS(
  SELECT
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date,
    number_attacks,
    DATE_DIFF(date, attack_date, DAY) days_since_attack,
    # From GFW: https://github.com/GlobalFishingWatch/bigquery-documentation-wf827/blob/master/queries/fishing_hours_gridded.sql
    # At the equator, a one degree cell is approximately 111 km2 on a side
    # Moving away from the poles, the distance of one degree of longitude changes
    # due to the shape of a globe. Thus, we need to adjust the distance in km of
    # longitude based on latitude using the formula cos(radians(latitude)).
    # We also multiply 111 by the final resolution of the grid cell
    COS(RADIANS(binned.lat_bin)) * (111*pixel_size()) * (111*pixel_size()) grid_area_km2
  FROM
    binned
  LEFT JOIN
    attack_info_base_all_encounters
  ON
    binned.lat_bin = attack_info_base_all_encounters.attack_lat_bin
    AND binned.lon_bin = attack_info_base_all_encounters.attack_lon_bin
    AND binned.date > attack_info_base_all_encounters.attack_date),
  # Now summarize attacks where each row is vessel-by-trip-by-date-by-grid
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
      (days_since_attack <= 30*12,number_attacks,0)) number_previous_attacks_grid_12_months
  FROM
    by_voyage_date_grid_attack
  GROUP BY
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin,
    grid_area_km2),
  # Now summarize attacks where each row is vessel-by-trip-by-date-by-grid
  # For all encounter types
  by_voyage_date_grid_all_encounters AS(
  SELECT
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin,
    SUM(
    IF
      (days_since_attack <= 30*12,number_attacks,0)) number_previous_attacks_grid_12_months_all_encounters
  FROM
    by_voyage_date_grid_attack_all_encounters
  GROUP BY
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin)
SELECT
  * EXCEPT(grid_attacked_in_study_period),
  EXTRACT(YEAR from date) AS year,
  IFNULL(grid_attacked_in_study_period,FALSE) grid_attacked_in_study_period,
IF
  (number_previous_attacks_grid_all_time >0,TRUE,FALSE) grid_has_previous_attacks,
  {hotspots_sql}
FROM
binned
LEFT JOIN
  by_voyage_date_grid
USING
  (mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin)
    LEFT JOIN
  by_voyage_date_grid_all_encounters
USING
  (mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin)
LEFT JOIN
  attack_info_base_in_study_period
USING
  (lon_bin,
    lat_bin)