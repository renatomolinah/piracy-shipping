# Description:
# Here we aggregate the data up to either 5x5 degree grids, or 0.5x0.5 degree grids for JC's analysis. 
#For the 0.5x0.5 degree aggregation, we aggregate shipping activity at the daily vessel level, 
# and determine our pirate attack indicators based on that date. 
# For the 5x5 degree grids, we aggregate shipping activity at the vessel level based on voyage departure date, 
#and determine our pirate attack indicators based on that voyage departure date 
# (since this dataset will be the one that gets aggregated to the voyage-level in the final below). 
#For the 5x5 degree analysis, we also add our wind speed and heading data for each grid 
# matched to the day each voyage passes through each grid.
#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
# Pull AIS positions from ungridded_data
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
    # Assign lat and lon bins based on pixel size
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin,
    # We will add wind data at the actual date on which activity occurred, regardless of the aggregation
      EXTRACT(MONTH
    FROM
      timestamp) wind_month,
      EXTRACT(YEAR
    FROM
      timestamp) wind_year
  FROM
    `emlab-gcp.piracy.ungridded_data_v_20240228`),
  wind_info AS(
  SELECT
    EXTRACT(YEAR
    FROM
      date) wind_year,
    EXTRACT(MONTH
    FROM
      date) wind_month,
    lat_bin,
    lon_bin,
    wind_speed_ms,
    wind_direction_degrees
  FROM
    `emlab-gcp.piracy.{wind_table_location}`),
  # Summarize hours, distance, and message by vessel-by-trip-by-date-by-grid
  binned AS(
  SELECT
    mmsi,
    trip_id,
    lat_bin,
    lon_bin,
    date,
    # Need to include wind_month and wind_year for matching to wind data
    # when making grid-level dataset that will feed into the voyage-level dataset
    {ifelse(voyage_level,
      'wind_month,wind_year,',
      '')}
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
        # Need to include wind_month and wind_year for matching to wind data
    # when making grid-level dataset that will feed into the voyage-level dataset
        {ifelse(voyage_level,
      'wind_month,wind_year,',
      '')}
    date),
  # Select attack info
  # Select all rows except those encounters that are labeled as suspicious approaches
  # Then aggregate by summing the number of encounters per lat_bin/lon_bin/attack_date
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
  # Select attack info for all encounter types
  # Then aggregate by summing the number of encounters per lat_bin/lon_bin/attack_date
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
      (days_since_attack <= 365,number_attacks,0)) number_previous_attacks_grid_12_months_all_encounters
  FROM
    by_voyage_date_grid_attack_all_encounters
  GROUP BY
    mmsi,
    trip_id,
    date,
    lat_bin,
    lon_bin),
# Get vessel info, which is pre-filtered list of cargo vessels
# This provides binaries so that the dataset can be filtered by on the vessel class selection criteria
vessel_info AS(
  SELECT
    mmsi,
    best_vessel_type_cargo,
    registry_vessel_type_any_cargo,
    registry_vessel_type_always_cargo
  FROM
    `emlab-gcp.piracy.vessel_info` )
SELECT
  {ifelse(voyage_level,
  '* EXCEPT(grid_attacked_in_study_period,wind_month,wind_year)',
  '* EXCEPT(grid_attacked_in_study_period)')},
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
LEFT JOIN
vessel_info
USING(mmsi)
# For voyage-level analysis based on 5x5 degree data, add wind data
    {ifelse(voyage_level,
      'LEFT JOIN wind_info USING (wind_month,wind_year,lat_bin,lon_bin)','')}