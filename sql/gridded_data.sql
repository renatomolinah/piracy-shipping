#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
WITH
vessel_info AS(
  SELECT
    *
  FROM
    `emlab-gcp.piracy.vessel_info` ),
  voyage_info AS(
  SELECT
    * EXCEPT(voyage_mmsi,
    departure_timestamp,
    arrival_timestamp,
    to_anchorage_id,
    from_anchorage_id),
    DATE(departure_timestamp) date,
    EXTRACT(MONTH
    FROM
      departure_timestamp) month,
    EXTRACT(YEAR
    FROM
      departure_timestamp) year
  FROM
    `emlab-gcp.piracy.voyage_info` ),
  voyages_ais_positions AS(
  SELECT
    * EXCEPT(lat,
      lon),
    FLOOR(lat/pixel_size()) * pixel_size() lat_bin,
    FLOOR(lon/pixel_size()) * pixel_size() lon_bin
  FROM
    `emlab-gcp.piracy.ungridded_data`
  JOIN
    voyage_info
  USING
    (trip_id)),
  # Summarize by all columns except a few https://stackoverflow.com/questions/54792360/bigquery-group-by-all-columns-except-a-few
  binned AS(
  SELECT
    DISTINCT * EXCEPT(hours,
      distance_km,
      heading,
      main_fuel_consumption_mt_inst,
      aux_fuel_consumption_mt_inst),
    SUM(hours) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) hours,
    SUM(distance_km) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) distance_km,
    COUNT(*) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) ais_messages,
    AVG(heading) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) heading,
    SUM(main_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) aux_fuel_consumption_mt_inst
  FROM
    voyages_ais_positions),
  # Select attack info
  attack_info_base AS(
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
  # Each row will be a voyage and grid and days-since-attack
  by_voyage_grid_attack AS(
  SELECT
    * EXCEPT(attack_lat_bin,
      attack_lon_bin),
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
    AND binned.date >= attack_info_base.attack_date),
  by_voyage_grid AS(
    # Now summarize attack info by voyage and grid
  SELECT
    DISTINCT * EXCEPT(days_since_attack,
      number_attacks,
      attack_date),
    SUM(IFNULL(number_attacks,0)) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) number_previous_attacks_grid_all_time,
    MIN(days_since_attack) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) days_since_attack,
    SUM(
    IF
      (days_since_attack <= 30*12,number_attacks,0)) OVER(PARTITION BY trip_id, CAST(lat_bin AS INT64),
      CAST(lon_bin AS INT64)) number_previous_attacks_grid_12_months
  FROM
    by_voyage_grid_attack)
SELECT
  *,
IF
  (number_previous_attacks_grid_all_time >0,TRUE,FALSE) grid_has_previous_attacks,
  {hotspots_sql}
FROM
  by_voyage_grid
JOIN
vessel_info
USING(mmsi,year)