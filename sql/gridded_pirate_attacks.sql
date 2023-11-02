#standardSQL
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
WITH
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
  # Create table with all dates in our study period
  all_dates AS(
  SELECT
    *
  FROM
    UNNEST(GENERATE_DATE_ARRAY('2013-01-01', '2022-12-31', INTERVAL 1 DAY)) AS date ),
  # Find all lat/lon attack grid combos in our study period
  all_attack_grids AS(
  SELECT
    attack_lat_bin lat_bin,
    attack_lon_bin lon_bin
  FROM
    attack_info_base
  GROUP BY
    lat_bin,
    lon_bin),
  # Find all lat/lon/date grid combos
  all_date_attack_grid_combos AS(
  SELECT
    *
  FROM
    all_dates
  CROSS JOIN (
    SELECT
      *
    FROM
      all_attack_grids)),
  # Start with all lat/lon/date grid combos, then join previous attacks for that combo
  joined AS(
  SELECT
    * EXCEPT(number_attacks,
      attack_lat_bin,
      attack_lon_bin),
    DATE_DIFF(date, attack_date, DAY) days_since_attack,
    IFNULL(number_attacks,0) number_attacks
  FROM
    all_date_attack_grid_combos
  LEFT JOIN
    attack_info_base
  ON
    all_date_attack_grid_combos.lat_bin = attack_info_base.attack_lat_bin
    AND all_date_attack_grid_combos.lon_bin = attack_info_base.attack_lon_bin
    AND all_date_attack_grid_combos.date >= attack_info_base.attack_date
  ORDER BY
    date,
    lat_bin,
    lon_bin)
# Now summarize most recent days since attack, and number of total previous attacks over all time, for each lat/lon/date combo
SELECT
  date,
  lat_bin,
  lon_bin,
  MIN(days_since_attack) days_since_attack,
  SUM(number_attacks) number_previous_attacks_all_time,
  SUM(IF(days_since_attack <= 30*12,number_attacks,0)) number_previous_attacks_grid_12_months,
  {hotspots_sql}
FROM
  joined
GROUP BY
  date,
  lat_bin,
  lon_bin
ORDER BY
date,
  lat_bin,
  lon_bin