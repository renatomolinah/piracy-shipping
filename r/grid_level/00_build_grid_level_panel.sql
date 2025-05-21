################################################################################
#
# This SQL query combines the gridded data from the gridded attack cluster and
# trips from vessels to create a grid-level panel of attaks and ship transit.
# The data are actually queried and downloaded with 01_build_grid_level_panel.R,
# but this provides a way to check directly on BigQuery. The script does the
# following:
# 
# 1) Build a table of the number of attacks (by monthly leads and lags) for each
# grid cell, and identify whether it falls in a hotspot cluster or the rest of 
# the world.
#
# 2) Build a table of outcome variables of interest by gridcell
# 
# 3) Combines 1 and 2
#
################################################################################
WITH
  with_clusters AS (
  SELECT
    date,
    grid_id,
    lat_bin,
    lon_bin,
    attacks_window_neixt_1_month,
    attacks_window_next_2_month,
    attacks_window_next_3_month,
    attacks_window_next_4_month,
    attacks_window_next_5_month,
    attacks_window_next_6_month,
    attacks_window_next_7_month,
    attacks_window_next_8_month,
    attacks_window_next_9_month,
    attacks_window_next_10_month,
    attacks_window_next_11_month,
    attacks_window_next_12_month,
    attacks_window_last_1_month,
    attacks_window_last_2_month,
    attacks_window_last_3_month,
    attacks_window_last_4_month,
    attacks_window_last_5_month,
    attacks_window_last_6_month,
    attacks_window_last_7_month,
    attacks_window_last_8_month,
    attacks_window_last_9_month,
    attacks_window_last_10_month,
    attacks_window_last_11_month,
    attacks_window_last_12_month,
    days_since_attack,
    grid_has_previous_attacks,
    CASE
      WHEN (lat_bin BETWEEN -5.75 AND 11.45) AND (lon_bin BETWEEN -9 AND 14.7) THEN "GoG"
      WHEN (lat_bin BETWEEN -19.8 AND 32.1) AND (lon_bin BETWEEN 83 AND 129.3) THEN "SEA"
      WHEN (lat_bin BETWEEN -10.35 AND 31.6) AND (lon_bin BETWEEN 33.2 AND 72.6) THEN "GoA"
    ELSE "None"
  END
    AS attack_cluster
  FROM
    `emlab-gcp.piracy.piracy_attacks_0_5` ),
  #
  #
  #
  #
  track_info AS (
  SELECT
    date,
    lat_bin,
    lon_bin,
    COUNT(DISTINCT(mmsi)) AS n_vessels,
    COUNT(DISTINCT(trip_id)) AS n_trips,
    SUM(hours) AS hours,
    SUM(distance_km) AS distance_km,
    SUM(ais_messages) AS n_ais_messages
  FROM
    `emlab-gcp.piracy.gridded_data_ml`
  GROUP BY
    date,
    lat_bin,
    lon_bin)
  #
  #
  #
  #
  ########
SELECT
  *
FROM
  with_clusters
LEFT JOIN
  track_info
USING
  (date,
    lat_bin,
    lon_bin)