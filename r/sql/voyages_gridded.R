# Take AIS position data for each voyage, and group by grid
# Add attack data for each grid
# Add wind data for each grid
# Save as voyages_gridded

sql <-glue::glue("
  #standardSQL
  WITH
ais_info AS(
  SELECT
  mmsi,
  vessel_type,
  flag,
  length,
  engine_power,
  crew,
  tonnage,
  aux_engine_power,
  design_speed,
  main_sfc,
  aux_sfc,
  FLOOR(start_lat/5) * 5 lat_bin,
  FLOOR(start_lon/5) * 5 lon_bin,
  from_anchorage_id,
  from_anchorage_name,
  from_port_name,
  departure_timestamp,
  DATE(departure_timestamp) departure_date,
  to_anchorage_id,
  to_anchorage_name,
  to_port_name,
  arrival_timestamp,
  SUM(hours) hours,
  SUM(avg_distance_km) distance_km,
  SUM(total_fuel_consumption_mt) total_fuel_consumption_mt,
  {clusters_aggregated}
  FROM
  `piracy.voyage_ais_positions`
  WHERE
  NOT total_fuel_consumption_mt IS NULL
  GROUP BY
  mmsi,
  vessel_type,
  flag,
  length,
  engine_power,
  crew,
  tonnage,
  aux_engine_power,
  design_speed,
  main_sfc,
  aux_sfc,
  lat_bin,
  lon_bin,
  from_anchorage_id,
  from_anchorage_name,
  from_port_name,
  departure_timestamp,
  departure_date,
  to_anchorage_id,
  to_anchorage_name,
  to_port_name,
  arrival_timestamp),
wind AS(
  SELECT
  *
    FROM
  `piracy.wind`),
piracy_attacks AS(
  SELECT
  *
    FROM
  `piracy.piracy_attacks`),
master AS(
  SELECT
  ais_info.departure_date departure_date,
  ais_info.lat_bin lat_bin,
  ais_info.lon_bin lon_bin,
  mmsi,
  vessel_type,
  flag,
  length,
  engine_power,
  crew,
  tonnage,
  aux_engine_power,
  design_speed,
  main_sfc,
  aux_sfc,
  from_anchorage_id,
  from_anchorage_name,
  from_port_name,
  departure_timestamp,
  to_anchorage_id,
  to_anchorage_name,
  to_port_name,
  arrival_timestamp,
  distance_km,
  hours,
{clusters_aggregated_2},
days_since_attack,
grid_has_previous_attacks,
attacks_last_7_days, 
attacks_last_14_days,
attacks_last_21_days,
attacks_last_30_days,
attacks_last_60_days,
attacks_last_90_days,
attacks_last_120_days,
attacks_last_150_days,
attacks_last_180_days,
attacks_last_210_days,
attacks_last_240_days,
attacks_last_270_days,
attacks_last_300_days,
attacks_last_330_days,
attacks_last_365_days,
  total_fuel_consumption_mt,
  speed_m_s,
direction_degrees
  FROM
  ais_info
  LEFT JOIN
  wind
  ON
  ais_info.departure_date = wind.date
  AND ais_info.lat_bin = wind.lat_bin
  AND ais_info.lon_bin = wind.lon_bin
  LEFT JOIN
  piracy_attacks
  ON
  ais_info.departure_date = piracy_attacks.date
  AND ais_info.lat_bin = piracy_attacks.lat_bin
  AND ais_info.lon_bin = piracy_attacks.lon_bin)
SELECT
*
  FROM
master
")
bq_table(project = project,table = "voyages_gridded",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "voyages_gridded",dataset = "piracy"),
                 allowLargeResults = TRUE)
