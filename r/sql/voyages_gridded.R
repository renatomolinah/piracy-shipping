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
  AVG(heading) heading,
  {clusters_aggregated}
  FROM
  `piracy.voyage_ais_positions`
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
attacks_last_1_year, 
attacks_last_2_years, 
attacks_last_3_years, 
attacks_last_4_years, 
attacks_last_5_years, 
attacks_last_6_years, 
attacks_last_7_years, 
  speed_m_s,
direction_degrees,
heading
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
