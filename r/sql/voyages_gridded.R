# Take AIS position data for each voyage, and group by grid
# Add attack data for each grid
# Add wind data for each grid
# Save as voyages_gridded

sql <-glue::glue("
  #standardSQL
  WITH
  vessel_info AS(
  SELECT
  *
  FROM 
  `piracy.vessel_info`
  ),
ais_info AS(
  SELECT
  mmsi,
  FLOOR(lat/5) * 5 lat_bin,
  FLOOR(lon/5) * 5 lon_bin,
  trip_id,
  DATE(timestamp) departure_date,
  SUM(hours) hours,
  SUM(avg_distance_km) distance_km,
  AVG(heading) heading,
  SUM(main_fuel_consumption_mt_inst) main_fuel_consumption_mt_inst,
  SUM(aux_fuel_consumption_mt_inst) aux_fuel_consumption_mt_inst,
  {clusters_aggregated}
  FROM
  `piracy.voyage_ais_positions`
  GROUP BY
  mmsi,
  lat_bin,
  lon_bin
  trip_id),
  voyage_info AS(
  SELECT
  *
  FROM
  `piracy.voyages_with_anchorages`
  ),
wind AS(
  SELECT
  *
    FROM
  `piracy.wind`),
piracy_attacks AS(
  SELECT
  *
    FROM
  `piracy.piracy_attacks`)
  SELECT
  ais_info.departure_date departure_date,
  ais_info.lat_bin lat_bin,
  ais_info.lon_bin lon_bin,
  ais_info.mmsi,
  voyage_info.year,
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
  from_port from_port_name,
  departure_timestamp,
  to_anchorage_id,
  to_port to_port_name,
  arrival_timestamp,
  distance_km,
  hours,
  main_fuel_consumption_mt_inst,
  aux_fuel_consumption_mt_inst,
  (main_fuel_consumption_mt_inst + aux_fuel_consumption_mt_inst) total_fuel_consumption_mt_inst,
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
# Wind vector relative to direction of travel
# Positive is tailwind, negative is headwind
COS(RADIANS(direction_degrees - heading)) * speed_m_s wind_vector
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
  AND ais_info.lon_bin = piracy_attacks.lon_bin
  LEFT JOIN
  vessel_info
  USING(mmsi,year)
  LEFT JOIN
  voyage_info
  USING(mmsi,trip_id))
")
bq_table(project = project,table = "voyages_gridded",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "voyages_gridded",dataset = "piracy"),
                 allowLargeResults = TRUE)
