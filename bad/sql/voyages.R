# Summarize gridded data into a single row for each voyage
# Emissions info from here: https://www.sciencedirect.com/science/article/pii/S1361920909001072
# Price info scraped from bunkerindex.com
# Calculate fuel consumption at trip level using average speed

sql <-glue::glue(
  "
#standardSQL
  WITH vessel_info AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.vessel_info` ),
  voyage_info AS(
  SELECT
    *,
    DATE(departure_timestamp) departure_date
  FROM
    `ucsb-gfw.piracy.voyages_with_anchorages`),
  gridded_info AS(
  SELECT
    mmsi,
    trip_id,
    SUM(distance_km) total_distance_km,
{clusters_aggregated},
    SUM(hours) total_hours,
    (SUM(distance_km) * 0.539957 / SUM(hours)) implied_speed_knots,
    SUM(CASE
        WHEN NOT(grid_has_previous_attacks IS NULL) THEN distance_km
        ELSE 0 END) attack_grid_distance_km,
    SUM(CASE
        WHEN NOT(grid_has_previous_attacks IS NULL) THEN hours
        ELSE 0 END) attack_grid_hours,
    (CASE
        WHEN SUM(grid_has_previous_attacks) IS NULL THEN 0
        ELSE SUM(grid_has_previous_attacks) END) number_attack_grids,
    SUM(attacks_last_1_year) attacks_last_1_year,
    SUM(attacks_last_2_years) attacks_last_2_years,
    SUM(attacks_last_3_years) attacks_last_3_years,
    MIN(days_since_attack) days_since_attack,
    (CASE
        WHEN SUM(hours) = 0 THEN AVG(speed_m_s)
        ELSE (SUM(speed_m_s * hours) / SUM(hours)) END) speed_m_s,
    (CASE
        WHEN SUM(hours) = 0 THEN AVG(direction_degrees)
        ELSE (SUM(direction_degrees * hours) / SUM(hours)) END) direction_degrees,
    (CASE
        WHEN SUM(hours) = 0 THEN AVG(wind_vector)
        ELSE (SUM(wind_vector * hours) / SUM(hours)) END) wind_vector,
    SUM(main_fuel_consumption_mt_inst) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) aux_fuel_consumption_mt_inst,
    SUM(total_fuel_consumption_mt_inst) total_fuel_consumption_mt_inst
  FROM
    `piracy.voyages_gridded_bad`
  GROUP BY
    mmsi,
    trip_id),
  fuel_prices AS(
  SELECT
    *
  FROM
    `piracy.daily_fuel_prices`
  WHERE
    fuel_index = 'BIX 380 CST' ),
  to_anchorage_info AS(
  SELECT
    s2id,
    ST_GEOGPOINT(lon,
      lat) to_anchorage_position
  FROM
    `world-fishing-827.gfw_research.named_anchorages_v20180803_13b`),
  from_anchorage_info AS(
  SELECT
    s2id,
    ST_GEOGPOINT(lon,
      lat) from_anchorage_position
  FROM
    `world-fishing-827.gfw_research.named_anchorages_v20180803_13b`),
  grided_filtered AS(
  SELECT
    *
  FROM
    gridded_info
  WHERE
    total_hours > 0
    AND total_distance_km >0),
  master AS(
  SELECT
    *
  FROM
    grided_filtered
  LEFT JOIN
    voyage_info USING(mmsi,
      trip_id)
  LEFT JOIN
    fuel_prices
  ON
    voyage_info.departure_date = fuel_prices.date
  LEFT JOIN
    from_anchorage_info
  ON
    voyage_info.from_anchorage_id = from_anchorage_info.s2id
  LEFT JOIN
    to_anchorage_info
  ON
    voyage_info.to_anchorage_id = to_anchorage_info.s2id
  LEFT JOIN
    vessel_info USING(mmsi,
      year)),
  master_2 AS(
  SELECT
    mmsi,
    vessel_type,
    flag,
    year,
    length length_m,
    crew,
    tonnage tonnage_gt,
    engine_power engine_power_kW,
    aux_engine_power aux_engine_power_kW,
    design_speed design_speed_knots,
    main_sfc main_sfc_g_per_kWH,
    aux_sfc aux_sfc_g_per_kWH,
    trip_id,
    from_port,
    from_country,
    to_port,
    to_country,
    departure_timestamp,
    arrival_timestamp,
    DATE(departure_timestamp) departure_date,
    DATE(arrival_timestamp) arrival_date,
    total_distance_km,
    ST_DISTANCE(from_anchorage_position,
      to_anchorage_position)/1000 total_haversine_distance_km,
    total_hours,
    implied_speed_knots,
    {clusters_aggregated_2},
    attack_grid_distance_km,
    attack_grid_hours,
    number_attack_grids,
    days_since_attack,
    attacks_last_1_year,
    attacks_last_2_years,
    attacks_last_3_years,
    speed_m_s wind_speed_m_per_s,
    direction_degrees wind_direction_degrees,
    wind_vector,
    price_usd_mt,
    main_fuel_consumption_mt_inst,
    total_fuel_consumption_mt_inst,
    aux_fuel_consumption_mt_inst,
    total_hours*(0.8 * POW(implied_speed_knots/design_speed, 3))*main_sfc*engine_power/1000000 main_fuel_consumption_mt_voyage,
    total_hours*0.5*aux_sfc*aux_engine_power/1000000 aux_fuel_consumption_mt_voyage,
    (total_hours*(0.8 * POW(implied_speed_knots/design_speed, 3))*main_sfc*engine_power/1000000 + total_hours*0.5*aux_sfc*aux_engine_power/1000000) total_fuel_consumption_mt_voyage
  FROM
    master)
SELECT
  *,
  price_usd_mt * total_fuel_consumption_mt_voyage total_fuel_cost_usd_voyage,
  3.17 * total_fuel_consumption_mt_voyage emissions_co2_kg_voyage,
  87 * main_fuel_consumption_mt_voyage + 57 * aux_fuel_consumption_mt_voyage emissions_nox_kg_voyage,
  20 * 3.3 * total_fuel_consumption_mt_voyage emissions_sox_kg_voyage,
  price_usd_mt * total_fuel_consumption_mt_inst total_fuel_cost_usd_inst,
  3.17 * total_fuel_consumption_mt_inst emissions_co2_kg_inst,
  87 * main_fuel_consumption_mt_inst + 57 * aux_fuel_consumption_mt_inst emissions_nox_kg_inst,
  20 * 3.3 * total_fuel_consumption_mt_inst emissions_sox_kg_inst
FROM
  master_2
"
)

bq_table(project = project,table = "voyages_bad",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "voyages_bad",dataset = "piracy"),
                 allowLargeResults = TRUE)

# Create table that defines port-to-port distance cutoff values
# Based on all voyages, then add column for outliers
sql <- "#standardSQL
  WITH outlier_info AS(
  SELECT
    to_port,
    from_port,
    AVG(total_distance_km) mean_distance,
    STDDEV(total_distance_km) sd_distance,
    GREATEST(AVG(total_distance_km) - 2 * STDDEV(total_distance_km),0) min_distance_cutoff_2sd,
    AVG(total_distance_km) + 2 * STDDEV(total_distance_km) max_distance_cutoff_2sd,
    GREATEST(AVG(total_distance_km) - 3 * STDDEV(total_distance_km),0) min_distance_cutoff_3sd,
    AVG(total_distance_km) + 3 * STDDEV(total_distance_km) max_distance_cutoff_3sd
  FROM
    `ucsb-gfw.piracy.voyages_bad`
  GROUP BY
    from_port,
    to_port)
SELECT
  *,
  IF(total_distance_km < min_distance_cutoff_2sd OR total_distance_km > max_distance_cutoff_2sd,TRUE,FALSE) outlier_2sd,
  IF(total_distance_km < min_distance_cutoff_3sd OR total_distance_km > max_distance_cutoff_3sd,TRUE,FALSE) outlier_3sd
FROM
  `ucsb-gfw.piracy.voyages_bad`
LEFT JOIN
  outlier_info USING(to_port,
    from_port)"

bq_table(project = project,table = "voyages_with_outliers_bad",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "voyages_with_outliers_bad",dataset = "piracy"),
                 allowLargeResults = TRUE)
