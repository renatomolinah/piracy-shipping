WITH
  gridded_data AS(
  SELECT
    *,
    IF(NOT days_since_attack IS NULL,TRUE,FALSE) grid_has_previous_attacks,
    IF(NOT days_since_attack IS NULL,distance_km,0) attack_grid_distance_km,
    IF(NOT days_since_attack IS NULL,hours,0) attack_grid_hours
  FROM
    `emlab-gcp.piracy.{gridded_data_table_location}`
    WHERE
    hours > 0
    AND distance_km >0),
  fuel_prices AS(
  SELECT
    *
  FROM
    `emlab-gcp.ucsb_gfw_legacy_piracy.daily_fuel_prices`
  WHERE
    fuel_index = 'BIX 380 CST' ),
  aggregated AS(
  SELECT
    DISTINCT * EXCEPT(hours,
      distance_km,
      main_fuel_consumption_mt_inst,
      aux_fuel_consumption_mt_inst,
      ais_messages,
      wind_vector,
      days_since_attack,
      lat_bin,
      lon_bin,
      attacks_window_last_1_month,
      attacks_window_last_2_month,
      attacks_window_last_3_month,
      attacks_window_last_4_month,
      attacks_window_last_5_month,
      attacks_window_last_6_month,
      grid_has_previous_attacks,
      attack_grid_distance_km,
      attack_grid_hours),
    SUM(hours) OVER(PARTITION BY trip_id) hours,
    SUM(distance_km) OVER(PARTITION BY trip_id) distance_km,
    SUM(ais_messages) OVER(PARTITION BY trip_id) ais_messages,
    AVG(wind_vector) OVER(PARTITION BY trip_id) wind_vector,
    SUM(main_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) aux_fuel_consumption_mt_inst,
    SUM(main_fuel_consumption_mt_inst + aux_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) total_fuel_consumption_mt_inst,
    SUM(IF(grid_has_previous_attacks,1,0)) OVER(PARTITION BY trip_id) number_attack_grids,
    MIN(days_since_attack) OVER(PARTITION BY trip_id) days_since_attack,
    SUM(attack_grid_distance_km) OVER(PARTITION BY trip_id) attack_grid_distance_km,
    SUM(attack_grid_hours) OVER(PARTITION BY trip_id) attack_grid_hours
  FROM
    gridded_data),
  aggregated_with_voyage_fuel AS(
  SELECT
    *,
    hours*(0.8 * POW(
      IF
        ((distance_km * 0.539957 / hours)/design_speed>1,1,(distance_km * 0.539957 / hours)/design_speed), 3))*main_sfc*engine_power/1000000 main_fuel_consumption_mt_voyage,
    hours*0.5*aux_sfc*aux_engine_power/1000000 aux_fuel_consumption_mt_voyage,
    (hours*(0.8 * POW(
        IF
          ((distance_km * 0.539957 / hours)/design_speed>1,1,(distance_km * 0.539957 / hours)/design_speed), 3))*main_sfc*engine_power/1000000 + hours*0.5*aux_sfc*aux_engine_power/1000000) total_fuel_consumption_mt_voyage
  FROM
    aggregated )
SELECT
  * EXCEPT(main_fuel_consumption_mt_inst,
    aux_fuel_consumption_mt_inst,
    main_fuel_consumption_mt_voyage,
    aux_fuel_consumption_mt_voyage,
    design_speed,
    main_sfc,
    aux_sfc,
    fuel_index,
    price_usd_mt,
    departure_timestamp,
    arrival_timestamp),
  price_usd_mt * total_fuel_consumption_mt_voyage total_fuel_cost_usd_voyage,
  3.17 * total_fuel_consumption_mt_voyage emissions_co2_mt_voyage,
  87 * main_fuel_consumption_mt_voyage + 57 * aux_fuel_consumption_mt_voyage emissions_nox_kg_voyage,
  20 * 3.3 * total_fuel_consumption_mt_voyage emissions_sox_kg_voyage,
  price_usd_mt * total_fuel_consumption_mt_inst total_fuel_cost_usd_inst,
  3.17 * total_fuel_consumption_mt_inst emissions_co2_mt_inst,
  87 * main_fuel_consumption_mt_inst + 57 * aux_fuel_consumption_mt_inst emissions_nox_kg_inst,
  20 * 3.3 * total_fuel_consumption_mt_inst emissions_sox_kg_inst
FROM
  aggregated_with_voyage_fuel
LEFT JOIN
  fuel_prices
USING
  (date)