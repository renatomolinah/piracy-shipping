#standardSQL
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
WITH
  wind_info AS(
  SELECT
    EXTRACT(YEAR
    FROM
      date) year,
    EXTRACT(MONTH
    FROM
      date) month,
    lat_bin,
    lon_bin,
    wind_speed_ms,
    wind_direction_degrees
  FROM
    `emlab-gcp.piracy.{wind_table_location}`),
  gridded_data AS(
  SELECT
    *
    EXCEPT(wind_direction_degrees,heading),
  COS(RADIANS(wind_direction_degrees - heading)) * wind_speed_ms wind_vector
  FROM
    `emlab-gcp.piracy.{gridded_data_table_location}`
      # Add wind info
LEFT JOIN
  wind_info
USING
  (month,
    year,
    lat_bin,
    lon_bin)
    WHERE
    hours > 0
    AND distance_km >0),
  fuel_prices AS(
  SELECT
    *
  FROM
    `emlab-gcp.piracy.fuel_prices`),
  aggregated AS(
  SELECT
    DISTINCT * EXCEPT(hours,
      distance_km,
      main_fuel_consumption_mt_inst,
      aux_fuel_consumption_mt_inst,
      ais_messages,
      wind_vector,
      wind_speed_ms,
      days_since_attack,
      lat_bin,
      lon_bin,
      grid_area_km2,
      number_previous_attacks_grid_all_time,
      number_previous_attacks_grid_12_months,
      number_previous_attacks_grid_12_months_all_encounter_types,
      grid_has_previous_attacks,
      hotspot_southeast_asia,
      hotspot_gulf_of_aden,
      hotspot_gulf_of_guinea,
      grid_attacked_in_study_period),
    SUM(hours) OVER(PARTITION BY trip_id) hours,
    SUM(distance_km) OVER(PARTITION BY trip_id) distance_km,
    SUM(ais_messages) OVER(PARTITION BY trip_id) ais_messages,
    AVG(wind_speed_ms) OVER(PARTITION BY trip_id) wind_speed_ms,
    AVG(wind_vector) OVER(PARTITION BY trip_id) wind_vector,
    SUM(grid_area_km2) OVER(PARTITION BY trip_id) voyage_grid_area_km2,
    SUM(main_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) aux_fuel_consumption_mt_inst,
    SUM(main_fuel_consumption_mt_inst + aux_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) total_fuel_consumption_mt_inst,
    SUM(number_previous_attacks_grid_all_time) OVER(PARTITION BY trip_id) number_previous_attacks_all_time,
    SUM(number_previous_attacks_grid_12_months) OVER(PARTITION BY trip_id) number_previous_attacks_12_months,
    SUM(number_previous_attacks_grid_12_months_all_encounter_types) OVER(PARTITION BY trip_id) number_previous_attacks_12_months_all_encounter_types,
    MIN(days_since_attack) OVER(PARTITION BY trip_id) days_since_attack,
    SUM(hotspot_southeast_asia) OVER(PARTITION BY trip_id) hotspot_southeast_asia,
    SUM(hotspot_gulf_of_aden) OVER(PARTITION BY trip_id) hotspot_gulf_of_aden,
    SUM(hotspot_gulf_of_guinea) OVER(PARTITION BY trip_id) hotspot_gulf_of_guinea
  FROM
    gridded_data),
  aggregated_with_voyage_fuel AS(
  SELECT
    *,
    # main_sfc is always 206; aux_sfc is always 221
  # sfc from here:https://www.sciencedirect.com/science/article/pii/S1361920909001072#bib18
    hours*(0.8 * POW(
      IF
        ((distance_km * 0.539957 / hours)/design_speed>1,1,(distance_km * 0.539957 / hours)/design_speed), 3))*206*engine_power/1000000 main_fuel_consumption_mt_voyage,
    hours*0.5*221*aux_engine_power/1000000 aux_fuel_consumption_mt_voyage,
    (hours*(0.8 * POW(
        IF
          ((distance_km * 0.539957 / hours)/design_speed>1,1,(distance_km * 0.539957 / hours)/design_speed), 3))*206*engine_power/1000000 + hours*0.5*221*aux_engine_power/1000000) total_fuel_consumption_mt_voyage
  FROM
    aggregated ),
average_route_attacks_per_route_and_trip AS(
SELECT
*
FROM 
`emlab-gcp.piracy.average_route_attacks_per_route_and_trip`
)
SELECT
  * EXCEPT(month,
  year,
  main_fuel_consumption_mt_inst,
    aux_fuel_consumption_mt_inst,
    main_fuel_consumption_mt_voyage,
    aux_fuel_consumption_mt_voyage,
    price_usd_mt,
      hotspot_southeast_asia,
      hotspot_gulf_of_aden,
      hotspot_gulf_of_guinea),
  price_usd_mt * total_fuel_consumption_mt_voyage total_fuel_cost_usd_voyage,
  3.17 * total_fuel_consumption_mt_voyage emissions_co2_mt_voyage,
  87 * main_fuel_consumption_mt_voyage + 57 * aux_fuel_consumption_mt_voyage emissions_nox_kg_voyage,
  20 * 3.3 * total_fuel_consumption_mt_voyage emissions_sox_kg_voyage,
  price_usd_mt * total_fuel_consumption_mt_inst total_fuel_cost_usd_inst,
  3.17 * total_fuel_consumption_mt_inst emissions_co2_mt_inst,
  87 * main_fuel_consumption_mt_inst + 57 * aux_fuel_consumption_mt_inst emissions_nox_kg_inst,
  20 * 3.3 * total_fuel_consumption_mt_inst emissions_sox_kg_inst,
  IF(hotspot_southeast_asia>0,TRUE,FALSE) hotspot_southeast_asia,
  IF(hotspot_gulf_of_aden>0,TRUE,FALSE) hotspot_gulf_of_aden,
  IF(hotspot_gulf_of_guinea>0,TRUE,FALSE) hotspot_gulf_of_guinea
FROM
  aggregated_with_voyage_fuel
  JOIN
average_route_attacks_per_route_and_trip
USING
(trip_id)
LEFT JOIN
  fuel_prices
USING
  (date)
