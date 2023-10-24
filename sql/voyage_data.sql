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
    EXCEPT(wind_direction_degrees,heading,wind_speed_ms),
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
      days_since_attack,
      lat_bin,
      lon_bin,
      grid_area_km2,
      number_previous_attacks_grid_all_time,
      grid_has_previous_attacks),
    SUM(hours) OVER(PARTITION BY trip_id) hours,
    SUM(distance_km) OVER(PARTITION BY trip_id) distance_km,
    SUM(ais_messages) OVER(PARTITION BY trip_id) ais_messages,
    AVG(wind_vector) OVER(PARTITION BY trip_id) wind_vector,
    SUM(grid_area_km2) OVER(PARTITION BY trip_id) voyage_grid_area_km2,
    SUM(main_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) aux_fuel_consumption_mt_inst,
    SUM(main_fuel_consumption_mt_inst + aux_fuel_consumption_mt_inst) OVER(PARTITION BY trip_id) total_fuel_consumption_mt_inst,
    SUM(number_previous_attacks_grid_all_time) OVER(PARTITION BY trip_id) number_previous_attacks_all_time,
    MIN(days_since_attack) OVER(PARTITION BY trip_id) days_since_attack
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
    main_sfc,
    aux_sfc,
    price_usd_mt,
    departure_timestamp,
    arrival_timestamp,
    from_anchorage_id,
    to_anchorage_id),
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
  JOIN
average_route_attacks_per_route_and_trip
USING
(trip_id)
LEFT JOIN
  fuel_prices
USING
  (date)
