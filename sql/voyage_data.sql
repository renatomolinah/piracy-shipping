#standardSQL
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
WITH
vessel_info AS(
  SELECT
    *
  FROM
    `emlab-gcp.piracy.vessel_info_v_20240228` ),
  voyage_info AS(
  SELECT
    trip_id,
    from_port,
    from_country,
    to_port,
    to_country,
    total_haversine_distance_km
  FROM
    `emlab-gcp.piracy.voyage_info_v_20240228` ),
  gridded_data AS(
  SELECT
    *
  FROM
    `emlab-gcp.piracy.gridded_data_5_v_20240307`),
  # We will add fuel prices based on the voyage departure date
  fuel_prices AS(
  SELECT
    * EXCEPT(date),
    date departure_date
  FROM
    `emlab-gcp.piracy.fuel_prices_v_20240228`),
  aggregated AS(
  SELECT
    mmsi,
    trip_id,
    departure_date,
    SUM(hours) hours,
    SUM(distance_km) distance_km,
    SUM(ais_messages) ais_messages,
    AVG(wind_vector) wind_vector,
    SUM(grid_area_km2) voyage_grid_area_km2,
    SUM(main_fuel_consumption_mt_inst) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) aux_fuel_consumption_mt_inst,
    SUM(main_fuel_consumption_mt_inst + aux_fuel_consumption_mt_inst) total_fuel_consumption_mt_inst,
    SUM(number_previous_attacks_grid_all_time) number_previous_attacks_all_time,
    SUM(number_previous_attacks_grid_12_months) number_previous_attacks_12_months,
    SUM(number_previous_attacks_grid_12_months_all_encounters) number_previous_attacks_12_months_all_encounter_types,
    MIN(days_since_attack) days_since_attack,
    SUM(hotspot_southeast_asia) hotspot_southeast_asia,
    SUM(hotspot_gulf_of_aden) hotspot_gulf_of_aden,
    SUM(hotspot_gulf_of_guinea) hotspot_gulf_of_guinea
  FROM
    gridded_data
  GROUP BY
    mmsi,
    trip_id,
    departure_date,
    year),
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
    aggregated 
     LEFT JOIN
  vessel_info
USING
  (mmsi)),
average_route_attacks_per_route_and_trip AS(
SELECT
*
FROM 
`emlab-gcp.piracy.average_route_attacks_per_route_and_trip_v_20240307`
)
SELECT
  * EXCEPT(main_fuel_consumption_mt_inst,
      aux_fuel_consumption_mt_inst,
      main_fuel_consumption_mt_voyage,
      aux_fuel_consumption_mt_voyage,
      price_usd_mt,
      hotspot_southeast_asia,
      hotspot_gulf_of_aden,
      hotspot_gulf_of_guinea,
  average_route_attacks_last_3_months,
  average_route_attacks_last_6_months,
  average_route_attacks_last_9_months,
  average_route_attacks_last_12_months,
  average_route_attacks_last_24_months,
  average_route_attacks_last_3_months_all_encounter_types),
  IFNULL(average_route_attacks_last_3_months,0) average_route_attacks_last_3_months,
  IFNULL(average_route_attacks_last_6_months,0) average_route_attacks_last_6_months,
  IFNULL(average_route_attacks_last_9_months,0) average_route_attacks_last_9_months,
  IFNULL(average_route_attacks_last_12_months,0) average_route_attacks_last_12_months,
  IFNULL(average_route_attacks_last_24_months,0) average_route_attacks_last_24_months,
  IFNULL(average_route_attacks_last_3_months_all_encounter_types,0) average_route_attacks_last_3_months_all_encounter_types,
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
LEFT  JOIN
average_route_attacks_per_route_and_trip
USING
(trip_id)
LEFT JOIN
  fuel_prices
USING
  (departure_date)
    LEFT JOIN
  voyage_info
USING
  (trip_id)
  # Only include voyages that have some time and distance
    WHERE
    hours > 0
    AND distance_km >0