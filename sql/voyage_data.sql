#standardSQL
CREATE TEMP FUNCTION
  RADIANS(x FLOAT64) AS ( ACOS(-1) * x / 180 );
CREATE TEMPORARY FUNCTION
  pixel_size() AS ({pixel_size});
WITH
vessel_info AS(
  SELECT
    *
  FROM
    `emlab-gcp.piracy.{vessel_info_table}` ),
  voyage_info AS(
  SELECT
    trip_id,
    from_port,
    from_country,
    to_port,
    to_country,
    total_haversine_distance_km
  FROM
    `emlab-gcp.piracy.{voyage_info_table}` ),
  -- Next add gridded 5x5 degree data, the primary grid size from which our
  -- voyage level data are constructed
  gridded_data AS(
  SELECT
    *,
    -- We will add wind and wave data at the actual date on which activity occurred, not the departure date
    DATE_TRUNC(DATE(timestamp),MONTH) truncated_date
  FROM
    `emlab-gcp.piracy.{gridded_data_5_table}`),
     -- Load 5x5 degree wind data
  wind_info AS(
  SELECT
    DATE_TRUNC(date,MONTH) truncated_date,
    lat_bin,
    lon_bin,
    wind_speed_ms,
    wind_direction_degrees
  FROM
    `emlab-gcp.piracy.{wind_table}`),
      -- Load 5x5 degree wave data
  wave_info AS(
  SELECT
    DATE_TRUNC(date,MONTH) truncated_date,
    lat_bin,
    lon_bin,
    surface_wave_height_m
  FROM
    `emlab-gcp.piracy.{wave_table}`),
  -- Now add wind and wave data to gridded data by appropriate location, month and year
  gridded_data_wind_and_waves as(
    SELECT
    *,
    -- Calculate wind vector, which combines wind speed and vessel heading
    COS(RADIANS(wind_direction_degrees - heading)) * wind_speed_ms wind_vector
    FROM
    gridded_data
    LEFT JOIN
    wind_info
    USING(lat_bin,lon_bin,truncated_date)
    LEFT JOIN
    wave_info
    USING(lat_bin,lon_bin,truncated_date)
  ),
  # We will add fuel prices based on the voyage departure date
  fuel_prices AS(
  SELECT
    * EXCEPT(date),
    date departure_date
  FROM
    `emlab-gcp.piracy.{fuel_price_table}`),
  aggregated AS(
  SELECT
    mmsi,
    trip_id,
    departure_date,
    SUM(hours) hours,
    SUM(distance_km) distance_km,
    SUM(ais_messages) ais_messages,
    AVG(wind_vector) wind_vector,
    AVG(wind_speed_ms) wind_speed_ms,
    AVG(surface_wave_height_m) surface_wave_height_m,
    SUM(grid_area_km2) voyage_grid_area_km2,
    SUM(main_fuel_consumption_mt_inst) main_fuel_consumption_mt_inst,
    SUM(aux_fuel_consumption_mt_inst) aux_fuel_consumption_mt_inst,
    SUM(main_fuel_consumption_mt_inst + aux_fuel_consumption_mt_inst) total_fuel_consumption_mt_inst,
    SUM(number_previous_attacks_grid_7_days) number_previous_attacks_7_days_5_degrees,
    SUM(number_previous_attacks_grid_15_days) number_previous_attacks_15_days_5_degrees,
    SUM(number_previous_attacks_grid_1_month) number_previous_attacks_1_month_5_degrees,
    SUM(number_previous_attacks_grid_3_months) number_previous_attacks_3_months_5_degrees,
    SUM(number_previous_attacks_grid_6_months) number_previous_attacks_6_months_5_degrees,
    SUM(number_previous_attacks_grid_12_months) number_previous_attacks_12_months_5_degrees,
    SUM(number_future_attacks_grid_7_days) number_future_attacks_7_days_5_degrees,
    SUM(number_future_attacks_grid_15_days) number_future_attacks_15_days_5_degrees,
    SUM(number_future_attacks_grid_1_month) number_future_attacks_1_month_5_degrees,
    SUM(number_future_attacks_grid_3_months) number_future_attacks_3_months_5_degrees,
    SUM(number_future_attacks_grid_6_months) number_future_attacks_6_months_5_degrees,
    SUM(number_future_attacks_grid_12_months) number_future_attacks_12_months_5_degrees,
    MIN(days_since_attack) days_since_attack,
    SUM(hotspot_southeast_asia) hotspot_southeast_asia,
    SUM(hotspot_gulf_of_aden) hotspot_gulf_of_aden,
    SUM(hotspot_gulf_of_guinea) hotspot_gulf_of_guinea
  FROM
    gridded_data_wind_and_waves
  GROUP BY
    mmsi,
    trip_id,
    departure_date,
    year),
    # For robustness check, add number_previous_attacks_* for 3 degree version of gridded dataset
  aggregated_3_degrees AS(
  SELECT
    trip_id,
    SUM(number_previous_attacks_grid_7_days) number_previous_attacks_7_days_3_degrees,
    SUM(number_previous_attacks_grid_15_days) number_previous_attacks_15_days_3_degrees,
    SUM(number_previous_attacks_grid_1_month) number_previous_attacks_1_month_3_degrees,
    SUM(number_previous_attacks_grid_3_months) number_previous_attacks_3_months_3_degrees,
    SUM(number_previous_attacks_grid_6_months) number_previous_attacks_6_months_3_degrees,
    SUM(number_previous_attacks_grid_12_months) number_previous_attacks_12_months_3_degrees
  FROM
    `emlab-gcp.piracy.{gridded_data_3_table}`
  GROUP BY
    trip_id),
    # For robustness check, add number_previous_attacks_* for 7 degree version of gridded dataset
  aggregated_7_degrees AS(
  SELECT
    trip_id,
    SUM(number_previous_attacks_grid_7_days) number_previous_attacks_7_days_7_degrees,
    SUM(number_previous_attacks_grid_15_days) number_previous_attacks_15_days_7_degrees,
    SUM(number_previous_attacks_grid_1_month) number_previous_attacks_1_month_7_degrees,
    SUM(number_previous_attacks_grid_3_months) number_previous_attacks_3_months_7_degrees,
    SUM(number_previous_attacks_grid_6_months) number_previous_attacks_6_months_7_degrees,
    SUM(number_previous_attacks_grid_12_months) number_previous_attacks_12_months_7_degrees
  FROM
    `emlab-gcp.piracy.{gridded_data_7_table}`
  GROUP BY
    trip_id),
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
# For each trip, this summarize the total number of attacks in the grids
# that voyages have previously passed through for that route (including the current voyage),
# over different rolling windows
# Load 5 degree version
total_rolling_route_attacks_per_trip_5_degrees AS(
SELECT
  *
FROM 
`emlab-gcp.piracy.{total_rolling_route_attacks_per_trip_5_table}`
),
# Load 3 degree version
total_rolling_route_attacks_per_trip_3_degrees AS(
SELECT
  *
FROM 
`emlab-gcp.piracy.{total_rolling_route_attacks_per_trip_3_table}`
),
# Load 7 degree version
total_rolling_route_attacks_per_trip_7_degrees AS(
SELECT
  *
FROM 
`emlab-gcp.piracy.{total_rolling_route_attacks_per_trip_7_table}`
),
# For each trip, this summarize the average number of attacks previous trips and grids
# that voyages have previously passed through for that route (not including the current voyage),
# over different rolling windows
# Load 5 degree version
average_rolling_route_attacks_per_trip_5_degrees AS(
SELECT
  *
FROM 
`emlab-gcp.piracy.{average_rolling_route_attacks_per_trip_5_table}`
),
# Load 3 degree version
average_rolling_route_attacks_per_trip_3_degrees AS(
SELECT
  *
FROM 
`emlab-gcp.piracy.{average_rolling_route_attacks_per_trip_3_table}`
),
# Load 7 degree version
average_rolling_route_attacks_per_trip_7_degrees AS(
SELECT
  *
FROM 
`emlab-gcp.piracy.{average_rolling_route_attacks_per_trip_7_table}`
)
SELECT
  * EXCEPT(main_fuel_consumption_mt_inst,
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
# Add attack indicators for 3 degree robustness check
LEFT JOIN
aggregated_3_degrees
USING(trip_id)
# Add attack indicators for 7 degree robustness check
LEFT JOIN
aggregated_7_degrees
USING(trip_id)
# Add attack indicators for total previous attacks along previous grids
LEFT  JOIN
total_rolling_route_attacks_per_trip_5_degrees
USING
(trip_id)
LEFT  JOIN
total_rolling_route_attacks_per_trip_3_degrees
USING
(trip_id)
LEFT  JOIN
total_rolling_route_attacks_per_trip_7_degrees
USING
(trip_id)
# Add attack indicators for average previous attacks along previous trips and previous grids
LEFT  JOIN
average_rolling_route_attacks_per_trip_5_degrees
USING
(trip_id)
LEFT  JOIN
average_rolling_route_attacks_per_trip_3_degrees
USING
(trip_id)
LEFT  JOIN
average_rolling_route_attacks_per_trip_7_degrees
USING
(trip_id)
# Add monthly fuel prices
LEFT JOIN
  fuel_prices
USING
  (departure_date)
# Add voyage info
    LEFT JOIN
  voyage_info
USING
  (trip_id)