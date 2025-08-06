# Query description:
# This generates the unguided data (e.g., AIS-ping-level) for 2013-2021, for all of our vessels of interest, 
# matched to all of our voyages of interest. This also calculates ping-level fuel consumption
#standardSQL
WITH
  vessel_info AS(
  SELECT
    mmsi,
    engine_power,
    aux_engine_power,
    design_speed
  FROM
    `emlab-gcp.piracy.{vessel_info_table}` ),
  voyage_info AS(
  SELECT
    mmsi,
    trip_id,
    departure_timestamp,
    arrival_timestamp
  FROM
    `emlab-gcp.piracy.{voyage_info_table}` ),
  ais_info AS(
  SELECT
    ssvid mmsi,
    lat,
    lon,
    timestamp,
    hours,
    implied_speed_knots,
    heading,
    meters_to_prev/1000 distance_km
  FROM
    `world-fishing-827.pipe_ais_v3_published.messages`
  WHERE
    DATE(timestamp) BETWEEN '{study_period_starting_date}'
    AND '{study_period_ending_date}'
    AND clean_segs),
  # Filter AIS messages to just those broadcast by our vessels of interest (e.g., cargo vessels)
  shipping_ais_info AS(
  SELECT
    *
  FROM
    ais_info
  JOIN
    vessel_info
  USING
    (mmsi)),
  # Now JOIN those AIS messages to the voyage data, so that each AIS message is assigned to a voyage
  shipping_ais_info_with_voyages AS(
  SELECT
    shipping_ais_info.mmsi mmsi,
    trip_id,
    departure_timestamp,
    arrival_timestamp,
    lat,
    lon,
    timestamp,
    hours,
    implied_speed_knots,
    heading,
    distance_km,
  # Calculate fuel consumption
  # main_sfc is always 206; aux_sfc is always 221
  # sfc from here:https://www.sciencedirect.com/science/article/pii/S1361920909001072#bib18
  # Ensure implied speed never exceed design speed. If it does, set this to 1 for calculating fuel consumption
  hours*(0.8 * POW(
    IF
      (implied_speed_knots/design_speed>1,1,implied_speed_knots/design_speed), 3))*206*engine_power/1000000 main_fuel_consumption_mt_inst,
  hours*0.5*221*aux_engine_power/1000000 aux_fuel_consumption_mt_inst
  FROM
    shipping_ais_info
  JOIN
    voyage_info
  ON
    shipping_ais_info.mmsi = voyage_info.mmsi
    AND shipping_ais_info.timestamp > voyage_info.departure_timestamp
    AND shipping_ais_info.timestamp <= voyage_info.arrival_timestamp)
SELECT
  * 
FROM
  shipping_ais_info_with_voyages