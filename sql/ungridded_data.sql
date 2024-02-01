#standardSQL
WITH
  vessel_info AS(
  SELECT
    mmsi,
    engine_power,
    aux_engine_power,
    design_speed
  FROM
    `emlab-gcp.piracy.vessel_info` ),
  voyage_info AS(
  SELECT
    voyage_mmsi,
    trip_id,
    departure_timestamp,
    arrival_timestamp
  FROM
    `emlab-gcp.piracy.voyage_info` ),
  # Select good segments
  good_segments AS (
  SELECT
    seg_id
  FROM
    `world-fishing-827.pipe_production_v20201001.research_segs`
  WHERE
    good_seg
    AND NOT overlapping_and_short ),
  # Select the AIS messages
  ais_info AS(
  SELECT
    CAST(ssvid AS INT64) mmsi,
    lat,
    lon,
    timestamp,
    hours,
    implied_speed_knots,
    heading,
    meters_to_prev/1000 distance_km
  FROM
    `world-fishing-827.pipe_production_v20201001.research_messages`
    # Get all data for 2013 and beyond
  WHERE
    _partitiontime >= '2013-01-01'
    # Only use good segments for AIS messages
    AND seg_id IN (
    SELECT
      seg_id
    FROM
      good_segments)),
  shipping_ais_info AS(
  SELECT
    *
  FROM
    ais_info
  JOIN
    vessel_info
  USING
    (mmsi)),
  shipping_ais_info_with_voyages AS(
  SELECT
    * EXCEPT(voyage_mmsi)
  FROM
    shipping_ais_info
  JOIN
    voyage_info
  ON
    shipping_ais_info.mmsi = voyage_info.voyage_mmsi
    AND shipping_ais_info.timestamp > voyage_info.departure_timestamp
    AND shipping_ais_info.timestamp <= voyage_info.arrival_timestamp)
SELECT
  * EXCEPT(engine_power,
    aux_engine_power,
    design_speed,
    implied_speed_knots,
    arrival_timestamp),
  # Calculate fuel consumption
  # main_sfc is always 206; aux_sfc is always 221
  # sfc from here:https://www.sciencedirect.com/science/article/pii/S1361920909001072#bib18
  # Ensure implied speed never exceed design speed. If it does, set this to 1 for calculating fuel consumption
  hours*(0.8 * POW(
    IF
      (implied_speed_knots/design_speed>1,1,implied_speed_knots/design_speed), 3))*206*engine_power/1000000 main_fuel_consumption_mt_inst,
  hours*0.5*221*aux_engine_power/1000000 aux_fuel_consumption_mt_inst
FROM
  shipping_ais_info_with_voyages