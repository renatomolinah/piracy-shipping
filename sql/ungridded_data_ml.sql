#standardSQL
WITH
  # Select good segments
  good_segments AS (
  SELECT
    seg_id
  FROM
    `world-fishing-827.gfw_research.pipe_v20201001_segs`
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
    meters_to_prev/1000 avg_distance_km,
    EXTRACT(YEAR
    FROM
      timestamp) year,
    EXTRACT(DATE
    FROM
      timestamp) date
  FROM
    `world-fishing-827.gfw_research.pipe_v20201001`
    # Only subset to a small date range for now
  WHERE
    _partitiontime < '2017-12-31'
    # Only use good segments for AIS messages
    AND seg_id IN (
    SELECT
      seg_id
    FROM
      good_segments)),
  # Get relevant vessel info for bunkers, reefer, cargo, and tankers
  vessel_info AS(
  SELECT
    CAST(ssvid AS INT64) mmsi,
    year,
    best.best_flag flag,
    best.best_vessel_class vessel_type,
    best.best_length_m length,
    best.best_tonnage_gt tonnage,
    best.best_engine_power_kw engine_power,
    best.best_crew_size crew,
    # From Bren GP, page 130
    # Based on linear regression of known vessels
    # https://www.bren.ucsb.edu/research/documents/whales_report.pdf
    0.1913 * best.best_engine_power_kw + 287.2 aux_engine_power,
    # Add design speed
    # From Bren GP, page 131
    # Based on linear regression of known vessels
    # https://www.bren.ucsb.edu/research/documents/whales_report.pdf
    3.39*POW(10,-4)*best.best_engine_power_kw+2.151*POW(10,-5)*best.best_tonnage_gt-2.742*POW(10,-9)*best.best_engine_power_kw*best.best_tonnage_gt+12.93 design_speed,
    # Add SFC from here:https://www.sciencedirect.com/science/article/pii/S1361920909001072#bib18
    206 main_sfc,
    221 aux_sfc
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20220101`
  WHERE
    best.best_vessel_class IN('cargo',
      'cargo_or_tanker',
      'tanker',
      'cargo_or_reefer',
      'specialized_reefer',
      'container_reefer',
      'reefer')
    # Ensure it's a reasonable vessel
    AND NOT activity.offsetting
    AND activity.overlap_hours_multinames = 0
    AND year < 2018),
  # Get anchorage, port, and country info for start of voyage
  from_anchorage_info AS(
  SELECT
    s2id from_anchorage_id,
    label from_port,
    iso3 from_country
  FROM
    `world-fishing-827.gfw_research.named_anchorages`),
  # Get anchorage, port, and country info for end of voyage
  to_anchorage_info AS(
  SELECT
    s2id to_anchorage_id,
    label to_port,
    iso3 to_country
  FROM
    `world-fishing-827.gfw_research.named_anchorages`),
  # Get voyage info
  # Using highest confidence voyages - see https://github.com/GlobalFishingWatch/bigquery-documentation-wf827/wiki/Anchorages-and-voyages
  voyages_base AS (
  SELECT
    CAST(ssvid AS INT64) mmsi,
    trip_start departure_timestamp,
    trip_end arrival_timestamp,
    trip_start_anchorage_id from_anchorage_id,
    trip_end_anchorage_id to_anchorage_id,
    trip_id
  FROM
    `world-fishing-827.pipe_production_v20201001.proto_voyages_c4`),
  # Combine voyage info with anchorages, ports, and countries
  voyages AS(
  SELECT
    *
  FROM
    voyages_base
  LEFT JOIN
    from_anchorage_info
  USING
    (from_anchorage_id)
  LEFT JOIN
    to_anchorage_info
  USING
    (to_anchorage_id)
  WHERE
    NOT from_port = to_port),
  # Assign AIS positions to voyages based on MMSI and timestamps
  voyages_ais_positions AS(
  SELECT
    ais_info.mmsi,
    ais_info.hours,
    ais_info.heading,
    ais_info.avg_distance_km,
    ais_info.year,
    ais_info.date,
    voyages.trip_id,
    # Calculate fuel consumption
    #main_sfc is always 206; aux_sfc is always 221
    hours*(0.8 * POW(implied_speed_knots/design_speed, 3))*206*engine_power/1000000 main_fuel_consumption_mt_inst,
    hours*0.5*221*aux_engine_power/1000000 aux_fuel_consumption_mt_inst,
    # Create grids, which we'll use to aggregate,
    lat,
    lon
    # Start with AIS messages
  FROM
    ais_info
    # Add vessel info
  LEFT JOIN
    vessel_info
  USING
    (mmsi,
      year)
    # Add voyage info
  JOIN
    voyages
  ON
    ais_info.mmsi = voyages.mmsi
    AND ais_info.timestamp > voyages.departure_timestamp
    AND ais_info.timestamp < voyages.arrival_timestamp)
SELECT
  *
FROM
  voyages_ais_positions
JOIN
  vessel_info
USING
  (mmsi,
    year)
  # Add voyage info
JOIN
  voyages
USING
  (mmsi,
    trip_id)