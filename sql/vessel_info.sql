#standardSQL
WITH
  # Get relevant vessel info for bunkers, reefer, cargo, and tankers
  vessel_info AS(
  SELECT
    CAST(ssvid AS INT64) mmsi,
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
    3.39*POW(10,-4)*best.best_engine_power_kw+2.151*POW(10,-5)*best.best_tonnage_gt-2.742*POW(10,-9)*best.best_engine_power_kw*best.best_tonnage_gt+12.93 design_speed
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_v20231201`
  WHERE
    best.best_vessel_class IN('cargo',
      'cargo_or_tanker',
      'bunker_or_tanker',
      'tanker',
      'cargo_or_reefer',
      'specialized_reefer',
      'container_reefer',
      'reefer',
      'bunker')
    # Ensure it's a reasonable vessel
    AND NOT activity.offsetting
    AND activity.overlap_hours_multinames = 0)
SELECT
*
FROM
vessel_info