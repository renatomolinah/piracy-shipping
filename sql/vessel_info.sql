#standardSQL
WITH
  # Get relevant vessel info
  vessel_info_base AS(
  SELECT
    CAST(ssvid AS INT64) mmsi,
    best.best_vessel_class best_vessel_type,
    ARRAY_TO_STRING(registry_info.best_known_vessel_class,";") registry_vessel_type,
    best.best_flag flag,
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
    `world-fishing-827.gfw_research.vi_ssvid_v20220101`
    # Ensure it's a reliable vessel that is not offetting or broadcasting multiple overlapping names
  WHERE
    NOT activity.offsetting
    AND activity.overlap_hours_multinames = 0),
  # Load time-varying version of vessel database
  # So we make the most restrictive sample, where we look for vessels that are consistently registered as cargo in the time-varying vessel info tables based on official registries
  vessel_info_base_byyear AS(
  SELECT
    CAST(ssvid AS INT64) mmsi,
    year,
  IF
    (REGEXP_CONTAINS(ARRAY_TO_STRING(registry_info.best_known_vessel_class,";"),'cargo|cargo_or_tanker|bunker_or_tanker|tanker|cargo_or_reefer|specialized_reefer|container_reefer|reefer|bunker' ), 1, 0) vessel_type_is_cargo_registry
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_byyear_v20220101`
    # Ensure it's a reliable vessel that is not offetting or broadcasting multiple overlapping names
  WHERE
    NOT activity.offsetting
    AND activity.overlap_hours_multinames = 0
    AND year >= 2013
    AND year <= 2021),
  # Now for each mmsi, summarize number of years in database, and number of years registered as cargo in database
  vessel_info_byyear_summary_by_mmsi AS(
  SELECT
    mmsi,
    COUNT(*) number_years,
    SUM(vessel_type_is_cargo_registry) number_years_registered_as_cargo
  FROM
    vessel_info_base_byyear
  GROUP BY
    mmsi),
  # Finally, determine which mmsi are registered as cargo acrosss all years
  vessel_consistently_cargo_across_years AS(
  SELECT
    mmsi
  FROM
    vessel_info_byyear_summary_by_mmsi
  WHERE
    number_years = number_years_registered_as_cargo ),
  vessel_info_with_cargo_binaries AS(
    # To that table, add on binaries for whether or not the best_vessel_type is cargo,
    # and whether or not the registry_vessel_type is cargo
    # and whether or not the vessel is registered as cargo in the time-varying version of the vessel database
  SELECT
    mmsi,
  IF
    ((best_vessel_type) IN('cargo',
        'cargo_or_tanker',
        'bunker_or_tanker',
        'tanker',
        'cargo_or_reefer',
        'specialized_reefer',
        'container_reefer',
        'reefer',
        'bunker'), TRUE, FALSE) vessel_type_is_cargo_best,
  IF
    (REGEXP_CONTAINS(registry_vessel_type,'cargo|cargo_or_tanker|bunker_or_tanker|tanker|cargo_or_reefer|specialized_reefer|container_reefer|reefer|bunker' ), TRUE, FALSE) vessel_type_is_cargo_registry,
  IF
    (mmsi IN (
      SELECT
        mmsi
      FROM
        vessel_consistently_cargo_across_years),TRUE,FALSE) vessel_type_is_cargo_registry_across_years
  FROM
    vessel_info_base)
SELECT
  *
FROM
  vessel_info_base
LEFT JOIN
  vessel_info_with_cargo_binaries
USING
  (mmsi)
  # Only keep mmsi that are either GFW-classified as cargo, or registry-listed as cargo, or registry-listed as cargo across all years
WHERE
  (vessel_type_is_cargo_best
    OR vessel_type_is_cargo_registry
    OR vessel_type_is_cargo_registry_across_years)