#standardSQL
WITH
  # Get relevant vessel info
  vessel_info_base AS(
  SELECT
    CAST(ssvid AS INT64) mmsi,
     # The "best" vessel class accounts for AIS info, official registries, and the GFW vessel classification algorithm
    best.best_vessel_class best_vessel_type,
    # Our main binary will flag vessels that have 'best' vessel class of one of the following
    IF(best.best_vessel_class IN('cargo',
        'cargo_or_tanker',
        'bunker_or_tanker',
        'tanker',
        'cargo_or_reefer',
        'specialized_reefer',
        'container_reefer',
        'reefer',
        'bunker'),TRUE,FALSE) best_vessel_type_cargo,
    # We can also get the vessel class just from official registries, where avaiable
    # This column is an array, since sometimes vessels are in multiple registries which may each have be for different vessel classes
    # We will unnest in below, so that we can determine whether each item in the array is a cargo vessel
    registry_info.best_known_vessel_class registry_vessel_type_array,
  # We also we turn that array into a string, and separate the different classes with a ';''
ARRAY_TO_STRING(registry_info.best_known_vessel_class,";") registry_vessel_type,
# We also make a binary for whether or not *any* of the registry vessel classes are ever one of our cargo types of interest
IF
(REGEXP_CONTAINS(ARRAY_TO_STRING(registry_info.best_known_vessel_class,";"),'cargo|cargo_or_tanker|bunker_or_tanker|tanker|cargo_or_reefer|specialized_reefer|container_reefer|reefer|bunker' ), TRUE, FALSE) registry_vessel_type_any_cargo,
    # do the neural net and vessel registries disagree about the vessel class?
    best.registry_net_disagreement registry_neural_net_disagreement,
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
  # Unnest registry info, so that each row represents a specific mmsi, and whether the registry is cargo or not
  vessel_info_registry_unnested AS(
    SELECT
    mmsi,
    IF(registry_vessel_type_string IN('cargo',
        'cargo_or_tanker',
        'bunker_or_tanker',
        'tanker',
        'cargo_or_reefer',
        'specialized_reefer',
        'container_reefer',
        'reefer',
        'bunker'),1,0) registry_is_cargo
    FROM
    vessel_info_base
    CROSS JOIN
    UNNEST(registry_vessel_type_array) AS registry_vessel_type_string
  ),
  # Now, for each vessel, summarize the number of registry entries, and number of registry entries that are cargo
  # And determine whether or not all registry entries are always cargo
  vessel_info_registry_by_mmsi AS(
    SELECT
    mmsi,
    IF(COUNT(*) = SUM(registry_is_cargo),
    TRUE,FALSE) registry_vessel_type_always_cargo
    FROM
    vessel_info_registry_unnested
    GROUP BY
    mmsi
  )
SELECT
  * EXCEPT(registry_vessel_type_array,registry_vessel_type_always_cargo),
  # If registry_vessel_type_always_cargo is missing, it is FALSE
  IFNULL(registry_vessel_type_always_cargo,FALSE) registry_vessel_type_always_cargo
FROM
  vessel_info_base
LEFT JOIN
  vessel_info_registry_by_mmsi
USING(mmsi)
# Only include vessels that match one of our class criteria
WHERE(best_vessel_type_cargo OR registry_vessel_type_any_cargo OR registry_vessel_type_always_cargo)