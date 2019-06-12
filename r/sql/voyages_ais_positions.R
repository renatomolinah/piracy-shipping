# For every voyage, get all AIS positions along the way 
# save as cargo_AIS_positions_with_ports
# Fuel consumption based on Juan's high seas work
# The following engine parameters from https://www.sciencedirect.com/science/article/pii/S1361920911001337
# Use main load factor of 0.8
# Aux load factor of 0.5
# Fuel consumption equation from that paper
# For posterity, this carries old fuel consumption - but we will recalculate this at the voyage level in the last query

cluster_filters <- paste0(cluster_filters$cluster_filter,collapse = ", ")
sql<-glue::glue("
#standardSQL
  WITH 
  vessel_info AS(
  SELECT
  mmsi,
  year,
  design_speed,
  main_sfc,
  aux_sfc,
  engine_power,
  aux_engine_power
  FROM
  `piracy.vessel_info`),
  ping_info AS (
  SELECT
    CAST(ssvid AS INT64) mmsi,
    lat,
    lon,
    timestamp,
    hours,
    implied_speed_knots,
    heading,
    avg_distance_m/1000 avg_distance_km
  FROM
    `world-fishing-827.gfw_research.pipe_production_b`
  WHERE
    lat < 90
    AND lat > -90
    AND lon < 180
    AND lon >-180
    AND _PARTITIONTIME BETWEEN TIMESTAMP('2012-01-01')
    AND TIMESTAMP('2017-12-31')
    AND CAST(ssvid AS INT64) IN (
    SELECT
      mmsi
    FROM
      `piracy.vessel_info`)),
   voyage_info AS(
  SELECT
    mmsi,
    year,
    departure_timestamp,
    arrival_timestamp,
    trip_id
  FROM
    `ucsb-gfw.piracy.voyages_with_anchorages`),
  ais_info AS(
  SELECT
    voyage_info.mmsi mmsi,
    ping_info.lat lat,
    ping_info.lon lon,
    ping_info.timestamp timestamp,
    ping_info.hours hours,
    ping_info.implied_speed_knots implied_speed,
    ping_info.heading heading,
    ping_info.avg_distance_km avg_distance_km,
    voyage_info.year year,
    voyage_info.trip_id trip_id
  FROM
    ping_info
  JOIN
    voyage_info
  ON
    ping_info.mmsi = voyage_info.mmsi
    AND ping_info.timestamp > voyage_info.departure_timestamp
    AND ping_info.timestamp < voyage_info.arrival_timestamp),
master AS(
SELECT
*
FROM
ais_info
LEFT JOIN
vessel_info
USING(mmsi,year))
SELECT
  mmsi,
  year,
  trip_id,
  lat,
  lon,
  timestamp,
  avg_distance_km,
  hours,
  heading,
  hours*(0.8 * POW(implied_speed/design_speed, 3))*main_sfc*engine_power/1000000 main_fuel_consumption_mt_inst,
  hours*0.5*aux_sfc*aux_engine_power/1000000 aux_fuel_consumption_mt_inst,
                {cluster_filters}
FROM
  master
"
)

bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)
           