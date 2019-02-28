# For every voyage, get all AIS positions along the way 
# save as cargo_AIS_positions_with_ports
# Fuel consumption based on Juan's high seas work
# The following engine parameters from https://www.sciencedirect.com/science/article/pii/S1361920911001337
# Use main load factor of 0.8
# Aux load factor of 0.5
# Fuel consumption equation from that paper
# For posterity, this carries old fuel consumption - but we will recalculate this at the voyage level in the last query
sql<-glue::glue("
#standardSQL
  WITH ping_info AS (
                SELECT
                mmsi,
                lat start_lat,
                lon start_lon,
                timestamp start_timestamp,
                next_lat end_lat,
                next_lon end_lon,
                next_timestamp end_timestamp,
                hours,
                implied_speed,
                avg_distance_km
                FROM
                `world-fishing-827.gfw_research.pipeline_p_p550_daily`
                WHERE
                lat < 90
                AND lat > -90
                AND lon < 180
                AND lon >-180
                AND _PARTITIONTIME BETWEEN TIMESTAMP('2012-01-01')
                AND TIMESTAMP('2017-12-31')
                AND mmsi IN (
                SELECT
                mmsi
                FROM
                `piracy.voyages_with_anchorages`)),
                voyage_info AS(
                SELECT
                mmsi,
                flag,
                EXTRACT(YEAR
                FROM
                departure_timestamp) AS year,
                from_anchorage_id,
                from_anchorage_name,
                from_port_name,
                departure_timestamp,
                to_anchorage_id,
                to_anchorage_name,
                to_port_name,
                arrival_timestamp,
                vessel_type,
                length,
                engine_power,
                crew,
                tonnage,
                aux_engine_power,
                design_speed,
                main_sfc,
                aux_sfc
                FROM
                `ucsb-gfw.piracy.voyages_with_anchorages`),
                ais_info AS(
                SELECT
                ping_info.mmsi mmsi,
                ping_info.start_lat start_lat,
                ping_info.start_lon start_lon,
                ping_info.start_timestamp start_timestamp,
                ping_info.end_timestamp end_timestamp,
                ping_info.hours hours,
                ping_info.implied_speed implied_speed,
                ping_info.avg_distance_km avg_distance_km,
                voyage_info.flag flag,
                voyage_info.year year,
                voyage_info.from_anchorage_id from_anchorage_id,
                voyage_info.from_anchorage_name from_anchorage_name,
                voyage_info.from_port_name from_port_name,
                voyage_info.departure_timestamp departure_timestamp,
                voyage_info.to_anchorage_id to_anchorage_id,
                voyage_info.to_anchorage_name to_anchorage_name,
                voyage_info.to_port_name to_port_name,
                voyage_info.arrival_timestamp arrival_timestamp,
                voyage_info.vessel_type vessel_type,
                voyage_info.length length,
                voyage_info.engine_power engine_power,
                voyage_info.crew crew,
                voyage_info.tonnage tonnage,
                voyage_info.aux_engine_power aux_engine_power,
                voyage_info.design_speed design_speed,
                voyage_info.main_sfc main_sfc,
                voyage_info.aux_sfc aux_sfc
                FROM
                ping_info
                JOIN
                voyage_info
                ON
                ping_info.mmsi = voyage_info.mmsi
                AND ping_info.start_timestamp > voyage_info.departure_timestamp
                AND ping_info.start_timestamp < voyage_info.arrival_timestamp)
SELECT
*,
{cluster_filters}
FROM
ais_info
"
)

bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "voyage_ais_positions",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)
           