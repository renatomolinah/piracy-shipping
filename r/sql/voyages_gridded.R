# Take AIS position data for each voyage, and group by grid
# Add attack data for each grid
# Add wind data for each grid
# Save as voyages_gridded

sql <-
  "
WITH
ais_info AS(
SELECT
mmsi,
vessel_type,
flag,
length,
engine_power,
crew,
tonnage,
aux_engine_power,
design_speed,
main_sfc,
aux_sfc,
FLOOR(start_lat/5) * 5 lat_bin,
FLOOR(start_lon/5) * 5 lon_bin,
from_anchorage_id,
from_anchorage_name,
from_port_name,
departure_timestamp,
DATE(departure_timestamp) departure_date,
to_anchorage_id,
to_anchorage_name,
to_port_name,
arrival_timestamp,
SUM(avg_distance_km) distance_km,
SUM(through_hotspot) through_hotspot,
SUM(total_fuel_consumption_mt) total_fuel_consumption_mt
FROM
[piracy.voyage_ais_positions]
GROUP BY
mmsi,
vessel_type,
length,
engine_power,
crew,
tonnage,
aux_engine_power,
design_speed,
main_sfc,
aux_sfc,
lat_bin,
lon_bin,
from_anchorage_id,
from_anchorage_name,
from_port_name,
departure_timestamp,
departure_date,
to_anchorage_id,
to_anchorage_name,
to_port_name,
arrival_timestamp),
ais_info_wind AS(
SELECT
*
FROM
ais_info
LEFT JOIN
wind
ON
ais_info.departure_date = wind.date
ais_info.lat_bin = wind.lat_bin
ais_info.lon_bin = wind.lon_bin),
ais_info_attacks AS(
SELECT
*
FROM
ais_info_wind
LEFT JOIN
piracy_attacks
ON
ais_info.departure_date = piracy_attacks.date
ais_info.lat_bin = piracy_attacks.lat_bin
ais_info.lon_bin = piracy_attacks.lon_bin
)
SELECT
*
FROM
ais_info_attacks
"
bq_table(project = project,table = "voyages_gridded",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "voyages_gridded",dataset = "piracy"),
                 allowLargeResults = TRUE)