# Take AIS position data for each voyage, and group by grid
# Add attack data for each grid
# Save as voyages_gridded
sql <- 
  "
SELECT
ais_info.mmsi mmsi,
ais_info.vessel_type vessel_type,
ais_info.lat_bin lat_bin,
ais_info.lon_bin lon_bin,
ais_info.from_anchorage from_anchorage,
ais_info.from_anchorage_id from_anchorage_id,
ais_info.departure_timestamp departure_timestamp,
ais_info.to_anchorage to_anchorage,
ais_info.to_anchorage_id to_anchorage_id,
ais_info.arrival_timestamp arrival_timestamp,
ais_info.distance_km distance_km,
ais_info.through_hotspot through_hotspot,
ais_info.hours hours,
ais_info.fishing fishing,
ais_info.ais_pings ais_pings,
attack_info.grid_id grid_id,
attack_info.grid_has_previous_attacks grid_has_previous_attacks,
attack_info.attacks_last_7_days attacks_last_7_days,
attack_info.attacks_last_14_days attacks_last_14_days,
attack_info.attacks_last_21_days attacks_last_21_days,
attack_info.attacks_last_30_days attacks_last_30_days,
attack_info.attacks_last_60_days attacks_last_60_days, 
attack_info.attacks_last_90_days attacks_last_90_days, 
attack_info.attacks_last_180_days attacks_last_180_days, 
attack_info.attacks_last_365_days attacks_last_365_days, 
attack_info.days_since_attack days_since_attack
FROM (
SELECT
mmsi,
vessel_type,
FLOOR(start_lat/5) * 5 lat_bin,
FLOOR(start_lon/5) * 5 lon_bin,
from_anchorage,
from_anchorage_id,
departure_timestamp,
DATE(departure_timestamp) departure_date,
to_anchorage,
to_anchorage_id,
arrival_timestamp,
SUM(avg_distance_km) distance_km,
SUM(fishing) fishing,
SUM(hours) hours,
SUM(through_hotspot) through_hotspot,
COUNT(*) ais_pings
FROM
[piracy.voyage_ais_positions]
GROUP BY
mmsi,
vessel_type,
lat_bin,
lon_bin,
from_anchorage,
from_anchorage_id,
departure_timestamp,
departure_date,
to_anchorage,
to_anchorage_id,
arrival_timestamp) ais_info
LEFT JOIN (
SELECT
DATE(date) date, 
grid_id, 
grid_has_previous_attacks,
lon_bin, 
lat_bin, 
attacks_last_7_days, 
attacks_last_14_days, 
attacks_last_21_days, 
attacks_last_30_days, 
attacks_last_60_days, 
attacks_last_90_days, 
attacks_last_180_days, 
attacks_last_365_days, 
days_since_attack
FROM
[piracy.piracy_attacks] ) attack_info
ON
ais_info.lat_bin = attack_info.lat_bin
AND ais_info.lon_bin = attack_info.lon_bin
AND ais_info.departure_date = attack_info.date"

bq_table(project = project,table = "all_cargo_voyages_gridded",dataset = "piracy") %>% 
  bq_table_delete()
bq_dataset_query(project,query = sql, destination_table = "voyages_gridded")