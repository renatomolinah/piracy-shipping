# Summarize gridded data into a single row for each voyage
"
SELECT
*
FROM(
SELECT
a.mmsi mmsi,
a.vessel_type vessel_type,
a.from_anchorage from_anchorage,
a.from_anchorage_id from_anchorage_id,
NTH(2, SPLIT(a.from_anchorage, ',')) from_country,
DATE(a.departure_timestamp) departure_date,
a.to_anchorage to_anchorage,
a.to_anchorage_id to_anchorage_id,
NTH(2, SPLIT(a.to_anchorage, ',')) to_country,
DATE(a.arrival_timestamp) arrival_date,
a.total_distance_km total_distance_km,
6371*ACOS(COS(RADIANS(anchorage_info_to.to_anchorage_lat))*COS(RADIANS(anchorage_info_from.from_anchorage_lat))*COS(RADIANS(anchorage_info_from.from_anchorage_lon)-RADIANS(anchorage_info_to.to_anchorage_lon))+SIN(RADIANS(anchorage_info_to.to_anchorage_lat))*SIN(RADIANS(anchorage_info_from.from_anchorage_lat))) total_haversine_distance_km,
a.total_hours total_hours,
a.through_hotspot through_hotspot,
a.total_fuel_consumption__low_bound total_fuel_consumption__low_bound,
a.total_fuel_consumption__high_bound total_fuel_consumption__high_bound,
a.attack_grid_distance_km attack_grid_distance_km,
a.attack_grid_hours attack_grid_hours,
a.number_attack_grids number_attack_grids,
a.attacks_last_7_days attacks_last_7_days,
a.attacks_last_14_days attacks_last_14_days,
a.attacks_last_21_days attacks_last_21_days,
a.attacks_last_30_days attacks_last_30_days,
a.attacks_last_60_days attacks_last_60_days,
a.attacks_last_90_days attacks_last_90_days,
a.attacks_last_180_days attacks_last_180_days,
a.attacks_last_365_days attacks_last_365_days,
a.days_since_attack days_since_attack,
a.speed_m_s wind_speed_m_s,
a.direction_degrees wind_direction_degrees,
FROM (
SELECT
mmsi,
vessel_type,
from_anchorage,
from_anchorage_id,
departure_timestamp,
to_anchorage,
to_anchorage_id,
arrival_timestamp,
SUM(distance_km) total_distance_km,
(CASE through_hotspot > 0
THEN 1
ELSE 0
END) through_hotspot,
SUM(hours) total_hours,
SUM(fishing) total_fishing,
SUM(total_fuel_consumption__low_bound) total_fuel_consumption__low_bound,
SUM(total_fuel_consumption__high_bound) total_fuel_consumption__high_bound,
SUM(CASE
WHEN NOT(grid_has_previous_attacks IS NULL) THEN distance_km
ELSE NULL END) attack_grid_distance_km,
SUM(CASE
WHEN NOT(grid_has_previous_attacks IS NULL) THEN hours
ELSE NULL END) attack_grid_hours,
SUM(CASE
SUM(grid_has_previous_attacks) number_attack_grids,
SUM(attacks_last_7_days) attacks_last_7_days,
SUM(attacks_last_14_days) attacks_last_14_days,
SUM(attacks_last_21_days) attacks_last_21_days,
SUM(attacks_last_30_days) attacks_last_30_days,
SUM(attacks_last_60_days) attacks_last_60_days,
SUM(attacks_last_90_days) attacks_last_90_days,
SUM(attacks_last_180_days) attacks_last_180_days,
SUM(attacks_last_365_days) attacks_last_365_days,
MIN(days_since_attack) days_since_attack
(SUM(speed_m_s * hours) / SUM(hours)) speed_m_s,
(SUM(direction_degrees * hours) / SUM(hours)) direction_degrees
FROM
[ucsb-gfw:piracy.voyages_gridded]
GROUP BY
mmsi,
vessel_type,
from_anchorage,
from_anchorage_id,
departure_timestamp,
to_anchorage,
to_anchorage_id,
arrival_timestamp
WHERE
total_fishing = 0
AND from_anchorage != to_anchorage) a
LEFT JOIN (
SELECT
s2id,
lat from_anchorage_lat,
lon from_anchorage_lon
FROM
[world-fishing-827:gfw_research.named_anchorages_20171120] ) anchorage_info_from
ON
a.from_anchorage_id = anchorage_info_from.s2id
LEFT JOIN (
SELECT
s2id,
lat to_anchorage_lat,
lon to_anchorage_lon
FROM
[world-fishing-827:gfw_research.named_anchorages_20171120] ) anchorage_info_to
ON
a.to_anchorage_id = anchorage_info_to.s2id)
WHERE
total_distance_km > 0.8 * total_haversine_distance_km"

bq_table(project = project,table = "all_cargo_voyages",dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = "filtered_cargo_voyages",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project = project,query = sql,destination_table = bq_table(project = project,table = "voyages",dataset = "piracy"),
                 allowLargeResults = TRUE)

bq_table(bq_perform_query(project = project,table = "voyages",dataset = "piracy")) %>%
  bq_table_save(destination_uris = "gs:://ucsb-gfw/piracy/voyages.csv")