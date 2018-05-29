# Summarize gridded data into a single row for each voyage
# Emissions info from here: https://www.sciencedirect.com/science/article/pii/S1361920909001072
# Price info scraped from bunkerindex.com
sql <-
  "
WITH
gridded_info AS(
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
departure_timestamp,
DATE(departure_timestamp) departure_date,
from_anchorage_id,
from_anchorage_name,
from_port_name,
arrival_timestamp,
to_anchorage_id,
to_anchorage_name,
to_port_name,
SUM(distance_km) total_distance_km,
(CASE through_hotspot > 0
THEN 1
ELSE 0
END) through_hotspot,
SUM(hours) total_hours,
SUM(total_fuel_consumption_mt) total_fuel_consumption_mt,
SUM(hours)*0.5*aux_sfc*aux_engine_power/1000000 aux_fuel_consumption_mt,
(SUM(total_fuel_consumption_mt) - SUM(hours)*0.5*aux_sfc*aux_engine_power/1000000) main_fuel_consumption_mt,
SUM(CASE
WHEN NOT(grid_has_previous_attacks IS NULL) THEN distance_km
ELSE 0 END) attack_grid_distance_km,
SUM(CASE
WHEN NOT(grid_has_previous_attacks IS NULL) THEN hours
ELSE 0 END) attack_grid_hours,
SUM(grid_has_previous_attacks) number_attack_grids,
SUM(attacks_last_7_days) attacks_last_7_days,
SUM(attacks_last_14_days) attacks_last_14_days,
SUM(attacks_last_21_days) attacks_last_21_days,
SUM(attacks_last_30_days) attacks_last_30_days,
SUM(attacks_last_60_days) attacks_last_60_days,
SUM(attacks_last_90_days) attacks_last_90_days,
SUM(attacks_last_120_days) attacks_last_120_days,
SUM(attacks_last_150_days) attacks_last_150_days,
SUM(attacks_last_180_days) attacks_last_180_days,
SUM(attacks_last_210_days) attacks_last_210_days,
SUM(attacks_last_240_days) attacks_last_240_days,
SUM(attacks_last_270_days) attacks_last_270_days,
SUM(attacks_last_300_days) attacks_last_300_days,
SUM(attacks_last_330_days) attacks_last_330_days,
SUM(attacks_last_365_days) attacks_last_365_days,
MIN(days_since_attack) days_since_attack
(SUM(speed_m_s * hours) / SUM(hours)) speed_m_s,
(SUM(direction_degrees * hours) / SUM(hours)) direction_degrees
FROM
[ucsb-gfw:piracy.voyages_gridded]
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
from_anchorage_id,
from_anchorage_name,
from_port_name,
departure_timestamp,
to_anchorage_id,
to_anchorage_name,
to_port_name,
arrival_timestamp
WHERE
AND from_anchorage != to_anchorage),
fuel_prices AS(
SELECT
*
FROM
[ucsb-gfw:fuel_analysis.daily_fuel_price]
WHERE
fuel_index = 'BIX 380 CST'
),
to_anchorage_info AS(
SELECT
s2id,
lat to_anchorage_lat,
lon to_anchorage_lon
FROM
[world-fishing-827:gfw_research.named_anchorages_20171120]),
from_anchorage_info AS(
SELECT
s2id,
lat from_anchorage_lat,
lon from_anchorage_lon
FROM
[world-fishing-827:gfw_research.named_anchorages_20171120]),
master AS(
SELECT
*
FROM
gridded_info
LEFT JOIN
fuel_prices
ON
gridded_info.departure_date = fuel_prices.date
LEFT JOIN
from_anchorage_info
ON
gridded_info.from_anchorage_id = from_anchorage_info.s2id
LEFT JOIN
to_anchorage_info
ON
gridded_info.to_anchorage_id = to_anchorage_info.s2id
)
SELECT
mmsi,
vessel_type,
flag,
year,
length,
engine_power,
crew,
tonnage,
aux_engine_power,
design_speed,
main_sfc,
aux_sfc,
from_anchorage_id from_anchorage,
from_anchorage_name from_anchorage_group
from_port_name from_port,
NTH(2, SPLIT(from_anchorage_name, ',')) from_country,
to_anchorage_id to_anchorage,
to_anchorage_name to_anchorage_group
to_port_name to_port,
NTH(2, SPLIT(to_anchorage_name, ',')) to_country,
DATE(departure_timestamp) departure_date,
DATE(arrival_timestamp) arrival_date,
total_distance_km,
6371*ACOS(COS(RADIANS(to_anchorage_lat))*COS(RADIANS(from_anchorage_lat))*COS(RADIANS(from_anchorage_lon)-RADIANS(to_anchorage_lon))+SIN(RADIANS(to_anchorage_lat))*SIN(RADIANS(from_anchorage_lat))) total_haversine_distance_km,
total_hours,
attack_grid_distance_km,
attack_grid_hours,
number_attack_grids,
through_hotspot,
days_since_attack,
attacks_last_7_days,
attacks_last_14_days,
attacks_last_21_days,
attacks_last_30_days,
attacks_last_60_days,
attacks_last_90_days,
attacks_last_120_days,
attacks_last_150_days,
attacks_last_180_days,
attacks_last_210_days,
attacks_last_240_days,
attacks_last_270_days,
attacks_last_300_days,
attacks_last_330_days,
attacks_last_365_days,
speed_m_s wind_speed_m_s,
direction_degrees wind_direction_degrees,
main_fuel_consumption_mt,
aux_fuel_consumption_mt,
total_fuel_consumption_mt,
price_usd_mt * total_fuel_consumption_mt total_fuel_cost_usd
3.17 * total_fuel_consumption_mt emissions_co2_kg
87 * main_fuel_consumption_mt + 57 * aux_fuel_consumption_mt emissions_nox_kg
20 * 3.3 * total_fuel_consumption_mt emissions_sox_kg
FROM
master
WHERE
total_distance_km > 0.8 * total_haversine_distance_km
"

bq_table(project = project,table = "voyages",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project = project,query = sql,destination_table = bq_table(project = project,table = "voyages",dataset = "piracy"),
                 allowLargeResults = TRUE)

bq_table(bq_perform_query(project = project,table = "voyages",dataset = "piracy")) %>%
  bq_table_save(destination_uris = "gs:://ucsb-gfw/piracy/voyages.csv")