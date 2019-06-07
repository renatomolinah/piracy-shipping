# Summarize gridded data into a single row for each voyage
# Emissions info from here: https://www.sciencedirect.com/science/article/pii/S1361920909001072
# Price info scraped from bunkerindex.com
# Calculate fuel consumption at trip level using average speed

sql <-glue::glue(
  "
#standardSQL
CREATE TEMP FUNCTION RADIANS(x FLOAT64) AS (
ACOS(-1) * x / 180
);
CREATE TEMP FUNCTION RADIANS_TO_KM(x FLOAT64) AS (
111.045 * 180 * x / ACOS(-1)
);
CREATE TEMP FUNCTION HAVERSINE(lat1 FLOAT64, long1 FLOAT64,
lat2 FLOAT64, long2 FLOAT64) AS (
RADIANS_TO_KM(
ACOS(COS(RADIANS(lat1)) * COS(RADIANS(lat2)) *
COS(RADIANS(long1) - RADIANS(long2)) +
SIN(RADIANS(lat1)) * SIN(RADIANS(lat2))))
);
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
departure_date,
EXTRACT(year FROM departure_date) year,
from_anchorage_id,
from_anchorage_name,
from_port_name,
arrival_timestamp,
to_anchorage_id,
to_anchorage_name,
to_port_name,
SUM(distance_km) total_distance_km,
{clusters_aggregated},
SUM(hours) total_hours,
(SUM(distance_km) * 0.539957 / SUM(hours)) implied_speed_knots,
SUM(CASE
WHEN NOT(grid_has_previous_attacks IS NULL) THEN distance_km
ELSE 0 END) attack_grid_distance_km,
SUM(CASE
WHEN NOT(grid_has_previous_attacks IS NULL) THEN hours
ELSE 0 END) attack_grid_hours,
(CASE WHEN SUM(grid_has_previous_attacks) IS NULL THEN 0
ELSE SUM(grid_has_previous_attacks)
END) number_attack_grids,
SUM(attacks_last_1_year) attacks_last_1_year,
SUM(attacks_last_2_years) attacks_last_2_years,
SUM(attacks_last_3_years) attacks_last_3_years,
MIN(days_since_attack) days_since_attack,
(CASE WHEN SUM(hours) = 0 THEN AVG(speed_m_s)
ELSE (SUM(speed_m_s * hours) / SUM(hours))
END) speed_m_s,
(CASE WHEN SUM(hours) = 0 THEN AVG(direction_degrees)
ELSE (SUM(direction_degrees * hours) / SUM(hours))
END) direction_degrees
FROM
`piracy.voyages_gridded`
WHERE
from_anchorage_id != to_anchorage_id
AND hours > 0
GROUP BY
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
from_anchorage_id,
from_anchorage_name,
from_port_name,
departure_timestamp,
departure_date,
year,
to_anchorage_id,
to_anchorage_name,
to_port_name,
arrival_timestamp),
fuel_prices AS(
SELECT
*
FROM
`piracy.daily_fuel_prices`
WHERE
fuel_index = 'BIX 380 CST'
),
to_anchorage_info AS(
SELECT
s2id,
iso3 to_country,
lat to_anchorage_lat,
lon to_anchorage_lon
FROM
`world-fishing-827.gfw_research.named_anchorages_v20180803_13b`),
from_anchorage_info AS(
SELECT
s2id,
iso3 from_country,
lat from_anchorage_lat,
lon from_anchorage_lon
FROM
`world-fishing-827.gfw_research.named_anchorages_v20180803_13b`),
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
),
master_2 AS(
SELECT
mmsi,
vessel_type,
flag,
year,
length length_m,
crew,
tonnage tonnage_gt,
engine_power engine_power_kW,
aux_engine_power aux_engine_power_kW,
design_speed design_speed_knots,
main_sfc main_sfc_g_per_kWH,
aux_sfc aux_sfc_g_per_kWH,
from_port_name from_port,
from_country,
to_port_name to_port,
to_country,
DATE(departure_timestamp) departure_date,
DATE(arrival_timestamp) arrival_date,
total_distance_km,
HAVERSINE(from_anchorage_lat,from_anchorage_lon,to_anchorage_lat,to_anchorage_lon) total_haversine_distance_km,
total_hours,
implied_speed_knots,
{clusters_aggregated_2},
attack_grid_distance_km,
attack_grid_hours,
number_attack_grids,
days_since_attack,
attacks_last_1_year,
attacks_last_2_years,
attacks_last_3_years,
speed_m_s wind_speed_m_per_s,
direction_degrees wind_direction_degrees,
price_usd_mt,
total_hours*(0.8 * POW(implied_speed_knots/design_speed, 3))*main_sfc*engine_power/1000000 main_fuel_consumption_mt,
total_hours*0.5*aux_sfc*aux_engine_power/1000000 aux_fuel_consumption_mt,
(total_hours*(0.8 * POW(implied_speed_knots/design_speed, 3))*main_sfc*engine_power/1000000 + total_hours*0.5*aux_sfc*aux_engine_power/1000000) total_fuel_consumption_mt
FROM
master)
SELECT
*,
price_usd_mt * total_fuel_consumption_mt total_fuel_cost_usd,
3.17 * total_fuel_consumption_mt emissions_co2_kg,
87 * main_fuel_consumption_mt + 57 * aux_fuel_consumption_mt emissions_nox_kg,
20 * 3.3 * total_fuel_consumption_mt emissions_sox_kg
FROM
master_2
"
)

bq_table(project = project,table = "voyages",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "voyages",dataset = "piracy"),
                 allowLargeResults = TRUE)

# Create table that defines port-to-port distance cutoff values
# Based on all voyages, then add column for outliers
sql <- "#standardSQL
  WITH outlier_info AS(
  SELECT
    to_port,
    from_port,
    AVG(total_distance_km) mean_distance,
    STDDEV(total_distance_km) sd_distance,
    GREATEST(AVG(total_distance_km) - 2 * STDDEV(total_distance_km),0) min_distance_cutoff_2sd,
    AVG(total_distance_km) + 2 * STDDEV(total_distance_km) max_distance_cutoff_2sd,
    GREATEST(AVG(total_distance_km) - 3 * STDDEV(total_distance_km),0) min_distance_cutoff_3sd,
    AVG(total_distance_km) + 3 * STDDEV(total_distance_km) max_distance_cutoff_3sd
  FROM
    `ucsb-gfw.piracy.voyages`
  GROUP BY
    from_port,
    to_port)
SELECT
  *,
  IF(total_distance_km > min_distance_cutoff_2sd AND total_distance_km < max_distance_cutoff_2sd,FALSE,TRUE) outlier_2sd,
  IF(total_distance_km > min_distance_cutoff_3sd AND total_distance_km < max_distance_cutoff_3sd,FALSE,TRUE) outlier_3sd
FROM
  `ucsb-gfw.piracy.voyages`
LEFT JOIN
  outlier_info USING(to_port,
    from_port)"

bq_table(project = project,table = "voyages_with_outliers",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "voyages_with_outliers",dataset = "piracy"),
                 allowLargeResults = TRUE)