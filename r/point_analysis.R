library(tidyverse)
library(bigrquery)

# Set up bigquery
project <-  "ucsb-gfw"

sql <-
  "
#standardSQL
  WITH ping_info AS (
  SELECT
    lat,
    lon,
    start_timestamp timestamp,
    hours,
    avg_distance_km,
    CONCAT(mmsi,'-',departure_timestamp) voyage_id
  FROM
    `ucsb-gfw.piracy.voyages_ais_positions`)
SELECT
  EXTRACT(date
  FROM
    timestamp) date,
  FLOOR(lat / 0.1) * 0.1 + 0.05 lat_bin,
  FLOOR(lon / 0.1) * 0.1 + 0.05 lon_bin,
  SUM(hours) hours,
  SUM(avg_distance_km) distance_km,
  STRING_AGG(DISTINCT voyage_id) voyage_id_array
FROM
  ping_info
GROUP BY
  date,
  lat_bin,
  lon_bin"

bq_table(project = project,table = "point_analysis_gridded_shipping_hours",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_analysis_gridded_shipping_hours",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql<-"#standardSQL
WITH
  filtered_attacks AS(
  SELECT
    reference attack_reference,
    attack_eez,
    date date_attack,
    ST_GEOGFROMTEXT(point) attack_geog
  FROM
    `ucsb-gfw.piracy.asam`
  WHERE
    date >= DATE(TIMESTAMP('2012-06-01'))),
  distinct_grids AS(
  SELECT
    DISTINCT lon_bin,
    lat_bin
  FROM
    `ucsb-gfw.piracy.point_analysis_gridded_shipping_hours`),
  joined_table AS(
  SELECT
    *
  FROM
    filtered_attacks
  CROSS JOIN (
    SELECT
      *,
      ST_GeogPoint(lon_bin,
        lat_bin) shipping_geog
    FROM
      distinct_grids)),
  final AS(
  SELECT
    attack_reference,
    lon_bin,
    lat_bin,
    ST_Distance(attack_geog,
      shipping_geog) distance_to_attack_m
  FROM
    joined_table)
SELECT
  *
FROM
  final
WHERE
  distance_to_attack_m <= 1e6"

bq_table(project = project,table = "point_distance_lookup",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_distance_lookup",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)


sql<- "#standardSQL
  WITH filtered_attacks AS(
  SELECT
    reference attack_reference,
    attack_eez,
    date date_attack
  FROM
    `ucsb-gfw.piracy.asam`
  WHERE
    date >= DATE(TIMESTAMP('2012-06-01'))),
  distance_lookup AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.point_distance_lookup`
  LEFT JOIN
    filtered_attacks USING (attack_reference)),
  gridded_shipping_hours AS(
  SELECT
    date date_shipping,
    hours shipping_hours,
    distance_km shipping_distance_traveled_km,
    lat_bin,
    lon_bin
  FROM
    `ucsb-gfw.piracy.point_analysis_gridded_shipping_hours` ),
  final AS(
  SELECT
    attack_reference,
    attack_eez,
    date_attack,
    date_shipping,
    DATE_DIFF(date_shipping,
      date_attack,
      DAY) days_since_attack,
          lat_bin shipping_lat_bin,
    lon_bin shipping_lon_bin,
    distance_to_attack_m/1000 distance_to_attack_km,
    shipping_distance_traveled_km,
    shipping_hours
  FROM
    gridded_shipping_hours
  JOIN
    distance_lookup USING(lon_bin,
      lat_bin))
SELECT
  *
FROM
  final
WHERE
  days_since_attack <= 365
  AND days_since_attack >= -365"

bq_table(project = project,table = "point_analysis_full",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_analysis_full",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql<-"#standardSQL
SELECT
  attack_reference,
  attack_eez,
  date_attack,
  (CASE
      WHEN days_since_attack = 0 THEN -30
      WHEN days_since_attack <0 THEN FLOOR(days_since_attack/30)*30
      ELSE CEILING(days_since_attack/30)*30 END) days_since_attack_bin,
  (CASE
      WHEN distance_to_attack_km = 0 THEN 50
      ELSE CEILING(distance_to_attack_km/50)*50 END) distance_to_attack_km_bin,
  SUM(shipping_distance_traveled_km) shipping_distance_traveled_km,
  SUM(shipping_hours) shipping_hours,
  ARRAY_CONCAT_AGG(DISTINCT voyage_id_array) voyage_id_array
FROM
  `ucsb-gfw.piracy.point_analysis_full`
GROUP BY
  attack_reference,
  attack_eez,
  date_attack,
  days_since_attack_bin,
  distance_to_attack_km_bin"

bq_table(project = project,table = "point_analysis_summary",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_analysis_summary",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

# Pull in data we want
sql<-"
SELECT * 
FROM `piracy.point_analysis_summary` 
WHERE 
days_since_attack_bin > -390 
AND days_since_attack_bin < 390 
AND distance_to_attack_km_bin <= 500
"

point_analysis_summary <- bq_project_query(project, sql) %>%
  bq_table_download(max_results = Inf)
write_csv(point_analysis_summary,path="point_analysis_summary.csv")

