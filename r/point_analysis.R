library(tidyverse)
library(bigrquery)

# Set up bigquery
project <-  "ucsb-gfw"

sql <-
  "
#standardSQL
  WITH ping_info AS (
  SELECT
    start_lat lat,
    start_lon lon,
    start_timestamp timestamp,
    hours,
    avg_distance_km,
    CONCAT(CAST(mmsi AS string),'-',CAST(departure_timestamp AS string)) voyage_id
  FROM
    `ucsb-gfw.piracy.voyage_ais_positions`)
SELECT
  EXTRACT(date
  FROM
    timestamp) date,
  FLOOR(lat / 0.1) * 0.1 + 0.05 lat_bin,
  FLOOR(lon / 0.1) * 0.1 + 0.05 lon_bin,
  SUM(hours) hours,
  SUM(avg_distance_km) distance_km,
  ARRAY_AGG(DISTINCT voyage_id) voyage_id_array
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
    date >= DATE(TIMESTAMP('2011-01-01'))),
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
    date_attack,
    attack_eez,
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

sql<-"#standardSQL
  WITH attack_info_1 AS(
  SELECT
    reference attack_reference_1,
    attack_eez,
    date date_attack_1,
    ST_GEOGFROMTEXT(point) attack_geog_1
  FROM
    `ucsb-gfw.piracy.asam`),
  attack_info_2 AS(
  SELECT
    reference attack_reference_2,
    date date_attack_2,
    ST_GEOGFROMTEXT(point) attack_geog_2
  FROM
    `ucsb-gfw.piracy.asam`),
  joined_data AS(
  SELECT
    *
  FROM
    attack_info_1
  CROSS JOIN
    attack_info_2),
  processed AS(
  SELECT
    attack_reference_1 attack_reference,
    attack_reference_2,
    attack_eez,
    date_attack_1 date_attack,
    ST_Distance(attack_geog_1,
      attack_geog_2)/1000 distance_between_attacks_km,
    DATE_DIFF(date_attack_1,
      date_attack_2,
      DAY) days_between_attacks
  FROM
    joined_data)
SELECT
  *
FROM
  processed"

bq_table(project = project,table = "point_attacks_crossed",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_attacks_crossed",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql<- "#standardSQL
  WITH distance_lookup AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.point_distance_lookup`),
  gridded_shipping_hours AS(
  SELECT
    date date_shipping,
    hours shipping_hours,
    distance_km shipping_distance_traveled_km,
    lat_bin,
    lon_bin,
    voyage_id_array
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
    shipping_hours,
    voyage_id_array
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
#https://stackoverflow.com/questions/52485871/distinct-count-across-bigquery-arrays

sql<-"#standardSQL
  CREATE TEMP FUNCTION DistinctCOUNT(arr ANY TYPE) AS ( (
    SELECT
      COUNT(DISTINCT x)
    FROM
      UNNEST(arr) AS x) );
  SELECT
    attack_reference,
    attack_eez,
    date_attack,
    (CASE
        WHEN days_since_attack = 0 THEN 30
        WHEN days_since_attack <0 THEN FLOOR(days_since_attack/30)*30
        ELSE CEILING(days_since_attack/30)*30 END) days_since_attack_bin,
    (CASE
        WHEN distance_to_attack_km = 0 THEN 50
        ELSE CEILING(distance_to_attack_km/50)*50 END) distance_to_attack_km_bin,
    SUM(shipping_distance_traveled_km) shipping_distance_traveled_km,
    SUM(shipping_hours) shipping_hours,
    DistinctCOUNT(ARRAY_CONCAT_AGG(voyage_id_array)) unique_number_voyages
  FROM
    `ucsb-gfw.piracy.point_analysis_full`
  GROUP BY
    attack_reference,
    attack_eez,
    date_attack,
    days_since_attack_bin,
    distance_to_attack_km_bin"

bq_table(project = project,table = "point_summary_shipping",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_summary_shipping",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql<-"#standardSQL
SELECT
    attack_reference,
    attack_eez,
    date_attack,
    COUNT(*) number_attacks,
    (CASE
        WHEN days_between_attacks = 0 THEN 30
        WHEN days_between_attacks <0 THEN FLOOR(days_between_attacks/30)*30
        ELSE CEILING(days_between_attacks/30)*30 END) days_since_attack_bin,
    (CASE
        WHEN distance_between_attacks_km = 0 THEN 50
        ELSE CEILING(distance_between_attacks_km/50)*50 END) distance_to_attack_km_bin
  FROM
    `ucsb-gfw.piracy.point_attacks_crossed`
  GROUP BY
    attack_reference,
    attack_eez,
    date_attack,
    days_since_attack_bin,
    distance_to_attack_km_bin"

bq_table(project = project,table = "point_summary_attacks",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_summary_attacks",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql<-"#standardSQL
WITH
  shipping_info AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.point_summary_shipping`),
  attack_info AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.point_summary_attacks`)
SELECT
  * EXCEPT (shipping_distance_traveled_km,
    shipping_hours,
    unique_number_voyages,
    number_attacks),
  IFNULL(shipping_distance_traveled_km, 0) shipping_distance_traveled_km,
  IFNULL(shipping_hours, 0) shipping_hours,
  IFNULL(unique_number_voyages, 0) unique_number_voyages,
  IFNULL(number_attacks, 0) number_attacks
FROM
  shipping_info FULL OUTER
JOIN
  attack_info USING(attack_reference,
    days_since_attack_bin,
    distance_to_attack_km_bin,
    attack_eez,
    date_attack)"

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
AND date_attack >= DATE(TIMESTAMP('2013-01-01'))
AND date_attack < DATE(TIMESTAMP('2018-01-01'))
"

point_analysis_summary <- bq_project_query(project, sql) %>%
  bq_table_download(max_results = Inf)

write_csv(point_analysis_summary,path="point_analysis_summary.csv")

