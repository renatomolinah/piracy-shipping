library(tidyverse)
library(bigrquery)

# Set up bigquery
project <-  "ucsb-gfw"

# Get region info
# Will need to join attacks to closest region
# https://stackoverflow.com/questions/53989172/sql-finding-the-closest-lat-lon-record-on-google-bigquery
sql <-"#standardSQL
  WITH region_points AS(
  SELECT
    CAST(SUBSTR(gridcode, 5, 7) AS FLOAT64) lon,
    CAST(SUBSTR(gridcode, 17, 7) AS FLOAT64) lat,
    IF(ARRAY_LENGTH(regions.ocean)=0,NULL,regions.ocean[ordinal(1)]) ocean,
    IF(ARRAY_LENGTH(regions.major_fao)=0,NULL,regions.major_fao[ordinal(1)]) major_fao,
    CAST(IF(ARRAY_LENGTH(regions.eez)=0,NULL,regions.eez[ordinal(1)]) AS INT64) eez_id
  FROM
    `world-fishing-827.pipe_reference.spatial_measures`),
  attack_points AS(
  SELECT
    ROUND(FLOOR(ST_Y(ST_GEOGFROMTEXT(point)) / 0.01) * 0.01,2) lat,
    ROUND(FLOOR(ST_X(ST_GEOGFROMTEXT(point)) / 0.01) * 0.01,2) lon,
    reference attack_reference
  FROM
    `piracy.asam`)
 SELECT
*
EXCEPT(eez_id)
FROM
attack_points
LEFT JOIN
region_points
USING(lat,lon)
LEFT JOIN(SELECT
eez_id,
sovereign1_iso3
FROM
`world-fishing-827.gfw_research.eez_info`)
USING(eez_id)"

bq_table(project = project,table = "asam_regions",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "asam_regions",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql <-
  "
#standardSQL
  WITH ping_info AS (
  SELECT
    lat,
    lon,
    timestamp,
    hours,
    avg_distance_km,
    trip_id
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
  ARRAY_AGG(DISTINCT trip_id) voyage_id_array
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
    date date_attack,
    ST_GEOGFROMTEXT(point) attack_geog
  FROM
    `ucsb-gfw.piracy.asam`),
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
  distance_to_attack_m <= 500e5"

bq_table(project = project,table = "point_distance_lookup",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_distance_lookup",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql<-"#standardSQL
  WITH attack_info_1 AS(
  SELECT
    reference attack_reference_1,
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
    `ucsb-gfw.piracy.point_distance_lookup`
  WHERE distance_to_attack_m <= 500000),
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
    date_attack,
    date_shipping,
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
  *,
    DATE_DIFF(date_shipping,
      date_attack,
      DAY) days_since_attack
FROM
  final"

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
    date_attack,
    days_since_attack_bin,
    distance_to_attack_km_bin"

bq_table(project = project,table = "point_summary_shipping",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_summary_shipping",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

sql<-"#standardSQL
  WITH master AS(
  SELECT
    attack_reference,
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
    date_attack,
    days_since_attack_bin,
    distance_to_attack_km_bin)
SELECT
  *
FROM
  master
LEFT JOIN (
  SELECT
    attack_reference,
    ocean,
    major_fao,
    sovereign1_iso3 eez
  FROM
    `ucsb-gfw.piracy.asam_regions`) USING(attack_reference)
WHERE distance_to_attack_km_bin <= 500"

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
    date_attack)"

bq_table(project = project,table = "point_analysis_summary",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_analysis_summary",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

# Pull in data we want
sql<-"
#standardSQL
WITH
master AS(
SELECT * 
FROM `piracy.point_analysis_summary` 
WHERE 
distance_to_attack_km_bin <= 500
AND date_attack < DATE(TIMESTAMP('2018-01-01'))),
cumulative_info AS(
SELECT 
attack_reference,
days_since_attack_bin,
SUM(number_attacks) cumulative_attacks_across_space
FROM master
GROUP BY
attack_reference,
days_since_attack_bin)
SELECT
*
FROM
master
LEFT JOIN
cumulative_info
USING(attack_reference,
days_since_attack_bin)
"

bq_table(project = project,table = "point_analysis_cumulative",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_analysis_cumulative",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

point_analysis_cumulative <- bq_project_query(project, "SELECT * FROM `piracy.point_analysis_cumulative`") %>%
  bq_table_download(max_results = Inf)

write_csv(point_analysis_cumulative,path="processed_data/point_analysis.csv")

