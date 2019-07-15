library(tidyverse)
library(bigrquery)

# Set up bigquery
project <-  "ucsb-gfw"

# Get region info
# Will need to join attacks to closest region
# https://stackoverflow.com/questions/53989172/sql-finding-the-closest-lat-lon-record-on-google-bigquery
sql <- "#standardSQL
  WITH region_points AS(
  SELECT
    CAST(ROUND(FLOOR(CAST(SUBSTR(gridcode, 17, 7) AS FLOAT64) / 1) * 1 + 0.5,2) AS STRING) lat_bin,
    CAST(ROUND(FLOOR(CAST(SUBSTR(gridcode, 5, 7) AS FLOAT64) / 1) * 1 + 0.5,2) AS STRING) lon_bin,
    IF(ARRAY_LENGTH(regions.ocean)=0,NULL,regions.ocean[ordinal(1)]) ocean,
    IF(ARRAY_LENGTH(regions.major_fao)=0,NULL,regions.major_fao[ordinal(1)]) major_fao,
    CAST(IF(ARRAY_LENGTH(regions.eez)=0,NULL,regions.eez[ordinal(1)]) AS INT64) eez_id
  FROM
    `world-fishing-827.pipe_reference.spatial_measures`),
  eez_count AS(
  SELECT
    lat_bin,
    lon_bin,
    eez_id,
    COUNT(*) count_eez_id
  FROM
    region_points
  WHERE
    NOT ocean IS NULL
    AND NOT major_fao IS NULL
  GROUP BY
    eez_id,
    lat_bin,
    lon_bin),
  major_fao_count AS(
  SELECT
    lat_bin,
    lon_bin,
    major_fao,
    COUNT(*) count_major_fao
  FROM
    region_points
  WHERE
    NOT ocean IS NULL
    AND NOT major_fao IS NULL
  GROUP BY
    major_fao,
    lat_bin,
    lon_bin),
  ocean_count AS(
  SELECT
    lat_bin,
    lon_bin,
    ocean,
    COUNT(*) count_ocean
  FROM
    region_points
  WHERE
    NOT ocean IS NULL
    AND NOT major_fao IS NULL
  GROUP BY
    ocean,
    lat_bin,
    lon_bin),
  ranked_eez_id AS(
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY lat_bin, lon_bin ORDER BY count_eez_id ASC) AS eez_id_rank
  FROM
    eez_count),
  ranked_major_fao AS(
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY lat_bin, lon_bin ORDER BY count_major_fao ASC) AS major_fao_rank
  FROM
    major_fao_count),
  ranked_ocean AS(
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY lat_bin, lon_bin ORDER BY count_ocean ASC) AS ocean_rank
  FROM
    ocean_count)
SELECT
  *
  EXCEPT(sovereign1_iso3,
  lon_bin,
  lat_bin,
  eez_id,
    eez_id_rank,
    major_fao_rank,
    ocean_rank,
    count_eez_id,
    count_major_fao,
    count_ocean),
      ST_GeogPoint(CAST(lon_bin AS FLOAT64),
      CAST(lat_bin AS FLOAT64)) region_point,
  IF(sovereign1_iso3 IS NULL,'high_seas',sovereign1_iso3) sovereign1_iso3
FROM (
  SELECT
    *
  FROM
    ranked_eez_id
  WHERE
    eez_id_rank = 1)
LEFT JOIN (
  SELECT
    *
  FROM
    ranked_major_fao
  WHERE
    major_fao_rank = 1) USING(lat_bin,
    lon_bin)
LEFT JOIN (
  SELECT
    *
  FROM
    ranked_ocean
  WHERE
    ocean_rank = 1) USING(lat_bin,
    lon_bin)
LEFT JOIN (
  SELECT
    eez_id,
    sovereign1_iso3
  FROM
    `world-fishing-827.gfw_research.eez_info`) USING(eez_id)"

bq_table(project = project,table = "gridded_region_info",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "gridded_region_info",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)


sql <-"#standardSQL
  WITH region_points AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.gridded_region_info`),
  attack_points AS(
  SELECT
    ST_GEOGFROMTEXT(point) attack_point,
    reference attack_reference
  FROM
    `piracy.asam`),
  joined AS(
  SELECT
    *,
    ST_Distance(attack_point,
      region_point) distance_m
  FROM
    attack_points
  CROSS JOIN
    region_points),
  ranked AS(
  SELECT
    *,
    ROW_NUMBER() OVER (PARTITION BY attack_reference ORDER BY distance_m ASC) AS region_rank
  FROM
    joined)
SELECT
  *
  EXCEPT(attack_point,
  region_point,
  region_rank,
  distance_m),
  st_x(attack_point) lon,
  st_y(attack_point) lat
FROM
  ranked
WHERE
  region_rank = 1"

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

cluster_boxes<-read_csv("processed_data/cluster_boxes.csv")
# Create sql SELECT clauses for each hotspot cluster
cluster_filters <- cluster_boxes %>%
  mutate(cluster_filter = paste0("(CASE WHEN (lat < ",lat_max, " AND lat > ",lat_min, " AND lon < ",lon_max," AND lon > ",lon_min,") THEN 1 ELSE 0 END) ",cluster))

cluster_filters <- paste0(cluster_filters$cluster_filter,collapse = ", ")

# Pull in data we want
sql<-glue::glue("
#standardSQL
WITH
hotspots AS(
SELECT
attack_reference,
{cluster_filters}
FROM
`piracy.asam_regions`), 
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
*,
EXTRACT(YEAR FROM date_attack) year_attack,
EXTRACT(MONTH FROM date_attack) month_attack,
EXTRACT(DAY FROM date_attack) day_attack
FROM
master
LEFT JOIN
cumulative_info
USING(attack_reference,
days_since_attack_bin)
LEFT JOIN
hotspots
USING(attack_reference)
LEFT JOIN (
  SELECT
    attack_reference,
    ocean,
    major_fao,
    sovereign1_iso3 eez
  FROM
    `ucsb-gfw.piracy.asam_regions`) USING(attack_reference)
")

bq_table(project = project,table = "point_analysis_cumulative",dataset = "piracy") %>% 
  bq_table_delete()
bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "point_analysis_cumulative",dataset = "piracy"),
                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

point_analysis_cumulative <- bq_project_query(project, "SELECT * FROM `piracy.point_analysis_cumulative`") %>%
  bq_table_download(max_results = Inf)

write_csv(point_analysis_cumulative,path="processed_data/point_analysis.csv")
