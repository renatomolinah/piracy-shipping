sql <- "
#standardSQL
  WITH base AS(
  SELECT
    avg_distance_km,
    hours,
    (CASE
        WHEN hotspot_gulf_of_guinea = 1 THEN 'hotspot_gulf_of_guinea'
        WHEN hotspot_southeast_asia = 1 THEN 'hotspot_southeast_asia'
        WHEN hotspot_gulf_of_aden = 1 THEN 'hotspot_gulf_of_aden'
        ELSE 'none' END) hotspot,
    year
  FROM
    `ucsb-gfw.piracy.voyage_ais_positions`)
SELECT
  hotspot,
  year,
  SUM(avg_distance_km) avg_distance_km,
  SUM(hours) hours
FROM
  base
GROUP BY
  hotspot,
  year"

bq_table(project = project,table = "hotspot_summary",dataset = "piracy") %>% 
  bq_table_delete()

bq_project_query(project,query = sql, destination_table = bq_table(project = project,table = "hotspot_summary",dataset = "piracy"),
                 allowLargeResults = TRUE)

hotspot_summary <- bq_project_query(project, "SELECT * FROM `piracy.hotspot_summary`") %>%
  bq_table_download(max_results = Inf)

summary <- hotspot_summary %>%
  group_by(hotspot) %>%
  summarize(avg_distance_km = sum(avg_distance_km),
            hours = sum(hours),
            avg_distance_km_fraction = sum(avg_distance_km) / sum(hotspot_summary$avg_distance_km) * 100,
            hours_fraction = sum(hours) / sum(hotspot_summary$hours) * 100)

summary 

percentage_distance <- (summary %>%
  filter(hotspot != "none") %>%
  .$avg_distance_km %>% sum()) / (sum(summary$avg_distance_km)) * 100


percentage_hours <- (summary %>%
                          filter(hotspot != "none") %>%
                          .$hours %>% sum()) / (sum(summary$hours)) * 100
percentage_distance
percentage_hours