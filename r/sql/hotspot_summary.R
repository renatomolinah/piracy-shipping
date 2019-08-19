sql <- "
#standardSQL
  WITH base AS(
  SELECT
    avg_distance_km,
    hours,
    IF(hotspot_gulf_of_guinea = 1
      OR hotspot_southeast_asia = 1
      OR hotspot_gulf_of_aden = 1, TRUE,FALSE) hotspot,
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
            hours = sum(hours))

percentage_distance <- (summary %>%
  filter(hotspot) %>%
  .$avg_distance_km) / (sum(summary$avg_distance_km)) * 100


percentage_hours <- (summary %>%
                          filter(hotspot) %>%
                          .$hours) / (sum(summary$hours)) * 100
percentage_distance
percentage_hours
