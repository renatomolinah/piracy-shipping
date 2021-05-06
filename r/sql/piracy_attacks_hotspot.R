bq_table(project = project,table = "piracy_attacks_hotspot",dataset = "piracy") %>% 
  bq_table_upload(values = expanded_asam_hotspot,
                  fields = as_bq_fields(expanded_asam_hotspot),
                  write_disposition = "WRITE_TRUNCATE")


sql <- "WITH
main AS(
SELECT 
trip_id,
departure_date,
hotspot_gulf_of_guinea,
hotspot_southeast_asia,
hotspot_gulf_of_aden
FROM
`ucsb-gfw.piracy.voyages_5`),
gulf_of_guinea AS(
SELECT
trip_id,
departure_date,
'Gulf of Guinea' Hotspot
FROM 
main
WHERE hotspot_gulf_of_guinea = 1),
southeast_asia AS(
SELECT
trip_id,
departure_date,
'Southeast Asia' Hotspot
FROM 
main
WHERE hotspot_southeast_asia = 1),
gulf_of_aden AS(
SELECT
trip_id,
departure_date,
'Gulf of Aden' Hotspot
FROM 
main
WHERE hotspot_gulf_of_aden = 1)
SELECT
*
FROM
gulf_of_guinea
UNION ALL
(SELECT * FROM southeast_asia)
UNION ALL
(SELECT * FROM gulf_of_aden)"

bq_project_query(billing_project,sql, 
                 destination_table = bq_table(project = project,table = "trip_hotspot_long",dataset = "piracy"),use_legacy_sql = FALSE, allowLargeResults = TRUE,write_disposition = "WRITE_TRUNCATE")

sql <-"WITH
trip_hotspot_long AS(
SELECT
  trip_id,
  departure_date date,
  Hotspot
FROM
  `ucsb-gfw.piracy.trip_hotspot_long`),
  piracy_attacks_hotspot AS(
      SELECT 
      *
      FROM
      `ucsb-gfw.piracy.piracy_attacks_hotspot`
  ),
joined AS(
SELECT 
*
FROM 
trip_hotspot_long
LEFT JOIN 
piracy_attacks_hotspot
USING(Hotspot,date))
SELECT 
trip_id,
SUM(hotspot_attacks_window_last3_month) hotspot_attacks_window_last3_month,
SUM(hotspot_attacks_window_last6_month) hotspot_attacks_window_last6_month,
SUM(hotspot_attacks_window_last9_month) hotspot_attacks_window_last9_month,
SUM(hotspot_attacks_window_last12_month) hotspot_attacks_window_last12_month
FROM joined
GROUP BY
trip_id"

bq_project_query(billing_project,sql, 
                 destination_table = bq_table(project = project,table = "trip_hotspot_summary",dataset = "piracy"),use_legacy_sql = FALSE, allowLargeResults = TRUE,write_disposition = "WRITE_TRUNCATE")
