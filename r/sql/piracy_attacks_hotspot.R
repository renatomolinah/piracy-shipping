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


sql<-"
WITH
  main AS(
  SELECT
    mmsi,
    DATE(timestamp) date,
    hotspot_gulf_of_guinea,
    hotspot_southeast_asia,
    hotspot_gulf_of_aden
  FROM
    `ucsb-gfw.piracy.voyage_ais_positions`
  GROUP BY
    mmsi,
    date,
    hotspot_gulf_of_guinea,
    hotspot_southeast_asia,
    hotspot_gulf_of_aden),
  gulf_of_guinea AS(
  SELECT
    mmsi,
    date,
    'Gulf of Guinea' Hotspot
  FROM
    main
  WHERE
    hotspot_gulf_of_guinea = 1),
  southeast_asia AS(
  SELECT
    mmsi,
    date,
    'Southeast Asia' Hotspot
  FROM
    main
  WHERE
    hotspot_southeast_asia = 1),
  gulf_of_aden AS(
  SELECT
    mmsi,
    date,
    'Gulf of Aden' Hotspot
  FROM
    main
  WHERE
    hotspot_gulf_of_aden = 1)
SELECT
  *
FROM
  gulf_of_guinea
UNION ALL (
  SELECT
    *
  FROM
    southeast_asia)
UNION ALL (
  SELECT
    *
  FROM
    gulf_of_aden)
"
bq_project_query(billing_project,sql, 
                 destination_table = bq_table(project = project,table = "vessels_per_date_hotspot",dataset = "piracy"),use_legacy_sql = FALSE, allowLargeResults = TRUE,write_disposition = "WRITE_TRUNCATE")

bq_project_query(billing_project, ("SELECT * FROM `ucsb-gfw.piracy.vessels_per_date_hotspot`")) %>%
  bq_table_download(max_results = Inf) %>%
  write_csv(path=here::here("processed_data/vessels_per_date_hotspot.csv"))


vessels_per_date_hotspot <- read_csv(here::here("processed_data/vessels_per_date_hotspot.csv"))

# Use the following if you want to run sequentially
plan(sequential)
# Use the following if you want to run in paralell
# This takes a long time to run
library(furrr)
library(tictoc)
library(progressr)
#plan(multisession, workers = parallel::detectCores()-1)
# Set up progress bar for running in parallel
handlers(list(
  handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta",
    width    = 60,
    complete = "+"
  )))

date_range <- seq(ymd('2012-01-01'),ymd('2017-12-31'), by = '1 day')

expanded_vessels_hotspot <- expand.grid(date = date_range) %>%
  crossing(expand.grid(hotspot_gulf_of_guinea = c(0,1),
                       hotspot_southeast_asia = c(0,1),
                       hotspot_gulf_of_aden = c(0,1)) %>%
             filter(!(hotspot_gulf_of_guinea==0 &
                      hotspot_southeast_asia==0 &
                      hotspot_gulf_of_aden==0))) %>%
  left_join(vessels_per_date_hotspot,by="date") %>%
  as_tibble() %>%
  group_by(hotspot_gulf_of_guinea,hotspot_southeast_asia,hotspot_gulf_of_aden) %>%
  nest() %>%
  ungroup() %>%
  crossing(month = c(seq(3,12,3))) 
tic()
# here we train and predict probabilities of being an offender during cross-validation
with_progress({
  p <- progressor(steps = nrow(expanded_vessels_hotspot))
expanded_vessels_hotspot_summary <-expanded_vessels_hotspot %>%
  mutate(rolling_results = future_pmap(.,function(data,month_temp,hotspot_gulf_of_guinea_temp,hotspot_southeast_asia_temp,hotspot_gulf_of_aden_temp){
    tmp_summary <- purrr::map(date_range,function(date_temp){
      if(hotspot_gulf_of_guinea_temp==0) data <- data %>% filter(Hotspot != "Gulf of Guinea")
      if(hotspot_southeast_asia_temp==0) data <- data %>% filter(Hotspot != "Southeast Asia")
      if(hotspot_gulf_of_aden_temp==0) data <- data %>% filter(Hotspot != "Gulf of Aden")
      date_min <- date_temp - days(month_temp*30-1)
      if(date_min < min(data$date)) return(tibble(unique_hotspot_vessels = NA_real_,
                                                  date = date_temp))
      data %>%
        filter(date <= date_temp,
               date >= date_min) %>%
        summarize(unique_hotspot_vessels = n_distinct(mmsi)) %>%
        mutate(date = date_temp)
    }) %>%
      bind_rows()
    p()
    return(tmp_summary)
  },.options=furrr_options(seed=101))) %>%
  dplyr::select(-data) %>%
  unnest(rolling_results)
})
toc()

expanded_vessels_hotspot_summary_wide <- expanded_vessels_hotspot_summary %>%
  mutate(month = paste0(abs(month),"_month")) %>%
  #mutate(month = ifelse(month>0,paste0("next_",month,"_month"),paste0("last_",abs(month),"_month"))) %>%
  pivot_wider(names_from = "month",
              values_from = c("unique_hotspot_vessels"),
              names_prefix = "unique_hotspot_vessels_last_") 

write_csv(expanded_vessels_hotspot_summary_wide,here::here("processed_data/expanded_vessels_hotspot_summary.csv"))
expanded_vessels_hotspot_summary_wide <- read.csv(here::here("processed_data/expanded_vessels_hotspot_summary.csv"),stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(date = as_date(date))
bq_table(project = project,table = "expanded_vessels_hotspot_summary",dataset = "piracy") %>% 
  bq_table_upload(values = expanded_vessels_hotspot_summary_wide,
                  fields = as_bq_fields(expanded_vessels_hotspot_summary_wide),
                  write_disposition = "WRITE_TRUNCATE")

sql<-"WITH
  main AS(
  SELECT
    trip_id,
    departure_date date,
    hotspot_gulf_of_guinea,
    hotspot_southeast_asia,
    hotspot_gulf_of_aden
  FROM
    `ucsb-gfw.piracy.voyages_5`),
  vessels AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.expanded_vessels_hotspot_summary` ),
  attacks AS(
  SELECT
    *
  FROM
    `ucsb-gfw.piracy.trip_hotspot_summary` )
SELECT
  * EXCEPT(date,
    hotspot_gulf_of_guinea,
    hotspot_southeast_asia,
    hotspot_gulf_of_aden)
FROM
  main
LEFT JOIN
  attacks
USING
  (trip_id)
LEFT JOIN
  vessels
USING
  (date,
    hotspot_gulf_of_guinea,
    hotspot_southeast_asia,
    hotspot_gulf_of_aden)"

bq_project_query(billing_project,sql, 
                 destination_table = bq_table(project = project,table = "trip_hotspot_summary_full",dataset = "piracy"),use_legacy_sql = FALSE, allowLargeResults = TRUE,write_disposition = "WRITE_TRUNCATE")

bq_project_query(billing_project, ("SELECT * FROM `ucsb-gfw.piracy.trip_hotspot_summary_full`")) %>%
  bq_table_download(max_results = Inf) %>%
  write_csv(path=here::here("processed_data/trip_hotspot_summary_full.csv"))