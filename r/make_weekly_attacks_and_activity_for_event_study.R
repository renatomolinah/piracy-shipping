

# Activity comes from here: ungridded_data_test_v_20240305
# Route info from here: voyage_info_v_20240228
# Pirate info from here: route_prior_date_has_attack_v_20241105

# Load packages ----------------------------------------------------------------
  pacman::p_load(
    here,
    DBI,
    glue,
    bigrquery,
    tidyverse
  )

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
piracy <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "piracy",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)


# Identify data sets -----------------------------------------------------------
top_routes <- readRDS(here("top_route_list.rds"))
activity <- tbl(piracy, "ungridded_data_v_20240228")
route_info <- tbl(piracy, "voyage_info_v_20240228")
attacks <- tbl(piracy, "route_prior_date_has_attack_v_20241105")

# Start query ------------------------------------------------------------------
# A table matching trip id to their routes
basic_route_info <- route_info %>% 
  select(trip_id, from_country, from_port, to_country, to_port) %>% 
  distinct()

# A table factorial combinations of weeks and routes, with an indicator for weeks with an attack
weekly_attacks <- attacks %>% 
  filter(
    sql(
      paste0(
        'CONCAT(from_port, " ", to_port) IN ("', paste(top_routes, collapse = '", "'), '") OR 
        CONCAT(to_port, " ", from_port) IN ("', paste(top_routes, collapse = '", "'), '")'
      )
    )
  ) %>% 
  filter(sql("EXTRACT(year FROM date) IN (2017, 2018)")) %>%
  mutate(year = sql("EXTRACT(year FROM date)"),
         week = sql("EXTRACT(week FROM date)")) %>% 
  group_by(year, week, from_country, from_port, to_country, to_port) %>% 
  summarize(attack = any(route_has_attack),
            days_with_attack = sum(ifelse(route_has_attack, 1, 0)),
            .groups = "drop")

# Weekly activity by route
weekly_activity <- activity %>% 
  filter(sql("EXTRACT(year FROM timestamp) IN (2017, 2018)")) %>%
  mutate(year = sql("EXTRACT(year FROM timestamp)"),
         week = sql("EXTRACT(week FROM timestamp)")) %>% 
  left_join(basic_route_info, by = join_by(trip_id)) %>% 
  group_by(year, week, from_country, from_port, to_country, to_port) %>% 
  summarize(hours = sum(hours, na.rm = T),
            distance_km = sum(distance_km, na.rm = T),
            main_fuel_consumption_mt_inst = sum(main_fuel_consumption_mt_inst, na.rm = T),
            aux_fuel_consumption_mt_inst = sum(aux_fuel_consumption_mt_inst, na.rm = T),
            .groups = "drop")

panel <- weekly_attacks %>% 
  left_join(weekly_activity, by = join_by(year, week, from_country, from_port, to_country, to_port)) %>% 
  select(year, week,
         from_country, from_port, to_country, to_port,
         attack, days_with_attack,
         hours, distance_km, main_fuel_consumption_mt_inst, aux_fuel_consumption_mt_inst)

local_panel <- collect(panel)

saveRDS(local_panel,
        here("processed_data", "weekly_attacks_and_activity_for_event_study.rds"))
