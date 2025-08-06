

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
activity <- tbl(piracy, "ungridded_data_v_20250521")
voyage_info <- tbl(piracy, "voyage_info_v_20250521")
attacks_all <- tbl(piracy, "route_all_time_date_has_attack_90p_threshold_v_20250521")
attacks_prev <- tbl(piracy, "route_prior_date_has_attack_v_20250521")
voyage_data <- tbl(piracy, "voyage_data_5_v_20250521")
hotspots <- tbl(piracy, "gridded_data_5_v_20250521")

attacks <- attacks_all |>
  select(date, from_country, from_port, to_country, to_port) |>
  left_join(attacks_prev,
            by = join_by(date, from_country, from_port, to_country, to_port)) |>
  replace_na(replace = list(route_has_attack = FALSE))

# Start query ------------------------------------------------------------------
# A table matching trip id to their routes
basic_route_info <- voyage_info |>
  select(trip_id, from_country, from_port, to_country, to_port) |>
  distinct()

# A table factorial combinations of day and routes, with an indicator for days with an attack
daily_attacks <- attacks_all |>
  # Keep only data from "top routes"
  filter(
    sql(
      paste0(
        'CONCAT(from_port, " ", to_port) IN ("', paste(top_routes, collapse = '", "'), '") OR
        CONCAT(to_port, " ", from_port) IN ("', paste(top_routes, collapse = '", "'), '")'
      )
    )
  ) |>
  group_by(date, from_country, from_port, to_country, to_port) |>
  summarize(attack = any(route_has_attack),
            days_with_attack = sum(ifelse(route_has_attack, 1, 0)),
            .groups = "drop")

# Daily activity by route
daily_activity <- activity |>
  mutate(date = sql("EXTRACT(date FROM timestamp)")) |>
  left_join(basic_route_info, by = join_by(trip_id)) |>
  group_by(date, from_country, from_port, to_country, to_port) |>
  summarize(hours = sum(hours, na.rm = T),
            distance_km = sum(distance_km, na.rm = T),
            main_fuel_consumption_mt_inst = sum(main_fuel_consumption_mt_inst, na.rm = T),
            aux_fuel_consumption_mt_inst = sum(aux_fuel_consumption_mt_inst, na.rm = T),
            n_trips = n_distinct(trip_id),
            n_vessels = n_distinct(mmsi),
            .groups = "drop")

panel <- daily_attacks |>
  left_join(daily_activity, by = join_by(date, from_country, from_port, to_country, to_port)) |>
  select(date,
         from_country, from_port, to_country, to_port,
         attack, days_with_attack,
         hours, distance_km, main_fuel_consumption_mt_inst, aux_fuel_consumption_mt_inst,
         n_trips, n_vessels) |>
  add_count(from_country, from_port, to_country, to_port) |>
  filter(n == 4383) |>
  replace_na(list(hours = 0,
                  distance_km = 0,
                  main_fuel_consumption_mt_inst = 0,
                  aux_fuel_consumption_mt_inst = 0,
                  n_trips = 0,
                  n_vessels = 0)) |>
  select(-n)

local_panel <- collect(panel)

saveRDS(local_panel,
        here("data", "processed", "daily_attacks_and_activity_for_event_study.rds"))
#
