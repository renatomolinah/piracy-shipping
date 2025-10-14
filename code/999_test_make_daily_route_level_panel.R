

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
activity <- tbl(piracy, "ungridded_data_v_20250521")
voyage_info <- tbl(piracy, "voyage_info_v_20250521")
attacks_all <- tbl(piracy, "route_all_time_date_has_attack_90p_threshold_v_20250521")

# Start queries ------------------------------------------------------------------
# A table matching trip id to their routes
basic_route_info <- voyage_info |>
  filter(from_country %in% c("AUS", "JPN"),
         to_country  %in% c("AUS", "JPN")) |>
  select(trip_id, from_country, from_port, to_country, to_port) |>
  distinct()


# A table with routes and an indicator for attacked dates
daily_attacks <- attacks_all |>
  filter(from_country %in% c("AUS", "JPN"),
         to_country  %in% c("AUS", "JPN")) |>
  filter(from_port == "NAGOYA",
         to_port == "USELESS LOOP") |>
  rename(attack = route_has_attack)

# Dates of attacks by route
attack_dates_by_route <- attacks_all |>
  filter(from_country %in% c("AUS", "JPN"),
         to_country  %in% c("AUS", "JPN"),
         route_has_attack) |>
  filter(from_port == "NAGOYA",
         to_port == "USELESS LOOP") |>
  rename(attack_date = date) |>
  mutate(attack_date_min = sql("DATE_ADD(attack_date, INTERVAL -15 DAY)"),
         attack_date_max = sql("DATE_ADD(attack_date, INTERVAL 15 DAY)")) |>
  select(from_country,
         to_country,
         from_port,
         to_port,
         attack_date_min,
         attack_date_max) |>
  left_join(daily_attacks,
            join_by(between(y$date, x$attack_date_min, x$attack_date_max),
                    from_country,
                    to_country,
                    from_port,
                    to_port)) |>
  select(-c(attack_date_min,
            attack_date_max)) |>
  distinct()

attack_dates_by_route_local <- collect(attack_dates_by_route)

# Daily activity by route
daily_activity <- activity |>
  mutate(date = sql("EXTRACT(date FROM timestamp)")) |>
  inner_join(basic_route_info, by = join_by(trip_id)) |>
  group_by(date, from_country, from_port, to_country, to_port) |>
  summarize(hours = sum(hours, na.rm = T),
            distance_km = sum(distance_km, na.rm = T),
            main_fuel_consumption_mt_inst = sum(main_fuel_consumption_mt_inst, na.rm = T),
            aux_fuel_consumption_mt_inst = sum(aux_fuel_consumption_mt_inst, na.rm = T),
            n_trips = n_distinct(trip_id),
            n_vessels = n_distinct(mmsi),
            .groups = "drop")

panel <- attack_dates_by_route |>
  left_join(daily_activity, by = join_by(date,
                                         from_country, from_port,
                                         to_country, to_port)) |>
  select(date,
         from_country, from_port, to_country, to_port, attack,
         hours, distance_km, main_fuel_consumption_mt_inst, aux_fuel_consumption_mt_inst,
         n_trips, n_vessels) |>
  # add_count(from_country, from_port, to_country, to_port) |>
  # filter(n == 4383) |>
  replace_na(list(hours = 0,
                  distance_km = 0,
                  main_fuel_consumption_mt_inst = 0,
                  aux_fuel_consumption_mt_inst = 0,
                  n_trips = 0,
                  n_vessels = 0))

local_panel <- collect(panel)

saveRDS(local_panel,
        here("data", "processed", "daily_attacks_and_activity_for_event_study.rds"))
#



# basic_route_info_local <- basic_route_info |> head(100) |> collect()
daily_attacks_local <- daily_attacks |>
  filter(from_port == "NAGOYA",
         to_port == "USELESS LOOP") |>
  collect()

daily_activity_local <- daily_activity |>
  filter(from_port == "NAGOYA",
         to_port == "USELESS LOOP") |>
  collect()

daily_attacks_local

daily_activity_local

test <- daily_attacks_local |>
  rename(attack_date = date) |>
  mutate(attack_date_min = attack_date - 15,
         attack_date_max = attack_date + 15) |>
  left_join(daily_activity_local, join_by(from_country,
                                          from_port,
                                          to_country,
                                          to_port,
                                          between(y$date, x$attack_date_min, x$attack_date_max))) |>
  arrange(date) |>
  replace_na(replace = list(hours = 0)) |>
  drop_na(date)


view(test)
