# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
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
route <- tbl(piracy, "route_prior_date_has_attack_v_20241105")
voyage <- tbl(piracy, "voyage_data_5_v_20240327")

## PROCESSING ##################################################################

# Build weekly version of each -------------------------------------------------
# Weekly attacks by route
route_wkly <- route %>% 
  filter(sql("EXTRACT(year FROM date) = 2017")) %>% 
  mutate(year = sql("EXTRACT(year FROM date)"),
         week = sql("EXTRACT(week FROM date)")) %>% 
  group_by(year, week, from_country, from_port, to_country, to_port) %>% 
  summarize(attack = any(route_has_attack),
            days_with_attack = sum(ifelse(route_has_attack, 1, 0)),
            .groups = "drop")

# Weekly transit by route, based on when a trip leaves port
voyage_wkly <- voyage %>% 
  filter(sql("EXTRACT(year FROM departure_date) = 2017")) %>% 
  mutate(year = sql("EXTRACT(year FROM departure_date)"),
         week = sql("EXTRACT(week FROM departure_date)")) %>% 
  group_by(year, week, from_country, from_port, to_country, to_port) %>% 
  summarize(hours = sum(hours),
            distance_km = sum(distance_km),
            n_mmsi = n_distinct(mmsi),
            n_trips = n_distinct(trip_id))

# Combine
panel <- route_wkly %>% 
  left_join(voyage_wkly, by = join_by(year, week, from_country, from_port, to_country, to_port)) %>% 
  arrange(from_country, from_port, to_country, to_port, year, week) %>% 
  select(year, week, from_country, from_port, to_country, to_port,
         attack, days_with_attack,
         hours, distance_km, n_mmsi, n_trips)

local <- collect(panel)

saveRDS(local,
        here("processed_data", "attack_counts.rds"))
