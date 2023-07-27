################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  magrittr,
  tidyverse
)

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
piracy <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "piracy",
  # # billing = "juancv-stanford",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

## PROCESSING ##################################################################

# Get data for clusters only ---------------------------------------------------
with_clusters <- tbl(piracy, "piracy_attacks_0_5") %>% 
  select(date,
         grid_id,
         lat_bin,
         lon_bin,
         attacks_window_next_1_month,
         attacks_window_next_2_month,
         attacks_window_next_3_month,
         attacks_window_next_4_month,
         attacks_window_next_5_month,
         attacks_window_next_6_month,
         attacks_window_next_7_month,
         attacks_window_next_8_month,
         attacks_window_next_9_month,
         attacks_window_next_10_month,
         attacks_window_next_11_month,
         attacks_window_next_12_month,
         attacks_window_last_1_month,
         attacks_window_last_2_month,
         attacks_window_last_3_month,
         attacks_window_last_4_month,
         attacks_window_last_5_month,
         attacks_window_last_6_month,
         attacks_window_last_7_month,
         attacks_window_last_8_month,
         attacks_window_last_9_month,
         attacks_window_last_10_month,
         attacks_window_last_11_month,
         attacks_window_last_12_month,
         days_since_attack,
         grid_has_previous_attacks) %>% 
  mutate(attack_cluster = case_when((between(lat_bin, -5.75, 11.45) & between(lon_bin, -9, 14.7)) ~ "GoG",
                                    (between(lat_bin, -19.8, 32.1) & between(lon_bin, 83, 129.3)) ~ "SEA",
                                    (between(lat_bin, -10.35, 31.6) & between(lon_bin, 33.2, 72.6)) ~ "GoA",
                                    T ~ "None"
  ))

# Get grid-level information from the tracks -----------------------------------
track_info <- tbl(piracy, "gridded_data_ml") %>% 
  group_by(date, lat_bin, lon_bin) %>% 
  summarize(hours = sum(hours, na.rm = T),
            distance_km = sum(distance_km, na.rm = T),
            n_vessels = n_distinct(mmsi),
            n_trips = n_distinct(mmsi),
            n_ais_messages = sum(ais_messages, na.rm = T),
            .groups = "drop") %>% 
  ungroup()

# Combine both into the final panel --------------------------------------------
gridded_panel <- with_clusters %>% 
  left_join(track_info,
            by = c("date", "lat_bin", "lon_bin"))

# X ----------------------------------------------------------------------------
local_gridded_panel <- collect(gridded_panel)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = local_gridded_panel,
        file = here("processed_data", "attacks_and_activity_by_grid.rds"))
