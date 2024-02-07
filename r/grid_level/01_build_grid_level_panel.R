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
  asam,
  sf,
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

## PROCESSING ##################################################################

# Get data for clusters only ---------------------------------------------------
with_clusters <- tbl(piracy, "gridded_pirate_attacks_0_5") %>% 
  mutate(grid_id = paste0(lat_bin, "_", lon_bin),
         attack_cluster = case_when(hotspot_gulf_of_guinea == 1 ~ "GoG",
                                    hotspot_southeast_asia == 1 ~ "SEA",
                                    hotspot_gulf_of_aden == 1 ~ "GoA",
                                    T ~ "None")) %>% 
  select(date,
         grid_id,
         lat_bin,
         lon_bin,
         days_since_attack,
         number_previous_attacks_grid_1_month,
         number_previous_attacks_grid_3_months,
         number_previous_attacks_grid_12_months,
         number_previous_attacks_all_time,
         attack_cluster)

# Get grid-level information from the tracks -----------------------------------
track_info <- tbl(piracy, "gridded_data_0_5") %>% 
  group_by(date, lat_bin, lon_bin) %>% 
  summarize(time_hours = sum(hours, na.rm = T),
            distance_km = sum(distance_km, na.rm = T),
            n_vessels = n_distinct(mmsi),
            n_trips = n_distinct(trip_id),
            n_ais_messages = sum(ais_messages, na.rm = T),
            .groups = "drop")
  

# Combine both into the final panel --------------------------------------------
gridded_panel <- with_clusters %>% 
  left_join(track_info,
            by = c("date", "lat_bin", "lon_bin"))

local_gridded_panel <- collect(gridded_panel)

# Add FAO zone info  -----------------------------------------------------------
sf_use_s2(F)
asam_regions <- asam_subregions() %>% 
  select(asam_region = REGION,
         asam_subregion = SUBREGION)

grid_regions <- local_gridded_panel %>% 
  select(grid_id, lat_bin, lon_bin) %>% 
  distinct() %>% 
  st_as_sf(coords = c("lon_bin", "lat_bin"),
           crs = 4326) %>% 
  st_join(asam_regions, join = st_nearest_feature) %>%
  st_drop_geometry()

final <- local_gridded_panel %>% 
  left_join(grid_regions, by = "grid_id")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = final,
        file = here("processed_data", "attacks_and_activity_by_grid.rds"))
