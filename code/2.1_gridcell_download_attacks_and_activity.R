################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Download the gridcell level information from BigQuery
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
# piracy <- dbConnect(
#   bigquery(),
#   project = "emlab-gcp",
#   dataset = "piracy",
#   billing = "emlab-gcp",
#   use_legacy_sql = FALSE,
#   allowLargeResults = TRUE
# )
# 
# gfw <- dbConnect(
#   bigquery(),
#   project = "global-fishing-watch",
#   dataset = "pipe_ais_v3_published",
#   billing = "emlab-gcp",
#   use_legacy_sql = FALSE,
#   allowLargeResults = TRUE
# )

con <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE
)

## PROCESSING ##################################################################
get_gridded_data <- function(tbl_sufix = "0_5") {
  
  res <- as.numeric(str_replace(tbl_sufix, "_", "."))
  
  # Get data for clusters only -------------------------------------------------
  with_clusters <- tbl(con,
                       DBI::Id(project = "emlab-gcp",
                               dataset = "piracy",
                               table = paste0("gridded_pirate_attacks_", tbl_sufix, "_v_20250521"))) %>%
    filter(date <= sql("DATE('2023-12-31')")) %>%
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
           number_previous_attacks_grid_6_months,
           number_previous_attacks_grid_12_months,
           number_previous_attacks_all_time,
           attack_cluster)
  
  # List of grids attacked in the date range
  grids_attacked_2012_2023 <- with_clusters %>%
    filter(days_since_attack == 0) %>%
    select(grid_id) %>%
    distinct()
  
  # Get grid-level information from the tracks ---------------------------------
  track_info <- tbl(con,
                    DBI::Id(project = "emlab-gcp",
                    dataset = "piracy",
                    table = paste0("gridded_data_", tbl_sufix, "_v_20250521"))) |> 
    group_by(date, lat_bin, lon_bin) %>%
    summarize(time_hours = sum(hours, na.rm = T),
              distance_km = sum(distance_km, na.rm = T),
              n_vessels = n_distinct(mmsi),
              n_trips = n_distinct(trip_id),
              n_ais_messages = sum(ais_messages, na.rm = T),
              .groups = "drop")
  
  # Get AIS disabling information ----------------------------------------------
  vessels <- tbl(con, DBI::Id(project = "emlab-gcp", dataset = "piracy", table = "vessel_info_v_20250521"))
  segs <- tbl(con, DBI::Id(project = "global-fishing-watch", dataset = "pipe_ais_v3_published", table = "segs_activity"))
  disab <- tbl(con, DBI::Id(project = "global-fishing-watch", dataset = "pipe_ais_v3_published", table = "product_events_ais_disabling"))
  gaps <- tbl(con, DBI::Id(project = "global-fishing-watch", dataset = "pipe_ais_v3_published", table = "product_events_ais_gaps"))
  
  # Non-fishing vessels only
  target_vessels <- vessels |> 
    select(mmsi) |> 
    distinct()
  
  # Identify good segments
  good_segs <- segs |> 
    filter(!overlapping_and_short,
           good_seg) |> 
    select(seg_id) |> 
    distinct()
  
  # Identify disabling events (id and start date)
  disab_ids <- disab |> 
    mutate(date = sql("EXTRACT(DATE FROM event_start)")) |> 
    select(event_id, date)
  
  # Filter all gaps to retain only info from:
  # - good segments
  # - vessels that are not fishing
  # - disabling events
  # And then count number of unique disabling events by grid cell and date
  gridded_gaps <- gaps |> 
    inner_join(good_segs, by = join_by(gap_start_seg_id == seg_id)) |> 
    inner_join(target_vessels, by = join_by(ssvid == mmsi)) |> 
    mutate(gap_date = sql("EXTRACT(DATE FROM gap_start)")) |> 
    inner_join(disab_ids, by = join_by(gap_id == event_id, gap_date == date)) |> 
    mutate(gap_lon_bin = floor(gap_start_lon / res) * res,
           gap_lat_bin = floor(gap_start_lat / res) * res) |> 
    group_by(gap_date, gap_lon_bin, gap_lat_bin) |> 
    summarize(n_ais_disabling = n_distinct(gap_id),
              .groups = "drop")

  # Combine all into the final panel --------------------------------------------
  gridded_panel <- with_clusters %>%
    inner_join(grids_attacked_2012_2023,
               by = "grid_id") %>%
    left_join(track_info,
              by = c("date", "lat_bin", "lon_bin")) |> 
    left_join(gridded_gaps, by = join_by(date == gap_date,
                                         lat_bin == gap_lat_bin,
                                         lon_bin == gap_lon_bin))
  
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
    left_join(grid_regions, by = "grid_id") |> 
    replace_na(replace = list(time_hours = 0,
                              distance_km = 0,
                              n_vessels = 0,
                              n_trips = 0,
                              n_ais_messages = 0,
                              n_ais_disabling = 0))
}

gridded_0_1 <- get_gridded_data(tbl_sufix = "0_1")
gridded_0_5 <- get_gridded_data("0_5")
gridded_1 <- get_gridded_data("1")

# Test no NAs in outcome variables
gridded_0_1 |> 
  select(time_hours:n_ais_disabling) |> 
  lapply(function(x){sum(is.na(x))})

gridded_0_5 |> 
  select(time_hours:n_ais_disabling) |> 
  lapply(function(x){sum(is.na(x))})

gridded_1 |> 
  select(time_hours:n_ais_disabling) |> 
  lapply(function(x){sum(is.na(x))})

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
saveRDS(object = gridded_0_1,
        file = here("data",
                    "processed",
                    "attacks_and_activity_by_grid_0_1.rds"))
saveRDS(object = gridded_0_5,
        file = here("data",
                    "processed",
                    "attacks_and_activity_by_grid_0_5.rds"))
saveRDS(object = gridded_1,
        file = here("data",
                    "processed",
                    "attacks_and_activity_by_grid_1.rds"))






























