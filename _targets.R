# Load packages required to define the pipeline:
library(targets)
# Set target options:
tar_option_set(
  # Load necessary packages
  packages = c("tidyverse")
)
# Source all necessary functions
tar_source("r/functions.R")

# Set directory where target objects will be saved
# emLab's Google Shared Drive where targets interm rds objects will live
# This path will need to be modified by each user, since everyone has a different path to this directory
# Uncomment this line, and run it for your personal machine
# targets::tar_config_set(store = "/Users/gmcdonald/Library/CloudStorage/GoogleDrive-gmcdonald@ucsb.edu/Shared\ drives/emlab/Projects/current-projects/piracy/data/_targets")

# The data directory for raw data is set in relation to the targets directory
data_directory <- targets::tar_config_get("store") |>
  stringr::str_remove_all("/_targets")

# Set up billing and project info for BigQuery
# Not that this requires authentication, so not all users will be able to do this
billing_project <- "emlab-gcp" # emLab's billing project
bq_dataset <- "piracy" # The dataset name for this project

# Set ggplot theme for all plots
ggplot2::theme_set(ggplot2::theme_bw() +
                     ggplot2::theme(axis.title.y = ggplot2::element_text(vjust=0.6),
                                    strip.background = ggplot2::element_blank(),
                                    panel.background = ggplot2::element_blank(),
                                    panel.border = ggplot2::element_blank(),
                                    panel.grid.minor = ggplot2::element_blank()))

# Note: Any target name that includes _bq at the end creates a table on BigQuery
# in the emlab-gcp.piracy dataset
list(
  # Process ASAM encounter data
  tar_target(
    name = asam_file,
    glue::glue("{data_directory}/raw/asam_data_download/ASAM_events.shp"),
    format = "file"
  ),
  tar_target(
    name = asam_data,
    sf::st_read(asam_file)
  ),
  tar_target(
    name = asam_data_processed,
    process_asam_data(asam_data,
                      table_name = "asam_data_v_20240228")
  ),
  # Add clusters to encounter data
  tar_target(
    name = asam_with_hotspots,
    generate_asam_with_hotspots(asam_data_processed,
                                year_min = 2010,
                                years_to_include = 12)
  ),
  # Create hotspot cluster bounding boxes
  tar_target(
    name = hotspots,
    generate_hotspot_boundaries(asam_with_hotspots)
  ),
  # Create SQL code for hotspot cluster bounding boxes
  # This will go into the gridded SQL queries
  tar_target(
    name = hotspots_sql,
    generate_hotspot_sql(hotspots)
  ),
  # Process wind data
  tar_target(
    name = wind_file,
    glue::glue("{data_directory}/raw/wind/era5_monthly_average_wind.nc"),
    format = "file"
  ),
  tar_target(
    name = wind_data_processed_5,
    process_wind_data(wind_file,
                      pixel_size = 5,
                      table_name = "wind_data_5_v_20240228")
  ),
  # Process fuel data
  tar_target(
    name = fuel_file,
    glue::glue("{data_directory}/raw/bix_world_ifo_380_index.csv"),
    format = "file"
  ),
  tar_target(
    name = fuel_data,
    process_fuel_data(fuel_file,
                      table_name = "fuel_prices_v_20240228")
  ),
  # Get vessel info for shipping vessels in BigQuery
  tar_target(
    name = vessel_info_sql,
    "sql/vessel_info.sql",
    format = "file"
  ),
  # Generate vessel characteristic info and save on BigQuery
  tar_target(
    name = vessel_info_bq,
    run_gfw_query(sql = vessel_info_sql %>%
                    readr::read_file(),
                  bq_table_name = "vessel_info_v_20240228")
  ),
  # Get voyage trip info in BigQuery
  tar_target(
    name = voyage_info_sql,
    "sql/voyage_info.sql",
    format = "file"
  ),
  # Generate voyage info and save on BigQuery
  tar_target(
    name = voyage_info_bq,
    run_gfw_query(sql = voyage_info_sql %>%
                    readr::read_file(),
                  bq_table_name = "voyage_info_v_20240228", 
                  # Trigger re-run of this if timestamp changes for vessel_info_bq
                  vessel_info_bq)
  ),
  # Process ungridded, raw AIS messages
  # This ungridded dataset will serve as the basis of all gridded and voyage-level datasets
  tar_target(
    name = ungridded_data_bq_sql,
    "sql/ungridded_data.sql",
    format = "file"
  ),
  tar_target(
    name = ungridded_data_bq,
    run_gfw_query(sql = ungridded_data_bq_sql %>%
                    readr::read_file(),
                  bq_table_name = "ungridded_data_v_20240228",
                  # Trigger re-run of this if timestamp changes for vessel_info_bq
                  vessel_info_bq,
                  # Trigger re-run of this if timestamp changes for voyage_info_bq
                  voyage_info_bq)
  ),
  # Create subsample of ungridded data for testing purposes
  # Filter to just those trips which began and ended in 2019
  tar_target(
    name = ungridded_data_test_bq,
    run_gfw_query(sql = "SELECT * FROM `emlab-gcp.piracy.ungridded_data_v_20240228` WHERE departure_timestamp >= '2019-01-01' AND arrival_timestamp <= '2019-12-31'",
                  bq_table_name = "ungridded_data_test_v_20240305")
  ),
  # Aggregate ungridded data to level of trip departure date and 5x5 degree pixels
  # This will eventually further be aggregated to the voyage-level for the voyage-level analysis
  # This loads the SQL query
  tar_target(
    name = gridded_data_5_sql,
    "sql/gridded_data_5.sql",
    format = "file"
  ),
  # Run the query and save it on BQ
  tar_target(
    name = gridded_data_5_bq,
    run_gfw_query(sql = gridded_data_5_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_5_test_v_20240305", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_test_bq
                  ungridded_data_test_bq)
  ),
  # Aggregate ungridded data to level of shipping activity date and 0.5x0.5 degree pixels
  # This will be the basis of the grid-level analysis
  # This loads the SQL query
  tar_target(
    name = gridded_data_0_5_sql,
    "sql/gridded_data_0_5.sql",
    format = "file"
  ),
  # Run the query and save it on BQ
  tar_target(
    name = gridded_data_0_5_bq,
    run_gfw_query(sql = gridded_data_0_5_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 0.5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_0_5_test_v_20240305", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_test_bq
                  ungridded_data_test_bq)
  ),
  # Generate some gridded pirate attack data, for the grid-level analysis
  tar_target(
    name = gridded_pirate_attacks_sql,
    "sql/gridded_pirate_attacks.sql",
    format = "file"
  ),
  tar_target(
    name = gridded_pirate_attacks_0_5_bq,
    run_gfw_query(sql = gridded_pirate_attacks_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 0.5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_pirate_attacks_0_5_v_20240228", 
                  # Trigger re-run of this if process_asam_data is run
                  process_asam_data)
  ),
  # Here we summarize aggregate spatial shipping activity at 0.5x0.5 degrees, for making a global map
  tar_target(
    name = aggregate_spatial_shipping_activity_sql,
    "sql/aggregate_spatial_shipping_activity.sql",
    format = "file"
  ),
  # Run this query and save to BigQuery
  tar_target(
    name = aggregate_spatial_shipping_activity_bq,
    run_gfw_query(sql = aggregate_spatial_shipping_activity_sql %>%
                    readr::read_file(),
                  bq_table_name = "aggregate_spatial_shipping_activity_v_20240228", 
                  # Trigger re-run of this if timestamp changes for gridded_data_0_5_bq
                  gridded_data_0_5_bq)
  ),
  # Pull gridded shipipng data locally
  tar_target(
    name = aggregate_spatial_shipping_activity,
    pull_gfw_data_locally(bq_table_name = "aggregate_spatial_shipping_activity_v_20240228", 
                  # Trigger re-run of this if timestamp changes for aggregate_spatial_shipping_activity_bq
                  aggregate_spatial_shipping_activity_bq)
  ),
  # Process the average number of attacks that occurred along each route, by trip
  tar_target(
    name = average_route_attacks_per_route_and_trip_sql,
    "sql/average_route_attacks_per_route_and_trip.sql",
    format = "file"
  ),
  # Run the query to generate the average number of attacks that occurred along each route, by trip
  # Save to BigQuery
  tar_target(
    name = average_route_attacks_per_route_and_trip_bq,
    run_gfw_query(sql = average_route_attacks_per_route_and_trip_sql %>%
                    readr::read_file(),
                  bq_table_name = "average_route_attacks_per_route_and_trip_test_v_20240305", 
                  # Trigger re-run of this if timestamp changes for gridded_data_5_bq
                  gridded_data_5_bq)
  ),
  # Process voyage-level data, which aggregates gridded data
  tar_target(
    name = voyage_data_sql,
    "sql/voyage_data.sql",
    format = "file"
  ),
  # Run query to generate voyage-level data and save on BigQuery
  tar_target(
    name = voyage_data_bq,
    run_gfw_query(sql = voyage_data_sql %>%
                    readr::read_file(),
                  bq_table_name = "voyage_data_5_test_v_20240305", 
                  #Trigger re-run of this if timestamp changes for gridded_data_5_bq
                  gridded_data_5_test_bq,
                  # Trigger re-run of this if timestamp changes for average_route_attacks_per_route_and_trip_bq
                  average_route_attacks_per_route_and_trip_bq)
  ),
  # Pull voyage-level data from BigQuery to local environment
  tar_target(
    name = voyage_data,
    pull_gfw_data_locally(bq_table_name = "voyage_data_5_test_v_20240305", 
                          # Trigger re-run of this if timestamp changes for voyage_data_bq
                          voyage_data_bq)
  ),
  # Set projection for global map figures
  tar_target(
    name = global_map_projection,
    "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  ),
  # Make global map showing attacks, hotspots, and shipping activity
  tar_target(
    name = global_map_figure,
    make_global_map_figure(asam_with_hotspots,
                           hotspots,
                           aggregate_spatial_shipping_activity,
                           map_projection = global_map_projection)
  ),
  # Make barplot that shows attacks over time, by hotspot
  tar_target(
    name = encounter_time_series_figure,
    make_encounter_time_series_figure(asam_data_processed,
                                      hotspots)
  ),
  # Summarize annual shipping activity by EEZ
  tar_target(
    name = shipping_activity_by_year_eez_sql,
    "sql/shipping_activity_by_year_eez.sql",
    format = "file"
  ),
  # Run this query and save the data on BigQuery
  tar_target(
    name = shipping_activity_by_year_eez_bq,
    run_gfw_query(sql = shipping_activity_by_year_eez_sql %>%
                    readr::read_file(),
                  bq_table_name = "shipping_activity_by_year_eez_v_20240228", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_bq
                  ungridded_data_bq)
  ),
  # Pull the annual shipping activty by EEZ data locally
  tar_target(
    name = shipping_activity_by_year_eez,
    pull_gfw_data_locally(bq_table_name = "shipping_activity_by_year_eez_v_20240228", 
                  # Trigger re-run of this if timestamp changes for shipping_activity_by_year_eez_bq
                  shipping_activity_by_year_eez_bq)
  )
)
