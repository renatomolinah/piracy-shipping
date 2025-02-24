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

# targets::tar_config_set(store = "/Users/juancarlosvillasenorderbez/Library/CloudStorage/GoogleDrive-juancarlos@ucsb.edu/Shared drives/emlab/projects/current-projects/piracy/data/_targets")
# targets::tar_config_set(store = "/Users/Shared/nextcloud/emLab/projects/current-projects/piracy/data/_targets")


# The data directory for raw data is set in relation to the targets directory
data_directory <- targets::tar_config_get("store") |>
  stringr::str_remove_all("/_targets")

# Set up billing and project info for BigQuery
# Not that this requires authentication, so not all users will be able to do this
billing_project <- "emlab-gcp" # emLab's billing project
bq_dataset <- "piracy" # The dataset name for this project

# Set ggplot theme for all plots
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 7))


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
                      table_name = "asam_data_v_20250210")
  ),
  # Create hotspot cluster bounding boxes, using attacks from 2010 - 2021
  tar_target(
    name = hotspots,
    generate_hotspot_boundaries(asam_data_processed,
                                year_min = 2010,
                                years_to_include = 12)
  ),
  # # Using hotspot cluster bounding boxes, assign hotspot to each asam attack
  tar_target(
    name = asam_with_hotspots,
    generate_asam_with_hotspots(asam_data_processed, hotspots)
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
                  bq_table_name = "voyage_info_v_20250210", 
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
                  bq_table_name = "ungridded_data_v_20250210",
                  # Trigger re-run of this if timestamp changes for vessel_info_bq
                  vessel_info_bq,
                  # Trigger re-run of this if timestamp changes for voyage_info_bq
                  voyage_info_bq)
  ),
  # Keep only these trips
  # Apply some rules-of-thumb filters to remove potentially erroneous trips
  # i.e., trips > 60 days, distance greater than earth's circumfrence, from port = to port, 
  # distance greater than4x average trip distance
  tar_target(
    name = keep_these_trips_bq_sql,
    "sql/keep_these_trips.sql",
    format = "file"
  ),
  tar_target(
    name = keep_these_trips_bq,
    run_gfw_query(sql = keep_these_trips_bq_sql %>%
                    readr::read_file(),
                  bq_table_name = "keep_these_trips_v_20250210",
                  # Trigger re-run of this if timestamp changes for ungridded_data_bq
                  ungridded_data_bq,
                  # Trigger re-run of this if timestamp changes for voyage_info_bq
                  voyage_info_bq)
  ),
  # Aggregate ungridded data to level of trip departure date and 5x5 degree pixels
  # This will eventually further be aggregated to the voyage-level for the voyage-level analysis
  # This loads the SQL query
  tar_target(
    name = gridded_data_sql,
    "sql/gridded_data.sql",
    format = "file"
  ),
  # Run the query and save it on BQ
  tar_target(
    name = gridded_data_5_bq,
    run_gfw_query(sql = gridded_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_5_v_20250210", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_bq
                  ungridded_data_bq, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
  ),
  # For robustness check, make another version of this table that is 3x3 degrees
  tar_target(
    name = gridded_data_3_bq,
    run_gfw_query(sql = gridded_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 3,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_3_v_20250210", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_bq
                  ungridded_data_bq, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
  ),
  # For robustness check, make another version of this table that is 7x7 degrees
  tar_target(
    name = gridded_data_7_bq,
    run_gfw_query(sql = gridded_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 7,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_7_v_20250210", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_bq
                  ungridded_data_bq, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
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
                  bq_table_name = "gridded_data_0_5_v_20250210", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_bq
                  ungridded_data_bq, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
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
                  bq_table_name = "gridded_pirate_attacks_0_5_v_20250210", 
                  # Trigger re-run of this if asam_data_processed is run
                  asam_data_processed)
  ),
  # Also make a 5x5 degree version, for making figures
  tar_target(
    name = gridded_pirate_attacks_5_bq,
    run_gfw_query(sql = gridded_pirate_attacks_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_pirate_attacks_5_v_20250210", 
                  # Trigger re-run of this if asam_data_processed is run
                  asam_data_processed)
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
                  bq_table_name = "aggregate_spatial_shipping_activity_v_20250210", 
                  # Trigger re-run of this if timestamp changes for gridded_data_0_5_bq
                  gridded_data_0_5_bq)
  ),
  # Pull gridded shipipng data locally
  tar_target(
    name = aggregate_spatial_shipping_activity,
    pull_gfw_data_locally(bq_table_name = "aggregate_spatial_shipping_activity_v_20250210", 
                  # Trigger re-run of this if timestamp changes for aggregate_spatial_shipping_activity_bq
                  aggregate_spatial_shipping_activity_bq)
  ),
  # Process the total number of attacks that occurred along each route, by trip, over a rolling time window
  tar_target(
    name = total_rolling_route_attacks_per_trip_sql,
    "sql/total_rolling_route_attacks_per_trip.sql",
    format = "file"
  ),
  # Run the query to generate total_rolling_route_attacks_per_trip_sql
  # Save to BigQuery
  # First do 5 degree version
  tar_target(
    name = total_rolling_route_attacks_per_trip_5_degrees_bq,
    run_gfw_query(sql = total_rolling_route_attacks_per_trip_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 5),
                  bq_table_name = "total_rolling_route_attacks_per_trip_5_degrees_v_20250210", 
                  # Trigger re-run of this if timestamp changes for gridded_data_5_bq
                  gridded_data_5_bq,
                  # Trigger re-run of timestamp changes ov voyage_info_bq
                  voyage_info_bq,
                  # Trigger re-run of timestamp changes ov asam_data_processed
                  asam_data_processed, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
  ),
  # Also do 3 degree version
  tar_target(
    name = total_rolling_route_attacks_per_trip_3_degrees_bq,
    run_gfw_query(sql = total_rolling_route_attacks_per_trip_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 3),
                  bq_table_name = "total_rolling_route_attacks_per_trip_3_degrees_v_20250210", 
                  # Trigger re-run of this if timestamp changes for gridded_data_5_bq
                  gridded_data_5_bq,
                  # Trigger re-run of timestamp changes ov voyage_info_bq
                  voyage_info_bq,
                  # Trigger re-run of timestamp changes ov asam_data_processed
                  asam_data_processed, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
  ),
  # Process the average number of attacks that occurred along previous trips for each route, by trip, over a rolling time window
  tar_target(
    name = average_rolling_route_attacks_per_trip_sql,
    "sql/average_rolling_route_attacks_per_trip.sql",
    format = "file"
  ),
  # Run the query to generate average_rolling_route_attacks_per_trip_sql
  # Save to BigQuery
  # First do 5 degree version
  tar_target(
    name = average_rolling_route_attacks_per_trip_5_degrees_bq,
    run_gfw_query(sql = average_rolling_route_attacks_per_trip_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 5),
                  bq_table_name = "average_rolling_route_attacks_per_trip_5_degrees_v_20250210", 
                  # Trigger re-run of this if timestamp changes for gridded_data_5_bq
                  gridded_data_5_bq,
                  # Trigger re-run of timestamp changes ov voyage_info_bq
                  voyage_info_bq,
                  # Trigger re-run of timestamp changes ov asam_data_processed
                  asam_data_processed, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
  ),
  # Also do 3 degree version
  tar_target(
    name = average_rolling_route_attacks_per_trip_3_degrees_bq,
    run_gfw_query(sql = average_rolling_route_attacks_per_trip_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 3),
                  bq_table_name = "average_rolling_route_attacks_per_trip_3_degrees_v_20250210", 
                  # Trigger re-run of this if timestamp changes for gridded_data_5_bq
                  gridded_data_5_bq,
                  # Trigger re-run of timestamp changes ov voyage_info_bq
                  voyage_info_bq,
                  # Trigger re-run of timestamp changes ov asam_data_processed
                  asam_data_processed, 
                  # Trigger re-run of this if timestamp changes for keep_these_trips_bq
                  keep_these_trips_bq)
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
                  bq_table_name = "voyage_data_5_v_20250210", 
                  #Trigger re-run of this if timestamp changes for gridded_data_5_bq
                  gridded_data_5_bq, 
                  #Trigger re-run of this if timestamp changes for gridded_data_3_bq
                  gridded_data_3_bq, 
                  #Trigger re-run of this if timestamp changes for gridded_data_7_bq
                  gridded_data_7_bq,
                  # Trigger re-run of this if timestamp changes for total_rolling_route_attacks_per_trip_5_degrees_bq
                  total_rolling_route_attacks_per_trip_5_degrees_bq,
                  # Trigger re-run of this if timestamp changes for total_rolling_route_attacks_per_trip_3_degrees_bq
                  total_rolling_route_attacks_per_trip_3_degrees_bq,
                  # Trigger re-run of this if timestamp changes for average_rolling_route_attacks_per_trip_5_degrees_bq
                  average_rolling_route_attacks_per_trip_5_degrees_bq,
                  # Trigger re-run of this if timestamp changes for average_rolling_route_attacks_per_trip_3_degrees_bq
                  average_rolling_route_attacks_per_trip_3_degrees_bq)
  ),
  # Pull voyage-level data from BigQuery to local environment
  # Don't do this for now, since it's ~15GB
  # tar_target(
  #   name = voyage_data,
  #   pull_gfw_data_locally(bq_table_name = "voyage_data_5_v_20250210", 
  #                         # Trigger re-run of this if timestamp changes for voyage_data_bq
  #                         voyage_data_bq)
  # ),
  # Make figure with global map showing attacks shipping activity
  # As well as time series of attack
  tar_target(
    name = map_with_attack_timeseries_figure,
    make_map_with_attack_timeseries_figure(asam_with_hotspots,
                           hotspots,
                           aggregate_spatial_shipping_activity,
                           attack_year_min = 2013,
                           attack_year_max = 2021)),
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
                  bq_table_name = "shipping_activity_by_year_eez_v_20250210", 
                  # Trigger re-run of this if timestamp changes for ungridded_data_bq
                  ungridded_data_bq)
  ),
  # Pull the annual shipping activty by EEZ data locally
  tar_target(
    name = shipping_activity_by_year_eez,
    pull_gfw_data_locally(bq_table_name = "shipping_activity_by_year_eez_v_20250210", 
                  # Trigger re-run of this if timestamp changes for shipping_activity_by_year_eez_bq
                  shipping_activity_by_year_eez_bq)
  ),
  # For each route, find the 3 degree pixels and dates that vessels travel along
  tar_target(
    name = route_pixel_dates_sql,
    "sql/route_pixel_dates.sql",
    format = "file"
  ),
  # Run this query and save the data on BigQuery
  tar_target(
    name = route_pixel_dates_bq,
    run_gfw_query(sql = route_pixel_dates_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 3),
                  bq_table_name = "route_pixel_dates_v_20250210")
  ),
  # For each route, find the pixels that vessels travel through across all time
  tar_target(
    name = route_pixels_all_time_sql,
    "sql/route_pixels_all_time.sql",
    format = "file"
  ),
  # Run this query and save the data on BigQuery
  tar_target(
    name = route_pixels_all_time_bq,
    run_gfw_query(sql = route_pixels_all_time_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 3),
                  bq_table_name = "route_pixels_all_time_v_20250210")
  ),
  tar_target(
    name = gridded_pirate_attacks_3_bq,
    run_gfw_query(sql = gridded_pirate_attacks_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 3,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_pirate_attacks_3_v_20250210")
  ),
  # For all combinations of route and every possible date,
  # determine if an attack happened along the route on that date
  # Define routes as the pixels that voyages passed through across all time
  tar_target(
    name = route_all_time_date_has_attack_sql,
    "sql/route_all_time_date_has_attack.sql",
    format = "file"
  ),
  # Run this query and save the data on BigQuery
  tar_target(
    name = route_all_time_date_has_attack_50p_threshold_bq,
    run_gfw_query(sql = route_all_time_date_has_attack_sql %>%
                    readr::read_file() %>%
                    glue::glue(gridded_attack_table = "gridded_pirate_attacks_3_v_20250210",
                               fraction_route_trips_through_pixel_min_threshold = 0.5),
                  bq_table_name = "route_all_time_date_has_attack_50p_threshold_v_20250210")
  ),
  # Run this query and save the data on BigQuery
  tar_target(
    name = route_all_time_date_has_attack_90p_threshold_bq,
    run_gfw_query(sql = route_all_time_date_has_attack_sql %>%
                    readr::read_file() %>%
                    glue::glue(gridded_attack_table = "gridded_pirate_attacks_3_v_20250210",
                               fraction_route_trips_through_pixel_min_threshold = 0.9),
                  bq_table_name = "route_all_time_date_has_attack_90p_threshold_v_20250210")
  ),
  # For all combinations of route and every possible date,
  # find pixels that voyages passed through before or on each date
  #  so if a route hadn't actually been traversed before a particular date, there won't be any rows for that route-date
  tar_target(
    name = route_prior_date_pixels_sql,
    "sql/route_prior_date_pixels.sql",
    format = "file"
  ),
  # Run this query and save the data on BigQuery
  tar_target(
    name = route_prior_date_pixels_bq,
    run_gfw_query(sql = route_prior_date_pixels_sql %>%
                    readr::read_file(),
                  bq_table_name = "route_prior_date_pixels_v_20250210")
  ),
  # For all combinations of route and every possible date,
  # determine if an attack happened along the route on that date
  # using routes as the pixels that voyages passed through before or on each date
  tar_target(
    name = route_prior_date_has_attack_sql,
    "sql/route_prior_date_has_attack.sql",
    format = "file"
  ),
  # Run this query and save the data on BigQuery
  tar_target(
    name = route_prior_date_has_attack_bq,
    run_gfw_query(sql = route_prior_date_has_attack_sql %>%
                    readr::read_file() %>%
                    glue::glue(gridded_attack_table = "gridded_pirate_attacks_3_v_20250210"),
                  bq_table_name = "route_prior_date_has_attack_v_20250210")
  )
)
