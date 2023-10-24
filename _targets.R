# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  # Load necessary packages
  packages = c("tidyverse")
)
# Source all necessary functions
tar_source("r/functions.R")

# Set data directory on emLab's Google Shared Drive where targets interm rds objects will live
# This path will need to be modified by each user, since everyone has a different path to this directory
data_directory <- "/Users/gmcdonald/Library/CloudStorage/GoogleDrive-gmcdonald@ucsb.edu/Shared\ drives/emlab/Projects/current-projects/piracy/data"

# Set directory where target objects will be saved
tar_config_set(store = glue::glue("{data_directory}/_targets"))

# Set up billing and project info for BigQuery
# Not that this requires authentication, so not all users will be able to do this
billing_project <- "emlab-gcp" # emLab's billing project
bq_dataset <- "piracy" # The dataset name for this project
query_path <- "sql" # Define directory where SQL queries live


# Replace the target list below with your own:
list(
  # Process ASAM piracy data
  tar_target(
    name = asam_file,
    glue::glue("{data_directory}/raw//asam_data_download/ASAM_events.shp"),
    format = "file"
  ),
  tar_target(
    name = asam_data,
    sf::st_read(asam_file)
  ),
  tar_target(
    name = asam_data_processed,
    process_asam_data(asam_data)
  ),
  # Create hotspot cluster bounding boxes
  tar_target(
    name = hotspots,
    generate_hotspot_boundaries(asam_data_processed,
                                year_min = 2010,
                                years_to_include = 12)
  ),
  # Create SQL code for hotspot cluster bounding boxes
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
                      table_name = "wind_data_5")
  ),
  # Process fuel data
  tar_target(
    name = fuel_file,
    glue::glue("{data_directory}/raw/bix_world_ifo_380_index.csv"),
    format = "file"
  ),
  tar_target(
    name = fuel_data,
    process_fuel_data(fuel_file)
  ),
  tar_target(
    # Process ungridded, raw AIS messages
    name = ungridded_data_sql,
    "sql/ungridded_data.sql",
    format = "file"
  ),
  tar_target(
    name = ungridded_data,
    run_gfw_query(sql = ungridded_data_sql,
                  bq_table_name = "ungridded_data", 
                  download_data = FALSE)
  ),
  # Process gridded data, which aggregates ungridded data to different pixel resolutions
  tar_target(
    name = gridded_data_sql,
    "sql/gridded_data.sql",
    format = "file"
  ),
  # Here we make a 0.5x0.5 gridded dataset
  # We will wait to run this until we're sure
  tar_target(
    name = gridded_data_0_5,
    run_gfw_query(sql = gridded_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 0.5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_0_5", 
                  download_data = FALSE)
  ),
  # Here we summarize aggregate spatial shipping activity
  # We will wait to run this until we're sure
  tar_target(
    name = aggregate_spatial_shipping_activity_sql,
    "sql/aggregate_spatial_shipping_activity.sql",
    format = "file"
  ),
  tar_target(
    name = aggregate_spatial_shipping_activity,
    run_gfw_query(sql = aggregate_spatial_shipping_activity_sql %>%
                    readr::read_file(),
                  bq_table_name = "aggregate_spatial_shipping_activity", 
                  download_data = TRUE)
  ),
  # Here we make a 5x5 degree gridded dataset
  tar_target(
    name = gridded_data_5,
    run_gfw_query(sql = gridded_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_5", 
                  download_data = FALSE)
  ),
  # Process the average number of attacks that occurred along each route
  # by trip
  tar_target(
    name = average_route_attacks_per_route_and_trip_sql,
    "sql/average_route_attacks_per_route_and_trip.sql",
    format = "file"
  ),
  tar_target(
    name = average_route_attacks_per_route_and_trip,
    run_gfw_query(sql = average_route_attacks_per_route_and_trip_sql,
                  bq_table_name = "average_route_attacks_per_route_and_trip", 
                  download_data = FALSE)
  ),
  # Process voyage-level data, which aggregates gridded data
  tar_target(
    name = voyage_data_sql,
    "sql/voyage_data.sql",
    format = "file"
  ),
  tar_target(
    name = voyage_data,
    run_gfw_query(sql = voyage_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(gridded_data_table_location = "gridded_data_5",
                               wind_table_location = "wind_data_5"),
                  bq_table_name = "voyage_data_5", 
                  download_data = TRUE)
  ),
  tar_target(
    name = global_map_figure,
    make_global_map_figure(asam_data_processed,
                           hotspots,
                           aggregate_spatial_shipping_activity,
                           map_projection = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  )
)
