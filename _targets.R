# Load packages required to define the pipeline:
library(targets)

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

# Set ggplot theme for all plots
ggplot2::theme_set(ggplot2::theme_bw() +
            ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0,vjust=0.6),
                  strip.background = ggplot2::element_blank(),
                  strip.text.y = ggplot2::element_text(angle=0),
                  strip.text.y.right = ggplot2::element_text(angle=0),
                  strip.text.y.left = ggplot2::element_text(angle=0),
                  panel.background = ggplot2::element_blank(),
                  panel.border = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank()))

# Replace the target list below with your own:
list(
  # Process ASAM piracy attack data
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
    process_asam_data(asam_data)
  ),
  # Add clusters to attack data
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
  # Process ungridded, raw AIS messages
  # This ungridded dataset will serve as the basis of all gridded and voyage-level datasets
  tar_target(
    name = ungridded_data_sql,
    "sql/ungridded_data.sql",
    format = "file"
  ),
  tar_target(
    name = ungridded_data,
    run_gfw_query(sql = ungridded_data_sql %>%
                    readr::read_file(),
                  bq_table_name = "ungridded_data", 
                  download_data = FALSE)
  ),
  # Process gridded data, which aggregates ungridded data to different pixel resolutions
  # This loads the general SQL query, which can then be modified for different pixel sizes and hotspots
  tar_target(
    name = gridded_data_sql,
    "sql/gridded_data.sql",
    format = "file"
  ),
  # Here we make a 0.5x0.5 gridded dataset. This will serve as the basis of the grid-level analysis
  tar_target(
    name = gridded_data_0_5,
    run_gfw_query(sql = gridded_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 0.5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_0_5", 
                  download_data = FALSE,
                  # Trigger re-run of this if timestamp for when ungridded_data was generated changes
                  ungridded_data)
  ),
  # Here we summarize aggregate spatial shipping activity at 0.5x0.5 degrees, for making a global map
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
                  download_data = TRUE,
                  # Trigger re-run of this if timestamp for when gridded_data_0_5 was generated changes
                  gridded_data_0_5)
  ),
  # Here we make a 5x5 degree gridded dataset. This will serve as the basis of the voyages dataset and analysis
  tar_target(
    name = gridded_data_5,
    run_gfw_query(sql = gridded_data_sql %>%
                    readr::read_file() %>%
                    glue::glue(pixel_size = 5,
                               hotspots = hotspots_sql),
                  bq_table_name = "gridded_data_5", 
                  download_data = FALSE,
                  # Trigger re-run of this if timestamp for when ungridded_data was generated changes
                  ungridded_data)
  ),
  # Process the average number of attacks that occurred along each route, by trip
  tar_target(
    name = average_route_attacks_per_route_and_trip_sql,
    "sql/average_route_attacks_per_route_and_trip.sql",
    format = "file"
  ),
  tar_target(
    name = average_route_attacks_per_route_and_trip,
    run_gfw_query(sql = average_route_attacks_per_route_and_trip_sql %>%
                    readr::read_file(),
                  bq_table_name = "average_route_attacks_per_route_and_trip", 
                  download_data = FALSE,
                  # Trigger re-run of this if timestamp for when gridded_data_5 was generated changes
                  gridded_data_5)
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
                  download_data = TRUE,
                  # Trigger re-run of this if timestamp for when gridded_data_5 was generated changes
                  gridded_data_5,
                  # Trigger re-run of this if timestamp for when average_route_attacks_per_route_and_trip was generated changes
                  average_route_attacks_per_route_and_trip)
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
  # Map global maps that show how attacks and hotspots have evolved over time
  tar_target(
    name = map_hotspots_over_time_figure,
    make_hotspot_map_over_time(asam_data_processed,
                           map_projection = global_map_projection)
  ),
  # Make barplot that shows attacks over time, by hotspot
  tar_target(
    name = attack_time_series_figure,
    make_attack_time_series_figure(asam_with_hotspots)
  )
)
