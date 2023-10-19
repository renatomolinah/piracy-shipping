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

# Replace the target list below with your own:
list(
  tar_target(
    # Process ungridded, raw AIS messages
    name = ungridded_data_sql,
    "sql/ungridded_data.sql",
    format = "file"
  ),
  tar_target(
    name = ungridded_data,
    run_gfw_query(query = ungridded_data_sql,
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
  # tar_target(
  #   name = gridded_data_0_5,
  #   run_gfw_query(query = gridded_data_sql,
  #                 bq_table_name = "gridded_data_0_5", 
  #                 download_data = FALSE, 
  #                 pixel_size = 0.5, 
  #                 attack_table_location = "piracy_attacks_0_5")
  # ),
  # Here we make a 5x5 degree gridded dataset
  tar_target(
    name = gridded_data_0_5,
    run_gfw_query(query = gridded_data_sql,
                  bq_table_name = "gridded_data_5", 
                  download_data = FALSE, 
                  pixel_size = 5, 
                  attack_table_location = "piracy_attacks_5")
  ),
  # Process voyage-level data, which aggregates gridded data
  tar_target(
    name = voyage_data_sql,
    "sql/voyage_data.sql",
    format = "file"
  ),
  # Here we make the voyage-level dataset, using the 5x5 degree dataset as the base
  tar_target(
    name = voyage_data,
    run_gfw_query(query = voyage_data_sql,
                  bq_table_name = "voyage_data_5", 
                  download_data = TRUE, 
                  gridded_data_table_location = "gridded_data_5")
  )
)
