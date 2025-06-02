# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
# Set target options:
tar_option_set(
  # Load necessary packages
  packages = c("sf", "ggplot2")
)
# Source all necessary functions
tar_source("r/targets_functions.R")

# Set the data directory that is simlink'ed to Box
# Box is where we'll store all data - including raw, processed, output, and the targets data store
# For Gavin, I ran ln -s "/Users/gmcdonald/Library/CloudStorage/Box-Box/piracy-data" "/Users/gmcdonald/github/piracy-shipping/data"
box_directory <- here::here("data")
# Now set the targets store to a _targets folder in this Box directory
targets::tar_config_set(store = file.path(box_directory, "_targets"))

# Set up billing and project info for BigQuery
# Not that this requires authentication, so not all users will be able to do this
bq_billing_project <- "mex-fisheries" # JC's UMiami billing project
bq_data_project <- "emlab-gcp" # Where the piracy project lives
bq_dataset <- "piracy" # The dataset name for this project

# Set ggplot theme for all plots
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 7))


# Note: Any target name that includes _bq at the end creates a table on BigQuery
# in the emlab-gcp.piracy dataset
list(
  # Set version of model run
  tar_target(
    name = model_version,
    "20250521"
  ),
  # Set study period range
  # We will only pull GFW data for this time range
  # These should be a character format YYYY-MM-DD
  tar_target(
    name = study_period_starting_date,
    "2012-01-01"
  ),
  tar_target(
    name = study_period_ending_date,
    "2023-12-31"
  ),
  # Load ASAM data that was verified by JC's students
  tar_file_read(
    name = asam_data,
    command = here::here("data/processed/clean_asam_data.gpkg"),
    read = sf::read_sf(!!.x) |>
      # Standard is to have geometry column called geom
      dplyr::rename(geometry = geom) |>
      # Ensure date of occurrance is in UTC, not local time (see: https://github.com/renatomolinah/piracy-shipping/pull/9)
      dplyr::mutate(dateofocc = lubridate::with_tz(dateofocc, tzone = "UTC")) |>
      # Also make clean mdy date column
      dplyr::mutate(date = format(dateofocc, "%m/%d/%Y") |> lubridate::mdy()) |>
      # Create lon and lat columns: https://stackoverflow.com/a/61745776
      # We'll need these for BQ
      {
        \(.)
          dplyr::mutate(
            .,
            lon = sf::st_coordinates(.)[, 1],
            lat = sf::st_coordinates(.)[, 2]
          )
      }() |>
      # Filter to just attacks within our study period
      dplyr::filter(date >= lubridate::ymd(study_period_starting_date)) |>
      dplyr::filter(date <= lubridate::ymd(study_period_ending_date))
  ),
  # Upload ASAM data to BQ
  tar_target(
    name = asam_data_bq,
    upload_df_to_bq(
      df_to_upload = asam_data |>
        # Don't need geometry column anymore
        sf::st_drop_geometry(),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("asam_data_v_", model_version)
    )
  ),
  # Create hotspot cluster bounding boxes, using attacks for a given year range
  tar_target(
    name = hotspots,
    generate_hotspot_boundaries(
      asam_data,
      year_min = 2012,
      year_max = 2021
    )
  ),
  # Using hotspot cluster bounding boxes, assign hotspot to each asam attack
  tar_target(
    name = asam_with_hotspots,
    generate_asam_with_hotspots(asam_data, hotspots)
  ),
  # Create SQL code for hotspot cluster bounding boxes
  # This will go into the gridded SQL queries
  tar_target(
    name = hotspots_sql,
    generate_hotspot_sql(hotspots)
  ),
  # Process wind data to 5x5 degree grids
  tar_file_read(
    name = wind_data,
    command = here::here("data/raw/era5_wind/era5_monthly_average_wind.nc"),
    read = process_wind_data(!!.x, pixel_size = 5)
  ),
  # Upload wind data to BQ
  tar_target(
    name = wind_data_bq,
    upload_df_to_bq(
      df_to_upload = wind_data |>
        # Don't need geometry column anymore
        sf::st_drop_geometry(),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("wind_data_5_v_", model_version)
    )
  ),
  # Process fuel data, downloaded from Bunker Index
  # This interpolates missing dates using price from previous date
  tar_file_read(
    name = fuel_data,
    command = here::here("data/raw/bunker_index/bix_world_ifo_380_index.csv"),
    read = process_fuel_data(!!.x)
  ),
  # Upload fuel price data to BQ
  tar_target(
    fuel_data_bq,
    upload_df_to_bq(
      df_to_upload = fuel_data,
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("fuel_prices_v_", model_version)
    )
  ),
  # Generate vessel characteristic info and save on BigQuery
  tar_file_read(
    name = vessel_info_bq,
    command = here::here("sql/vessel_info.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("vessel_info_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Get voyage trip info in BigQuery
  tar_file_read(
    name = voyage_info_bq,
    command = here::here("sql/voyage_info.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          vessel_info_table = vessel_info_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("voyage_info_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Process ungridded, raw AIS messages
  # This ungridded dataset will serve as the basis of all gridded and voyage-level datasets
  tar_file_read(
    name = ungridded_data_bq,
    command = here::here("sql/ungridded_data.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          vessel_info_table = vessel_info_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("ungridded_data_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Keep only these trips
  # Apply some rules-of-thumb filters to remove potentially erroneous trips
  # i.e., trips > 60 days, distance greater than earth's circumfrence, from port = to port,
  # distance greater than4x average trip distance
  tar_file_read(
    name = keep_these_trips_bq,
    command = here::here("sql/keep_these_trips.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          ungridded_data_table = ungridded_data_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("keep_these_trips_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Aggregate ungridded data to level of trip departure date and 5x5 degree pixels
  # This will eventually further be aggregated to the voyage-level for the voyage-level analysis
  tar_file_read(
    name = gridded_data_5_bq,
    command = here::here("sql/gridded_data.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 5,
          hotspots = hotspots_sql,
          ungridded_data_table = ungridded_data_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId,
          vessel_info_table = vessel_info_bq$tableReference$tableId,
          wind_table = wind_data_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("gridded_data_5_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # For robustness check, make another version of this table that is 3x3 degrees
  tar_file_read(
    name = gridded_data_3_bq,
    command = here::here("sql/gridded_data.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 3,
          hotspots = hotspots_sql,
          ungridded_data_table = ungridded_data_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId,
          vessel_info_table = vessel_info_bq$tableReference$tableId,
          wind_table = wind_data_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("gridded_data_3_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # For robustness check, make another version of this table that is 7x7 degrees
  tar_file_read(
    name = gridded_data_7_bq,
    command = here::here("sql/gridded_data.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 7,
          hotspots = hotspots_sql,
          ungridded_data_table = ungridded_data_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId,
          vessel_info_table = vessel_info_bq$tableReference$tableId,
          wind_table = wind_data_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("gridded_data_7_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Aggregate ungridded data to level of shipping activity date and 0.5x0.5 degree pixels
  # This will be the basis of the grid-level analysis
  tar_file_read(
    name = gridded_data_0_5_bq,
    command = here::here("sql/gridded_data_0_5.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 0.5,
          hotspots = hotspots_sql,
          ungridded_data_table = ungridded_data_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId,
          vessel_info_table = vessel_info_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("gridded_data_0_5_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Generate some gridded pirate attack data, for the grid-level analysis
  tar_file_read(
    name = gridded_pirate_attacks_0_5_bq,
    command = here::here("sql/gridded_pirate_attacks.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 0.5,
          hotspots = hotspots_sql,
          asam_data_table = asam_data_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("gridded_pirate_attacks_0_5_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Also make a 5x5 degree version, for making figures
  tar_file_read(
    name = gridded_pirate_attacks_5_bq,
    command = here::here("sql/gridded_pirate_attacks.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 5,
          hotspots = hotspots_sql,
          asam_data_table = asam_data_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("gridded_pirate_attacks_5_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Here we summarize aggregate spatial shipping activity at 0.5x0.5 degrees, for making a global map
  tar_file_read(
    name = aggregate_spatial_shipping_activity_bq,
    command = here::here("sql/aggregate_spatial_shipping_activity.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          gridded_data_table = gridded_data_0_5_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "aggregate_spatial_shipping_activity_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Pull gridded shipipng data locally
  tar_target(
    name = aggregate_spatial_shipping_activity,
    pull_gfw_data_locally(
      bq_table_name = aggregate_spatial_shipping_activity_bq$tableReference$tableId,
      bq_billing_project = bq_billing_project
    ) |>
      save_as_csv(here::here(
        paste0(
          "data/processed/aggregate_spatial_shipping_activity_",
          model_version,
          ".csv"
        )
      ))
  ),
  # Process the total number of attacks that occurred along each route, by trip, over a rolling time window
  # Run the query to generate total_rolling_route_attacks_per_trip_sql
  # First do 5 degree version
  tar_file_read(
    name = total_rolling_route_attacks_per_trip_5_degrees_bq,
    command = here::here("sql/total_rolling_route_attacks_per_trip.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 5,
          gridded_data_table = gridded_data_5_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "total_rolling_route_attacks_per_trip_5_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Also do 3 degree version
  tar_file_read(
    name = total_rolling_route_attacks_per_trip_3_degrees_bq,
    command = here::here("sql/total_rolling_route_attacks_per_trip.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 3,
          gridded_data_table = gridded_data_3_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "total_rolling_route_attacks_per_trip_3_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Process the average number of attacks that occurred along previous trips for each route, by trip, over a rolling time window
  # Run the query to generate average_rolling_route_attacks_per_trip_sql
  tar_file_read(
    name = average_rolling_route_attacks_per_trip_5_degrees_bq,
    command = here::here("sql/average_rolling_route_attacks_per_trip.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 5,
          gridded_data_table = gridded_data_5_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "average_rolling_route_attacks_per_trip_5_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Also do 3 degree version
  tar_file_read(
    name = average_rolling_route_attacks_per_trip_3_degrees_bq,
    command = here::here("sql/average_rolling_route_attacks_per_trip.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 3,
          gridded_data_table = gridded_data_3_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          asam_data_table = asam_data_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "average_rolling_route_attacks_per_trip_3_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Process voyage-level data, which aggregates gridded data
  # Run query to generate voyage-level data and save on BigQuery
  tar_file_read(
    name = voyage_data_bq,
    command = here::here("sql/voyage_data.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          vessel_info_table = vessel_info_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
          fuel_price_table = fuel_data_bq$tableReference$tableId,
          gridded_data_5_table = gridded_data_5_bq$tableReference$tableId,
          gridded_data_3_table = gridded_data_3_bq$tableReference$tableId,
          gridded_data_7_table = gridded_data_7_bq$tableReference$tableId,
          total_rolling_route_attacks_per_trip_5_table = total_rolling_route_attacks_per_trip_5_degrees_bq$tableReference$tableId,
          total_rolling_route_attacks_per_trip_3_table = total_rolling_route_attacks_per_trip_3_degrees_bq$tableReference$tableId,
          average_rolling_route_attacks_per_trip_5_table = average_rolling_route_attacks_per_trip_5_degrees_bq$tableReference$tableId,
          average_rolling_route_attacks_per_trip_3_table = average_rolling_route_attacks_per_trip_3_degrees_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "voyage_data_5_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Get unique years contained in voyage_data, so we can download them one at a time
  tar_target(
    name = voyage_data_years,
    get_years_of_voyage_data(
      bq_table_name = voyage_data_bq$tableReference$tableId,
      bq_billing_project = bq_billing_project
    )
  ),
  # Now download voyage data, one year at a time
  tar_target(
    name = voyage_data_files,
    voyage_data_years |>
      download_year_of_voyage_data(
        bq_table_name = voyage_data_bq$tableReference$tableId,
        bq_billing_project = bq_billing_project
      ),
    pattern = map(voyage_data_years)
  ),
  # Make figure with global map showing attacks shipping activity
  # As well as time series of attack
  tar_file_read(
    name = map_with_attack_timeseries_figure,
    command = aggregate_spatial_shipping_activity,
    make_map_with_attack_timeseries_figure(
      asam_with_hotspots,
      hotspots,
      readr::read_csv(!!.x),
      attack_year_min = 2012,
      attack_year_max = 2023
    )
  ),
  # Summarize annual shipping activity by EEZ
  tar_file_read(
    name = shipping_activity_by_year_eez_bq,
    command = here::here("sql/shipping_activity_by_year_eez.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          ungridded_data_table = ungridded_data_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("shipping_activity_by_year_eez_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # Pull the annual shipping activty by EEZ data locally, save as CSV
  tar_target(
    name = shipping_activity_by_year_eez_file,
    pull_gfw_data_locally(
      bq_table_name = shipping_activity_by_year_eez_bq$tableReference$tableId,
      bq_billing_project = bq_billing_project
    ) |>
      save_as_csv(here::here(
        paste0(
          "data/processed/shipping_activity_by_year_eez_",
          model_version,
          ".csv"
        )
      )),
    format = "file"
  ),
  # For each route, find the 3 degree pixels and dates that vessels travel along
  tar_file_read(
    name = route_pixel_dates_bq,
    command = here::here("sql/route_pixel_dates.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 3,
          gridded_data_table = gridded_data_0_5_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("route_pixel_dates_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  # For each route, find the pixels that vessels travel through across all time
  tar_file_read(
    name = route_pixels_all_time_bq,
    command = here::here("sql/route_pixels_all_time.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 3,
          gridded_data_table = gridded_data_0_5_bq$tableReference$tableId,
          keep_these_trips_table = keep_these_trips_bq$tableReference$tableId,
          voyage_info_table = voyage_info_bq$tableReference$tableId,
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("route_pixels_all_time_v_", model_version),
      bq_billing_project = bq_billing_project
    ),
  ),
  tar_file_read(
    name = gridded_pirate_attacks_3_bq,
    command = here::here("sql/gridded_pirate_attacks.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          pixel_size = 3,
          hotspots = hotspots_sql,
          asam_data_table = asam_data_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0("gridded_pirate_attacks_3_v_", model_version),
      bq_billing_project = bq_billing_project
    )
  ),
  # For all combinations of route and every possible date,
  # determine if an attack happened along the route on that date
  # Define routes as the pixels that voyages passed through across all time
  # Use 50% threshold - 50% of voyages need to have passed through pixel for it to be considered on the route
  tar_file_read(
    name = route_all_time_date_has_attack_50p_threshold_bq,
    command = here::here("sql/route_all_time_date_has_attack.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          route_pixels_all_time_table = route_pixels_all_time_bq$tableReference$tableId,
          gridded_attack_table = gridded_pirate_attacks_3_bq$tableReference$tableId,
          fraction_route_trips_through_pixel_min_threshold = 0.5,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "route_all_time_date_has_attack_50p_threshold_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    )
  ),
  # Use 90% threshold - 90% of voyages need to have passed through pixel for it to be considered on the route
  tar_file_read(
    name = route_all_time_date_has_attack_90p_threshold_bq,
    command = here::here("sql/route_all_time_date_has_attack.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          route_pixels_all_time_table = route_pixels_all_time_bq$tableReference$tableId,
          gridded_attack_table = gridded_pirate_attacks_3_bq$tableReference$tableId,
          fraction_route_trips_through_pixel_min_threshold = 0.9,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "route_all_time_date_has_attack_90p_threshold_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # For all combinations of route and every possible date,
  # find pixels that voyages passed through before or on each date
  # so if a route hadn't actually been traversed before a particular date, there won't be any rows for that route-date
  tar_file_read(
    name = route_prior_date_pixels_bq,
    command = here::here("sql/route_prior_date_pixels.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          route_pixel_dates_table = route_pixel_dates_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "route_prior_date_pixels_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    ),
  ),
  # For all combinations of route and every possible date,
  # determine if an attack happened along the route on that date
  # using routes as the pixels that voyages passed through before or on each date
  tar_file_read(
    name = route_prior_date_has_attack_bq,
    command = here::here("sql/route_prior_date_has_attack.sql"),
    read = run_gfw_query(
      sql = readr::read_file(!!.x) |>
        stringr::str_glue(
          gridded_attack_table = gridded_pirate_attacks_3_bq$tableReference$tableId,
          route_pixel_dates_table = route_pixel_dates_bq$tableReference$tableId,
          study_period_starting_date = study_period_starting_date,
          study_period_ending_date = study_period_ending_date
        ),
      bq_data_project = bq_data_project,
      bq_dataset = bq_dataset,
      bq_table_name = paste0(
        "route_prior_date_has_attack_v_",
        model_version
      ),
      bq_billing_project = bq_billing_project
    )
  )
)
