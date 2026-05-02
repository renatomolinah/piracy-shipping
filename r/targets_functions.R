# This function pulls the necessary GFW data
# This requires special permissions, and is also very expensive to run, so will not be done often
# You can add any additional arguments for glue to make substitutions to the query, as necessary
run_gfw_query <- function(
  sql,
  bq_data_project,
  bq_dataset,
  bq_table_name,
  bq_billing_project,
  ...
) {
  table <- bigrquery::bq_table(
    project = bq_data_project,
    table = bq_table_name,
    dataset = bq_dataset
  )

  # Run query and save on BQ. We don't pull this locally yet.
  bigrquery::bq_project_query(
    bq_billing_project,
    sql,
    destination_table = table,
    use_legacy_sql = FALSE,
    allowLargeResults = TRUE,
    write_disposition = "WRITE_TRUNCATE"
  )

  # Return metadata, for targets to know that something chabged
  bigrquery::bq_table_meta(table)
}

get_years_of_voyage_data <- function(bq_table_name, bq_billing_project) {
  # Get unique years
  years <- bigrquery::bq_project_query(
    bq_billing_project,
    glue::glue(
      "SELECT DISTINCT(EXTRACT(YEAR FROM departure_date)) year FROM `emlab-gcp.{bq_dataset}.{bq_table_name}`"
    )
  ) |>
    bigrquery::bq_table_download() |>
    dplyr::pull(year) |>
    sort()
}

download_year_of_voyage_data <- function(
  year,
  bq_table_name,
  bq_billing_project
) {
  # Create directory, if it doesn't exist yet
  if (
    !dir.exists(here::here(file.path(
      "data/processed",
      bq_table_name
    )))
  ) {
    dir.create(here::here(file.path(
      "data/processed",
      bq_table_name
    )))
  }

  # If download is already complete, just skip this year
  if (
    file.exists(here::here(file.path(
      "data/processed",
      bq_table_name,
      glue::glue("{bq_table_name}_{year}.parquet")
    )))
  ) {
    return(file.path(
      "data/processed",
      bq_table_name,
      glue::glue("{bq_table_name}_{year}.parquet")
    ))
  }

  bigrquery::bq_project_query(
    bq_billing_project,
    glue::glue(
      "SELECT * FROM emlab-gcp.{bq_dataset}.{bq_table_name} WHERE EXTRACT(year FROM departure_date) = {year}"
    )
  ) |>
    bigrquery::bq_table_download() |>
    save_as_parquet(here::here(file.path(
      "data/processed",
      bq_table_name,
      glue::glue("{bq_table_name}_{year}.parquet")
    )))
}

# This function pulls GFW data locally from a specific table
pull_gfw_data_locally <- function(
  bq_table_name,
  bq_billing_project,
  ...
) {
  bigrquery::bq_project_query(
    bq_billing_project,
    glue::glue("SELECT * FROM emlab-gcp.{bq_dataset}.{bq_table_name}")
  ) |>
    bigrquery::bq_table_download()
}

# Make Figure that has two panels:
# A: global map, which includes ASAM attacks, shipping activity, and hotspots
# B: Time series of ASAM attacks, by hotspots
make_map_with_attack_timeseries_figure <- function(
  asam_with_hotspots,
  hotspots,
  aggregate_spatial_shipping_activity,
  attack_year_min = 2012,
  attack_year_max = 2023
) {
  sf::sf_use_s2(FALSE)

  asam_data_processed_sf <- asam_with_hotspots |>
    dplyr::filter(year >= attack_year_min, year <= attack_year_max)

  # Make coast and coastline sf objects
  coast <- rnaturalearth::ne_countries(returnclass = "sf")
  coastline <- rnaturalearth::ne_coastline(returnclass = "sf")

  # Make ASAM region sf
  # Make ASAM region sf
  asam_regions <- asam::asam_subregions() |>
    dplyr::select(asam_region = REGION) |>
    sf::st_buffer(dist = 0.01) |>
    dplyr::group_by(asam_region) |>
    dplyr::summarize() |>
    dplyr::ungroup()

  # Make hotspots into sf polygons for plotting
  hotspots_sf <- hotspots |>
    dplyr::mutate(
      cluster = dplyr::case_when(
        cluster == "hotspot_southeast_asia" ~ "Southeast Asia",
        cluster == "hotspot_gulf_of_aden" ~ "Gulf of Aden",
        cluster == "hotspot_gulf_of_guinea" ~ "Gulf of Guinea"
      ) |>
        forcats::fct_relevel("Gulf of Guinea")
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      geometry = sf::st_geometry(sf::st_polygon(list(rbind(
        c(lon_min, lat_min),
        c(lon_max, lat_min),
        c(lon_max, lat_max),
        c(lon_min, lat_max),
        c(lon_min, lat_min)
      ))))
    ) |>
    sf::st_as_sf(sf_column_name = "geometry", crs = 4326)

  # First make global map
  map <- ggplot() +
    geom_tile(
      data = aggregate_spatial_shipping_activity,
      aes(x = lon_bin, y = lat_bin, fill = hours)
    ) +
    geom_sf(data = asam_regions, fill = "transparent", color = "white") +
    geom_sf(data = coastline, color = "white") +
    geom_sf(data = coast, fill = "black", color = "black") +
    ggplot2::geom_sf(
      data = hotspots_sf,
      fill = NA,
      linewidth = 1.025,
      aes(color = cluster)
    ) +
    geom_point(
      data = asam_data_processed_sf,
      aes(x = lon, y = lat),
      color = "red",
      size = 0.01
    ) +
    labs(x = "", y = "") +
    scale_x_continuous(expand = c(0, 1)) +
    scale_y_continuous(expand = c(0, 1)) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "black"),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    ggplot2::scale_fill_viridis_c(
      "Shipping hours",
      trans = "pseudo_log",
      breaks = c(0, 100, 10000, 1e6),
      option = "mako"
    ) +
    scale_color_manual(
      values = RColorBrewer::brewer.pal(8, "Dark2")[c(2, 4, 6)]
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        frame.colour = "black",
        ticks.colour = "black",
        barwidth = unit(7, "cm"),
        barheight = unit(0.5, "cm")
      ),
      color = "none"
    ) +
    theme(plot.margin = unit(c(0.25, 0, 0, 0), "cm"))

  # Now make time series of attacks
  timeseries <- asam_data_processed_sf |>
    dplyr::group_by(year, cluster) |>
    dplyr::summarize(number_attacks = dplyr::n_distinct(reference)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      cluster = dplyr::case_when(
        cluster == "hotspot_southeast_asia" ~ "Southeast Asia",
        cluster == "hotspot_gulf_of_aden" ~ "Gulf of Aden",
        cluster == "hotspot_gulf_of_guinea" ~ "Gulf of Guinea",
        TRUE ~ "Rest of world"
      ) |>
        forcats::fct_relevel(c(
          "Gulf of Guinea",
          "Gulf of Aden",
          "Southeast Asia"
        ))
    ) |>
    ggplot2::ggplot(aes(x = year, y = number_attacks, fill = cluster)) +
    ggplot2::geom_bar(
      position = "stack",
      stat = "identity",
      color = "black",
      linewidth = 0.25
    ) +
    ggplot2::scale_fill_manual(
      "Hotspot",
      values = RColorBrewer::brewer.pal(8, "Dark2")[c(2, 4, 6, 8)]
    ) +
    ggplot2::labs(x = "", y = "Number of encounters\n") +
    ggplot2::theme(legend.position = c(0.9, 1.1)) +
    ggplot2::scale_x_continuous(
      breaks = seq(attack_year_min, attack_year_max, 1)
    ) +
    theme(legend.box.background = element_rect(fill = 'white', color = 'white'))

  combined_figure <- cowplot::plot_grid(
    map,
    timeseries,
    ncol = 1,
    rel_heights = c(0.67, 0.33),
    #label_y = 1.1,
    labels = c("a", "b")
  )

  ggplot2::ggsave(
    filename = here::here("figures/map_with_attack_timeseries.png"),
    combined_figure,
    height = 7,
    width = 6,
    dpi = 300
  )

  return(combined_figure)
}

process_wind_data <- function(wind_file, pixel_size) {
  # ERA5 monthly averaged data downloaded from Copernicus, 2012-2024
  # We get the u10 and v10 wind components, which come at monthly 0.25x0.25 degree resolution
  # We use the monthly reanalysis data
  # https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels-monthly-means?tab=download

  # Load u component
  wind_u <- tidync::tidync(wind_file) |>
    tidync::activate("u10") |>
    tidync::hyper_tibble() |>
    dplyr::rename(
      lon = longitude,
      lat = latitude,
      date = valid_time,
      u_ms = u10
    ) |>
    dplyr::mutate(across(c("lon", "lat"), ~ as.numeric(.))) |>
    # Rotate from 0-360 to -180 to 180
    dplyr::mutate(lon = ifelse(lon > 180, lon - 360, lon)) |>
    # Aggregate to our pixel size
    dplyr::mutate(
      lat_bin = floor(lat / pixel_size) * pixel_size,
      lon_bin = floor(lon / pixel_size) * pixel_size
    ) |>
    # https://help.marine.copernicus.eu/en/articles/5487266-how-to-average-winds#
    # The vector mean wind speed is obtained by first averaging u and v over the period of interest and then calculating the wind_speed from the averaged u and v.
    # The vector average should be used when the wind direction matters as well. For example in the transport of particles by the wind, because winds in opposite directions cancel out in terms of transport:
    dplyr::group_by(date, lat_bin, lon_bin) |>
    dplyr::summarize(
      u_ms = mean(u_ms, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  # Load v component
  wind_v <- tidync::tidync(wind_file) |>
    tidync::activate("v10") |>
    tidync::hyper_tibble() |>
    dplyr::rename(
      lon = longitude,
      lat = latitude,
      date = valid_time,
      v_ms = v10
    ) |>
    dplyr::mutate(across(c("lon", "lat"), ~ as.numeric(.))) |>
    # Rotate from 0-360 to -180 to 180
    dplyr::mutate(lon = ifelse(lon > 180, lon - 360, lon)) |>
    # Aggregate to our pixel size
    dplyr::mutate(
      lat_bin = floor(lat / pixel_size) * pixel_size,
      lon_bin = floor(lon / pixel_size) * pixel_size
    ) |>
    # https://help.marine.copernicus.eu/en/articles/5487266-how-to-average-winds#
    # The vector mean wind speed is obtained by first averaging u and v over the period of interest and then calculating the wind_speed from the averaged u and v.
    # The vector average should be used when the wind direction matters as well. For example in the transport of particles by the wind, because winds in opposite directions cancel out in terms of transport:
    dplyr::group_by(date, lat_bin, lon_bin) |>
    dplyr::summarize(
      v_ms = mean(v_ms, na.rm = TRUE)
    ) |>
    dplyr::ungroup()

  # Put it all together into tidy tibble
  wind_u |>
    dplyr::inner_join(wind_v, by = c("lon_bin", "lat_bin", "date")) |>
    dplyr::mutate(
      wind_speed_ms = sqrt(u_ms^2 + v_ms^2),
      # Also use average u and v to calculate wind direction in degrees, where 0 degrees means wind is coming from the north, 90 degrees means wind is coming from the east, etc.
      # https://www.eol.ucar.edu/content/wind-direction-quick-reference
      # https://stackoverflow.com/a/12632531
      wind_direction_degrees = (270 - atan2(v_ms, u_ms) * 180 / pi + 180) %% 360
    ) |>
    dplyr::mutate(date = lubridate::ymd(date))
}

# The BIX World IFO 380 is the calculated daily average for IFO 380 worldwide,
# covering all ports with IFO 380 prices listed in the Bunker Index prices section. Prices are in US$ per metric tonne.
# Downloaded from https://bunkerindex.com/prices/bix-world.php
# These were downloaded by Renato on April 9, 2026
process_fuel_data <- function(fuel_file) {
  # Load fuel price data
  fuel_price_data <- fuel_file |>
    readr::read_csv() |>
    dplyr::mutate(date = lubridate::mdy(date))

  # All dates
  date_range <- tibble::tibble(
    date = seq(min(fuel_price_data$date), max(fuel_price_data$date), by = 'day')
  )

  # Fill missing dates with last value
  interpolated_fuel_price_data <- fuel_price_data |>
    dplyr::right_join(date_range, by = "date") |>
    dplyr::arrange(date) |>
    tidyr::fill(price_usd_mt, .direction = "down")

  return(interpolated_fuel_price_data)
}

# Generate piracy attack hotspot boundaries
# use dbscan to generate clusters, focusing on a specified year range
generate_hotspot_boundaries <- function(asam_data, year_min, year_max) {
  asam_filtered <- asam_data |>
    dplyr::filter(
      lubridate::year(date) >= year_min,
      lubridate::year(date) <= year_max
    )

  # Find DBSCAN clusters for attacks occurring during this range
  dbscan_clusters <- fpc::dbscan(
    asam_data |>
      dplyr::select(lon, lat) |>
      sf::st_drop_geometry(),
    eps = 10,
    MinPts = 150
  )

  asam_with_clusters <- asam_data |>
    dplyr::mutate(cluster_number = dbscan_clusters$cluster) |>
    dplyr::mutate(
      cluster = dplyr::case_when(
        cluster_number == 1 ~ "hotspot_southeast_asia",
        cluster_number == 2 ~ "hotspot_gulf_of_guinea",
        cluster_number == 3 ~ "hotspot_gulf_of_aden",
        TRUE ~ "0"
      )
    )

  # Creat bounding box for each cluster, filter out 0 cluster since those are non-clustered attacks
  # Round this up or down to nearest 5 degrees, to match units of analysis

  purrr::map(unique(asam_with_clusters$cluster), function(x) {
    temp_df <- asam_with_clusters |>
      dplyr::filter(cluster == x)
    data.frame(
      cluster = x,
      lon_min = floor(min(temp_df$lon) / 5) * 5,
      lat_min = floor(min(temp_df$lat) / 5) * 5,
      lon_max = ceiling(max(temp_df$lon) / 5) * 5,
      lat_max = ceiling(max(temp_df$lat) / 5) * 5
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::filter(cluster != "0")
}

assign_hotspot_to_asam <- function(asam_data_processed, hotspots) {
  asam_data_processed |>
    dplyr::cross_join(hotspots) |>
    dplyr::mutate(
      attack_in_hotspot = ifelse(
        lat <= lat_max & lat >= lat_min & lon <= lon_max & lon >= lon_min,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::select(asam_reference, cluster, attack_in_hotspot) |>
    tidyr::pivot_wider(
      names_from = cluster,
      values_from = attack_in_hotspot
    ) |>
    dplyr::mutate(
      rest_of_world = ifelse(
        !hotspot_southeast_asia &
          !hotspot_gulf_of_aden &
          !hotspot_gulf_of_guinea,
        TRUE,
        FALSE
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(
        hotspot_southeast_asia,
        hotspot_gulf_of_aden,
        hotspot_gulf_of_guinea,
        rest_of_world
      ),
      names_to = "cluster",
      values_to = "attack_in_hotspot"
    )
}

# Code to generate SQL for assigning lat/lon to hotspots
generate_hotspot_sql <- function(hotspots) {
  hotspots |>
    dplyr::mutate(
      cluster_filter = glue::glue(
        "(CASE WHEN (lat_bin <= {lat_max} AND lat_bin >= {lat_min} AND lon_bin <= {lon_max} AND lon_bin >= {lon_min}) THEN 1 ELSE 0 END) {cluster}"
      )
    ) |>
    dplyr::pull(cluster_filter) |>
    paste0(collapse = ", ")
}

# Take ASAM data and add hotspot info, based on the hotspots boundaries
generate_asam_with_hotspots <- function(asam_data_processed, hotspots) {
  asam_data_processed |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::cross_join(hotspots) |>
    dplyr::mutate(
      attack_in_hotspot = ifelse(
        lat <= lat_max & lat >= lat_min & lon <= lon_max & lon >= lon_min,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::select(reference, year, lon, lat, cluster, attack_in_hotspot) |>
    tidyr::pivot_wider(
      names_from = cluster,
      values_from = attack_in_hotspot
    ) |>
    dplyr::mutate(
      rest_of_world = ifelse(
        !hotspot_southeast_asia &
          !hotspot_gulf_of_aden &
          !hotspot_gulf_of_guinea,
        TRUE,
        FALSE
      )
    ) |>
    tidyr::pivot_longer(
      cols = c(
        hotspot_southeast_asia,
        hotspot_gulf_of_aden,
        hotspot_gulf_of_guinea,
        rest_of_world
      ),
      names_to = "cluster",
      values_to = "attack_in_hotspot"
    ) |>
    dplyr::filter(attack_in_hotspot) |>
    dplyr::select(-attack_in_hotspot)
}

# Function to upload an arbitrary df to BQ
upload_df_to_bq <- function(
  df_to_upload,
  bq_data_project,
  bq_dataset,
  bq_table_name
) {
  table <- bigrquery::bq_table(
    project = bq_data_project,
    table = bq_table_name,
    dataset = bq_dataset
  )

  bigrquery::bq_table_upload(
    table,
    values = df_to_upload,
    fields = bigrquery::as_bq_fields(df_to_upload),
    write_disposition = "WRITE_TRUNCATE"
  )

  # Return metadata, so targets can keep track of everything
  bigrquery::bq_table_meta(table)
}

save_as_parquet <- function(df, location_to_save) {
  df |>
    arrow::write_parquet(sink = location_to_save)

  return(location_to_save)
}

save_as_csv <- function(df, location_to_save) {
  df |>
    readr::write_csv(file = location_to_save)

  return(location_to_save)
}

# Make Figure that has three panels to help visualize our
# analysis looking at how attacks in the Gulf of Aden relate to
# # shipping activity through the Suez Canal and around the Cape of Good Hope
# A: Monthly attacks in Gulf of Aden
# B: Monthly standard-normalized trips through Suez Canal and around Cape of Good Hope
# C: Monthly percent of trips around Cape of Good Hope
make_suez_canal_cape_good_hope_timeseries_figure <- function(
  asam_with_hotspots,
  asam_data,
  suez_canal_or_cape_good_hope_daily_trips
) {
  # Summarize monthly attacks in Gulf of Aden
  attack_timeseries_gulf_aden_monthly <- asam_with_hotspots |>
    dplyr::filter(cluster == "hotspot_gulf_of_aden") |>
    dplyr::select(reference) |>
    sf::st_drop_geometry() |>
    # Need to get attack date
    dplyr::inner_join(asam_data, by = "reference") |>
    dplyr::select(reference, dateofocc) |>
    dplyr::mutate(
      attack_date = as.Date(dateofocc),
      # Get attack month, by flooring to month and then converting back to date (to get first day of month)
      attack_month = lubridate::floor_date(attack_date, unit = "month") |>
        lubridate::ymd()
    ) |>
    # Now summarize attacks by month
    dplyr::group_by(attack_month) |>
    dplyr::summarize(number_attacks = dplyr::n_distinct(reference)) |>
    dplyr::ungroup()

  # Panel A: Make a barplot of monthly attacks in Gulf of Aden
  attack_trip_timeseries <- attack_timeseries_gulf_aden_monthly |>
    ggplot(aes(
      x = attack_month,
      y = number_attacks
    )) +
    geom_bar(stat = "identity") +
    scale_x_date(
      breaks = scales::breaks_width("1 year"),
      date_labels = "%Y"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Encounter month", y = "Monthly encounters\nin Gulf of Aden", color = "")

  # Now, aggregate number of trips that go through each location by departure month
  suez_canal_or_cape_good_hope_monthly_trips <- suez_canal_or_cape_good_hope_daily_trips |>
    dplyr::mutate(
      departure_month = lubridate::floor_date(departure_date, unit = "month") |>
        lubridate::ymd()
    ) |>
    dplyr::filter(departure_month <= as.Date("2023-9-01")) |>
    dplyr::group_by(departure_month, trip_location_flag) |>
    dplyr::summarize(number_trips = sum(number_trips)) |>
    dplyr::ungroup()

  # Find the mean and sd of number of trips for each route, to use for standard normalization
  trips_mean_and_sd <- suez_canal_or_cape_good_hope_monthly_trips |>
    dplyr::group_by(trip_location_flag) |>
    dplyr::summarize(
      number_trips_mean = mean(number_trips),
      number_trips_sd = sd(number_trips)
    ) |>
    dplyr::ungroup()

  # Now, add means and sds to main df and calculate standard-normalized number of trips for each route
  suez_canal_or_cape_good_hope_monthly_trips_with_normalization <- suez_canal_or_cape_good_hope_monthly_trips |>
    dplyr::left_join(trips_mean_and_sd, by = "trip_location_flag") |>
    dplyr::mutate(
      number_trips_demeaned = number_trips - number_trips_mean,
      number_trips_normalized = number_trips_demeaned / number_trips_sd
    )

  # Panel B: Make a line plot of monthly standard-normalized trips through Suez Canal and around Cape of Good Hope
  trip_timeseries <- suez_canal_or_cape_good_hope_monthly_trips_with_normalization |>
    dplyr::mutate(
      trip_location_flag = ifelse(
        trip_location_flag == "suez_canal",
        "Suez Canal",
        "Cape of Good Hope"
      )
    ) |>
    ggplot(aes(
      x = departure_month,
      y = number_trips_normalized,
      color = trip_location_flag
    )) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(
      breaks = scales::breaks_width("1 year"),
      date_labels = "%Y"
    ) +
    theme_minimal() +
    theme(
      legend.position = c(0.02, 1.15),
      legend.justification = c(0, 1),
      panel.grid.minor = element_blank(),
      legend.box.background = element_rect(fill = 'white', color = 'white'),
      legend.margin = margin(2, 4, 2, 4)
    ) +
    scale_color_brewer(palette = "Set1") +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(
      x = "",
      y = "Standard-normalized\nnumber of trips\nthrough each area",
      color = NULL
    )

  # Panel C: Make a line plot of monthly percent of trips around Cape of Good Hope

  fraction_plot <- suez_canal_or_cape_good_hope_monthly_trips |>
    dplyr::select(departure_month, trip_location_flag, number_trips) |>
    tidyr::pivot_wider(
      names_from = trip_location_flag,
      values_from = number_trips
    ) |>
    dplyr::mutate(
      fraction_trips_around_cape = cape_good_hope /
        (suez_canal + cape_good_hope)
    ) |>
    ggplot(aes(x = departure_month, y = fraction_trips_around_cape)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    scale_x_date(
      breaks = scales::breaks_width("1 year"),
      date_labels = "%Y"
    ) +
    theme_minimal() +
    labs(
      x = "Trip departure month",
      y = "Percent of trips\naround Cape of Good Hope"
    )

  # Now put it all together
  combined_figure <- cowplot::plot_grid(
    attack_trip_timeseries,
    trip_timeseries,
    fraction_plot,
    ncol = 1,
    align = "v",
    axis = "lr",
    labels = c("A", "B", "C")
  )

  ggplot2::ggsave(
    filename = here::here(
      "figures/suez_canal_cape_good_hope_timeseries_figure.png"
    ),
    combined_figure,
    height = 7,
    width = 6,
    dpi = 300
  )

  return(combined_figure)
}

# Process wave height data
process_wave_data <- function(wave_file, pixel_size) {
  # ERA5 post-processed daily statistics on single levels from 1940 to present
  # We get the surface wave height ("Significant height of combined wind waves and swell")
  # Data represent the daily mean value for each location, using the 6-hour frequency data as the basis
  # UTC+00:00 time zone
  # From reanalysis product
  # So these are 'surface wave height' values

  # Load u component
  tidync::tidync(wave_file) |>
    tidync::activate("swh") |>
    tidync::hyper_tibble() |>
    dplyr::rename(
      lon = longitude,
      lat = latitude,
      date = valid_time,
      surface_wave_height_m = swh
    ) |>
    dplyr::mutate(across(c("lon", "lat"), ~ as.numeric(.))) |>
    # Rotate from 0-360 to -180 to 180
    dplyr::mutate(lon = ifelse(lon > 180, lon - 360, lon)) |>
    # Aggregate to our pixel size
    dplyr::mutate(
      lat_bin = floor(lat / pixel_size) * pixel_size,
      lon_bin = floor(lon / pixel_size) * pixel_size
    ) |>
    dplyr::group_by(date, lat_bin, lon_bin) |>
    dplyr::summarize(
      surface_wave_height_m = mean(surface_wave_height_m, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(date = lubridate::ymd(date))
}
