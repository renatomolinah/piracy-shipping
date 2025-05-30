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

# This function pulls GFW data locally from a specific table
pull_gfw_data_locally <- function(bq_table_name, bq_billing_project, ...) {
  bigrquery::bq_project_query(
    bq_billing_project,
    glue::glue("SELECT * FROM emlab-gcp.{bq_dataset}.{bq_table_name}")
  ) |>
    bigrquery::bq_table_download(n_max = Inf)
}

# Code to process ASAM piracy data, and upload it to BigQuery
# Download from this website on October 20, 2023: https://msi.nga.mil/Piracy
# "Anti-shipping Activity Messages (ASAM) include the locations and descriptive accounts of specific hostile acts against ships and mariners.
# The reports may be useful for recognition, prevention and avoidance of potential hostile activity."

# A quick function to table unique values
get_values <- function(data, var) {
  data |>
    pull(var = var) |>
    str_to_upper() |>
    str_squish() |>
    str_trim() |>
    table() |>
    data.frame() |>
    arrange(desc(Freq))
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
    labels = c("A", "B")
  )

  ggplot2::ggsave(
    filename = "figures/map_with_attack_timeseries.png",
    combined_figure,
    height = 7,
    width = 6,
    dpi = 300
  )

  return(plot)
}
process_wind_data <- function(wind_file, pixel_size) {
  # ERA5 monthly averaged data downloaded from Copernicus
  # We get the u10 and v10 wind components, which come at monthly 0.25x0.25 degree resolution
  # https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=overview
  # Load u component
  wind_u <- raster::stack(wind_file, varname = "u10") |>
    # Need to rotate from 0-360 to -180-180, since original NC is provided in 0-360
    raster::rotate() |>
    stars::st_as_stars() |>
    sf::st_as_sf() |>
    dplyr::mutate(component = "u_ms")

  # Load v component
  wind_v <- raster::stack(wind_file, varname = "v10") |>
    # Need to rotate from 0-360 to -180-180, since original NC is provided in 0-360
    raster::rotate() |>
    stars::st_as_stars() |>
    sf::st_as_sf() |>
    dplyr::mutate(component = "v_ms")

  # Put it all together into tidy tibble
  all_wind_data <- dplyr::bind_rows(wind_u, wind_v) |>
    # Take lower left corner for lat and lon
    dplyr::mutate(
      bbox = purrr::map(geometry, ~ sf::st_bbox(.)),
      lon = purrr::map_dbl(bbox, ~ .$xmin),
      lat = purrr::map_dbl(bbox, ~ .$ymin)
    ) |>
    dplyr::select(-bbox) |>
    sf::st_set_geometry(NULL) |>
    tidyr::pivot_longer(cols = -c(lon, lat, component)) |>
    dplyr::mutate(date = lubridate::ymd(name)) |>
    dplyr::select(-name) |>
    tidyr::pivot_wider(names_from = "component", values_from = "value")

  wind_data_aggregated <- all_wind_data |>
    dplyr::mutate(
      lat_bin = floor(lat / pixel_size) * pixel_size,
      lon_bin = floor(lon / pixel_size) * pixel_size
    ) |>
    # https://help.marine.copernicus.eu/en/articles/5487266-how-to-average-winds#
    # The vector mean wind speed is obtained by first averaging u and v over the period of interest and then calculating the wind_speed from the averaged u and v.
    # The vector average should be used when the wind direction matters as well. For example in the transport of particles by the wind, because winds in opposite directions cancel out in terms of transport:
    dplyr::group_by(date, lat_bin, lon_bin) |>
    dplyr::summarize(
      u_ms = mean(u_ms, na.rm = TRUE),
      v_ms = mean(v_ms, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      wind_speed_ms = sqrt(u_ms^2 + v_ms^2),
      # https://disc.gsfc.nasa.gov/information/data-in-action?title=Derive%20Wind%20Speed%20and%20Direction%20With%20MERRA-2%20Wind%20Components#:~:text=The%20U%20wind%20component%20is,wind%20comes%20from%20the%20north.
      wind_direction_degrees = atan(v_ms / u_ms)
    ) |>
    dplyr::ungroup()
}

#   # Upload table to BQ
#   bigrquery::bq_table(project = billing_project,
#                       table = table_name,
#                       dataset = bq_dataset) |>
#     bigrquery::bq_table_upload(values = wind_data_aggregated,
#                                fields = bigrquery::as_bq_fields(wind_data_aggregated),
#                                write_disposition = "WRITE_TRUNCATE")
#
#   return(Sys.time())
# }
# The BIX World IFO 380 is the calculated daily average for IFO 380 worldwide,
# covering all ports with IFO 380 prices listed in the Bunker Index prices section. Prices are in US$ per metric tonne.
# Downloaded from https://bunkerindex.com/prices/bix-world.php
process_fuel_data <- function(fuel_file) {
  # Load fuel price data
  fuel_price_data <- fuel_file |>
    readr::read_csv() |>
    dplyr::select(date = Date, price_usd_mt = Price) |>
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
