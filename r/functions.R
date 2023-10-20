# This function pulls the necessary GFW data and optionally saves it locally as a CSV
# This requires special permissions, and is also very expensive to run, so will not be done often
# You can add any additional arguments for glue to make substitutions to the query, as necessary
run_gfw_query <- function(query, bq_table_name, download_data = FALSE, ...){
  
  # Load query
  sql <- glue::glue("{query}") %>%
    readr::read_file() %>%
    # Then make any substitutions necessary
    glue::glue(...)
  
  
  # Run query and save on BQ. We don't pull this locally yet.
  bigrquery::bq_project_query(billing_project,
                              sql,
                              destination_table = bigrquery::bq_table(project = billing_project,
                                                                      table = bq_table_name,
                                                                      dataset = bq_dataset),
                              use_legacy_sql = FALSE,
                              allowLargeResults = TRUE,
                              write_disposition = "WRITE_TRUNCATE")
  
  # Now we download data locally, if desired
  if(download_data) return(bigrquery::bq_project_query(billing_project, 
                                                       glue::glue("SELECT * FROM emlab-gcp.{bq_dataset}.{bq_table_name}")) %>%
                             bigrquery::bq_table_download(n_max = Inf))
  
  # If data is not to be pulled locally, simply return current system time
  if(!download_data) return(Sys.time())
}

# Code to process ASAM piracy data, and upload it to BigQuery
# Download from this website on October 20, 2023: https://msi.nga.mil/Piracy
# "Anti-shipping Activity Messages (ASAM) include the locations and descriptive accounts of specific hostile acts against ships and mariners. 
# The reports may be useful for recognition, prevention and avoidance of potential hostile activity."

process_asam_data <- function(asam_data){
  processed_asam_data <- asam_data %>%
    dplyr::mutate(date = lubridate::as_date(dateofocc)) %>%
    dplyr::select(-dateofocc) %>%
    dplyr::group_by(reference) %>%
    # Remove duplicates
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    # Only include pirate assaults
    dplyr::filter(hostilit_D == "Pirate Assault") %>%
    # Only include attacks in 2022 or before
    dplyr::filter(lubridate::year(date) <= 2022) %>%
    # Extract lat and lon columns
    dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                  lat = sf::st_coordinates(.)[,2]) %>%
    # Only extract necessary columns
    dplyr::select(asam_reference = reference,
                  date,
                  lon,
                  lat) %>%
    sf::st_set_geometry(NULL)
  
  # Upload table to BQ
  bigrquery::bq_table(project = billing_project,
                      table = "asam_data",
                      dataset = bq_dataset) %>% 
    bigrquery::bq_table_upload(values = processed_asam_data,
                               fields = bigrquery::as_bq_fields(processed_asam_data),
                               write_disposition = "WRITE_TRUNCATE")
  
  return(Sys.time())
}

# Make global map, which will include ASAM attacks and eventually shipping activity
make_global_map_figure <- function(asam_data_processed){
  # Set projection for mapping
  # Use Mollweide
  map_projection <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  
  # Load world land
  world_land <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
    sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
    sf::st_transform(map_projection)
  
  # Load ASAM data
  asam_data_processed_sf <- asam_data_processed %>%
    sf::st_as_sf(coords = c("lon","lat"),
                 crs = sf::st_crs(4326)) %>%
    sf::st_transform(map_projection)
  
  # Create bounding box of world, to use as outline in the projected maps
  # vectors of latitudes and longitudes that go once around the 
  # globe in 1-degree steps
  lats <- c(90:-90, -90:90, 90)
  longs <- c(rep(c(180, -180), each = 181), 180)
  
  world_bbox_sf <- 
    list(cbind(longs, lats)) %>%
    sf::st_polygon() %>%
    sf::st_sfc( # create sf geometry list column
      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    ) %>% 
    sf::st_sf() %>%
    sf::st_transform(map_projection)
  
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_bbox_sf,
                     fill = NA,
                     color = "black") +
    ggplot2::geom_sf(data = world_land,
                     color = "darkgrey",
                     fill = "darkgrey") +
    ggplot2::geom_sf(data = asam_data_processed_sf,
                     size = 0.01) + 
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank())
  
  ggplot2::ggsave(filename = "figures/map.png",
                  plot,
                  height = 4,
                  width = 7,
                  dpi = 300)
  
  return(plot)
}

process_wind_data <- function(wind_file,
                              pixel_size,
                              table_name){
  # ERA5 monthly averaged data downloaded from Copernicus
  # We get the u10 and v10 wind components, which come at monthly 0.25x0.25 degree resolution
  # https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=overview
  # Load u component
  wind_u <- raster::stack(wind_file,
                          varname = "u10")%>%
    # Need to rotate from 0-360 to -180-180, since original NC is provided in 0-360
    raster::rotate() %>%
    stars::st_as_stars() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(component = "u_ms")
  
  # Load v component
  wind_v <- raster::stack(wind_file,
                          varname = "v10")%>%
    # Need to rotate from 0-360 to -180-180, since original NC is provided in 0-360
    raster::rotate() %>%
    stars::st_as_stars() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(component = "v_ms")
  
  # Put it all together into tidy tibble
  all_wind_data <- bind_rows(wind_u, wind_v) %>%
    # Take lower left corner for lat and lon
    mutate(bbox = map(geometry,~sf::st_bbox(.)),
           lon = map_dbl(bbox,~.$xmin),
           lat = map_dbl(bbox,~.$ymin)) %>%
    dplyr::select(-bbox) %>%
    sf::st_set_geometry(NULL)%>% 
    tidyr::pivot_longer(cols = -c(lon,lat,component)) %>%
    mutate(date = lubridate::ymd(name)) %>%
    dplyr::select(-name) %>%
    pivot_wider(names_from = "component",
                values_from = "value")
  
  wind_data_aggregated <- all_wind_data %>%
    dplyr::mutate(lat_bin = floor(lat/pixel_size) * pixel_size,
                  lon_bin = floor(lon/pixel_size) * pixel_size) %>%
    # https://help.marine.copernicus.eu/en/articles/5487266-how-to-average-winds#
    # The vector mean wind speed is obtained by first averaging u and v over the period of interest and then calculating the wind_speed from the averaged u and v.
    # The vector average should be used when the wind direction matters as well. For example in the transport of particles by the wind, because winds in opposite directions cancel out in terms of transport:
    dplyr::group_by(date,lat_bin,lon_bin) %>%
    summarize(u_ms = mean(u_ms,na.rm=TRUE),
              v_ms = mean(v_ms,na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::mutate(wind_speed_ms =sqrt(u_ms^2 + v_ms^2),
                  # https://disc.gsfc.nasa.gov/information/data-in-action?title=Derive%20Wind%20Speed%20and%20Direction%20With%20MERRA-2%20Wind%20Components#:~:text=The%20U%20wind%20component%20is,wind%20comes%20from%20the%20north.
                  wind_direction_degrees = atan(v_ms/u_ms)) %>%
    dplyr::ungroup()
  
  # Upload table to BQ
  bigrquery::bq_table(project = billing_project,
                      table = table_name,
                      dataset = bq_dataset) %>% 
    bigrquery::bq_table_upload(values = wind_data_aggregated,
                               fields = bigrquery::as_bq_fields(wind_data_aggregated),
                               write_disposition = "WRITE_TRUNCATE")
  
  return(Sys.time())
}
# The BIX World IFO 380 is the calculated daily average for IFO 380 worldwide, 
# covering all ports with IFO 380 prices listed in the Bunker Index prices section. Prices are in US$ per metric tonne.
# Downloaded from https://bunkerindex.com/prices/bix-world.php
process_fuel_data <- function(fuel_file){
  # Load fuel price data
  fuel_price_data <- fuel_file %>%
    read_csv() %>%
    dplyr::select(date = Date,
                  price_usd_mt = Price) %>%
    dplyr::mutate(date = lubridate::mdy(date))
  
  # All dates
  date_range <- tibble(date = seq(min(fuel_price_data$date),
                    max(fuel_price_data$date), 
                    by = 'day'))
  
  # Fill missing dates with last value
  interpolated_fuel_price_data <- fuel_price_data%>%
    right_join(date_range) %>%
    arrange(date) %>% 
    tidyr::fill(price_usd_mt,.direction ="down")
  
  # Upload table to BQ
  bigrquery::bq_table(project = billing_project,
                      table = "fuel_prices",
                      dataset = bq_dataset) %>% 
    bigrquery::bq_table_upload(values = interpolated_fuel_price_data,
                               fields = bigrquery::as_bq_fields(interpolated_fuel_price_data),
                               write_disposition = "WRITE_TRUNCATE")
  
  return(Sys.time())
}
