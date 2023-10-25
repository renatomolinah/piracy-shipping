# This function pulls the necessary GFW data and optionally saves it locally as a CSV
# This requires special permissions, and is also very expensive to run, so will not be done often
# You can add any additional arguments for glue to make substitutions to the query, as necessary
run_gfw_query <- function(sql, bq_table_name, download_data = FALSE, ...){
  
  
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
  
  return(processed_asam_data)
}

# Make global map, which will include ASAM attacks and eventually shipping activity
make_global_map_figure <- function(asam_data_processed,
                                   hotspots,
                                   aggregate_spatial_shipping_activity,
                                   map_projection){

  shipping_data_stars <- aggregate_spatial_shipping_activity %>%
    stars::st_as_stars(dims  = c('lon_bin','lat_bin'))%>%
    sf::st_set_crs(4326) %>%
    sf::st_transform(map_projection)
  
  asam_data_processed_sf <- asam_data_processed %>%
    sf::st_as_sf(coords = c("lon","lat"),
                 crs = sf::st_crs(4326)) %>%
    sf::st_transform(map_projection)

  # Load world land
  world_land <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
    sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
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
    stars::geom_stars(data = shipping_data_stars,
                      aes(fill = hours),
                      alpha = 0.5) +
    ggplot2::geom_sf(data = world_land,
                     color = "darkgrey",
                     fill = "darkgrey") +
    ggplot2::geom_sf(data = asam_data_processed_sf,
                     size = 0.01) + 
    ggplot2::geom_sf(data = hotspots  %>%
                       mutate(cluster = case_when(cluster == "hotspot_southeast_asia" ~ "Southeast asia",
                                                  cluster == "hotspot_gulf_of_aden" ~ "Gulf of Aden",
                                                  cluster == "hotspot_gulf_of_guinea" ~ "Gulf of Guinea") %>%
                                fct_relevel("Gulf of Guinea")) %>%
                       dplyr::rowwise() %>%
                       dplyr::mutate(geometry = sf::st_geometry(sf::st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
                       sf::st_as_sf(sf_column_name = "geometry", crs = 4326) %>%
                       sf::st_transform(map_projection),
                     fill = NA,
                     linewidth = 1.025,
                     aes(color = as.factor(cluster))) +
    scale_color_brewer("Hotspot",
                       type ="qual",
                       palette = "Dark2")+ 
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    scale_fill_gradient2("Shipping hours",
                          trans = "log10",
                         labels = scales::comma,
                         low = "white",
                         high = "steelblue4")+
    guides(fill  = guide_legend(order = 1,
                                reverse=TRUE),
           color = guide_legend(order = 2))
  
  ggplot2::ggsave(filename = "figures/map.png",
                  plot,
                  height = 3,
                  width = 7,
                  dpi = 300)
  
  return(plot)
}

make_hotspot_map_over_time <- function(asam_data_processed,
                                       map_projection){
  # For rolling time windows, subset attack data to window,
  # Assign each attack to a cluster, then make the bounding box of the cluster
  hotspots_over_time <- tibble(year_start = seq(2002,2010)) %>%
    dplyr::mutate(cluster_data = purrr::map(year_start,function(year_start){
      generate_asam_with_hotspots(asam_data_processed,
                                  year_min = year_start,
                                  years_to_include = 12) %>%
        dplyr::select(-cluster) %>%
        dplyr::rename(cluster = cluster_number)})) %>%
    dplyr::mutate(cluster_boundaries = purrr::map(cluster_data,
                                                  ~generate_hotspot_boundaries(.))) %>%
    dplyr::mutate(cluster_data_sf = purrr::map(cluster_data, . %>%
                                                 sf::st_as_sf(coords = c("lon","lat"),
                                                              crs = sf::st_crs(4326)) %>%
                                                 sf::st_transform(map_projection))) %>%
    dplyr::mutate(cluster_boundaries_sf =  purrr::map(cluster_boundaries, .%>%
                                                        dplyr::rowwise() %>%
                                                        dplyr::mutate(geometry = sf::st_geometry(sf::st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
                                                        sf::st_as_sf(sf_column_name = "geometry", crs = 4326) %>%
                                                        sf::st_transform(map_projection))) %>%
    mutate(year_label = glue::glue("{year_start} - {year_start + 12}")) 
  
  # Load world land
  world_land <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
    sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
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
  
  # Turn windowed attack data into sf
  attack_data <- hotspots_over_time %>%
    dplyr::select(year_label,cluster_data_sf) %>%
    tidyr::unnest(cluster_data_sf) %>%
    sf::st_as_sf()
  
  # Turn windowed hotspot boundaries into sf
  hotspot_boundaries <- hotspots_over_time %>%
    dplyr::select(year_label,cluster_boundaries_sf) %>%
    tidyr::unnest(cluster_boundaries_sf) %>%
    sf::st_as_sf()
  
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = world_bbox_sf,
                     fill = NA,
                     color = "black") +
    ggplot2::geom_sf(data = world_land,
                     color = "darkgrey",
                     fill = "darkgrey") +
    ggplot2::geom_sf(data = attack_data,
                     size = 0.01) + 
    ggplot2::geom_sf(data = hotspot_boundaries,
                     fill = NA,
                     color = "red",
                     linewidth = 1.0001) +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
    ggplot2::facet_wrap(year_label~.) +
    ggplot2::theme(strip.background = ggplot2::element_blank())
  
  ggplot2::ggsave(filename = "figures/map_hotspots_over_time.png",
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

# Generate piracy attack hotspot boundaries
# use dbscan to generate clusters, focusing on a specified year range
generate_asam_with_hotspots <- function(asam_data_processed,
                                        year_min = 2010,
                                        years_to_include = 12){
  
  asam_filtered <- asam_data_processed %>%
    filter(lubridate::year(date) >= year_min,
           lubridate::year(date) <= year_min + years_to_include)
  
  # Find DBSCAN clusters for attacks occurring during this range
  dbscan_clusters <- fpc::dbscan(asam_filtered %>%
                                   dplyr::select(lon,lat),
                                 eps = 10, #km
                                 MinPts = 200)
  
  asam_filtered %>%
    dplyr::mutate(cluster_number = dbscan_clusters$cluster)%>%
    dplyr::mutate(cluster = dplyr::case_when(cluster_number == 1 ~ "hotspot_southeast_asia",
                                             cluster_number == 2 ~ "hotspot_gulf_of_aden",
                                             cluster_number == 3 ~ "hotspot_gulf_of_guinea",
                                             TRUE ~ "0"))
}

# Creat bounding box for each cluster, filter out 0 cluster since those are non-clustered attacks
# Round this up or down to nearest 5 degrees, to match units of analysis
generate_hotspot_boundaries <- function(asam_with_clusters){
  purrr::map(unique(asam_with_clusters$cluster),function(x){
    temp_df <- asam_with_clusters %>%
      dplyr::filter(cluster == x)
    data.frame(cluster = x,
               lon_min = floor(min(temp_df$lon)/5)*5,
               lat_min = floor(min(temp_df$lat)/5)*5,
               lon_max = ceiling(max(temp_df$lon)/5)*5,
               lat_max = ceiling(max(temp_df$lat)/5)*5)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(cluster != "0")
}

# Code to generate SQL for assigning lat/lon to hotspots
generate_hotspot_sql <- function(hotspots){
  hotspots %>%
    mutate(cluster_filter = glue::glue("(CASE WHEN (lat_bin <= {lat_max} AND lat_bin >= {lat_min} AND lon_bin <= {lon_max} AND lon_bin >= {lon_min}) THEN 1 ELSE 0 END) {cluster}")) %>%
    .$cluster_filter %>% paste0(.,collapse = ", ")
}


make_attack_time_series_figure <- function(asam_with_hotspots){
  
  plot <- asam_with_hotspots %>%
    group_by(year = lubridate::year(date),
             cluster) %>%
    summarize(number_attacks = n_distinct(asam_reference)) %>%
    ungroup() %>%
    mutate(cluster = case_when(cluster == "hotspot_southeast_asia" ~ "Southeast asia",
                               cluster == "hotspot_gulf_of_aden" ~ "Gulf of Aden",
                               cluster == "hotspot_gulf_of_guinea" ~ "Gulf of Guinea",
                               TRUE ~ "Rest of world")  %>%
             fct_relevel(c("Gulf of Guinea",
                           "Gulf of Aden",
                           "Southeast asia"))) %>%
    ggplot(aes(x = year, y = number_attacks, fill = cluster)) +
    geom_bar(position = "stack", stat="identity",color="black")  +
    scale_fill_brewer("Hotspot",
                       type ="qual",
                       palette = "Dark2") +
    labs(x = "",
         y = "Number\nattacks") +
    scale_x_continuous(breaks = seq(2010,2022,2))
  
  ggplot2::ggsave(filename = "figures/attack_time_series.png",
                  plot,
                  height = 4,
                  width = 7,
                  dpi = 300)
  
  return(plot)
}
