targets::tar_load(asam_data_processed)
library(tidyverse)

dist<- earth.dist(asam_data_processed  %>%
                    distinct(lon,lat) %>%
                    dplyr::select(lon,lat), dist=T)

# Find DBSCAN clusters for attacks occurring in 2010>
dbscan_clusters <- dbscan::dbscan(asam_data_processed %>%
                            filter(lubridate::year(date)>2010) %>%
                            dplyr::select(lon,lat),
                          eps = 10, #km
                          MinPts = 200)

clusters <- asam_data_processed %>%
  filter(lubridate::year(date)>2010) %>%
  mutate(cluster = dbscan_clusters$cluster)%>%
  mutate(cluster = case_when(cluster == 1 ~ "hotspot_southeast_asia",
                             cluster == 2 ~ "hotspot_gulf_of_aden",
                             cluster == 3 ~ "hotspot_gulf_of_guinea",
                             TRUE ~ "0"))

# Creat bounding box for each cluster, filter out 0 cluster since those are non-clustered attacks
cluster_boxes_new <- purrr::map(unique(clusters$cluster),function(x){
  temp_df <- clusters %>%
    filter(cluster == x)
  data.frame(cluster = x,
             lon_min = min(temp_df$lon)-5,
             lat_min = min(temp_df$lat)-5,
             lon_max = max(temp_df$lon)+5,
             lat_max = max(temp_df$lat) + 5)
}) %>%
  bind_rows() %>%
  filter(cluster != "0")

# Set projection for mapping
# Use Mollweide
map_projection <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Load world land
world_land <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
  sf::st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"), quiet = TRUE) %>%
  sf::st_transform(map_projection)

cluster_boxes %>%
  mutate(across(-c(cluster),
                ~floor(./5)*5))

ggplot() +
  geom_sf(data = world_land) + 
  geom_sf(data = cluster_boxes  %>%
            rowwise() %>%
            mutate(geometry = st_geometry(st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
            st_as_sf(sf_column_name = "geometry", crs = 4326),
          fill = NA,
          aes(color = cluster))+ 
  geom_sf(data = asam_filtered %>%
            st_as_sf(coords = c("lon","lat"), crs = 4326),
          aes(color = cluster),
          size=0.1) +
  scale_color_brewer(type ="qual",
                     palette = "Dark2")

tmp <- tibble(year = seq(2014,2022)) %>%
  mutate(year_label = glue::glue("{year-12} - {year}")) %>%
  mutate(data = purrr::map(year, function(year_tmp){
    asam_data_processed %>%
      filter(lubridate::year(date) > year_tmp - 12) %>%
      filter(lubridate::year(date) <= year_tmp)})) %>%
  mutate(clusters = purrr::map(data,function(df){
    # Find DBSCAN clusters for attacks occurring in 2010>
    dbscan_clusters <- dbscan(df %>%
                                dplyr::select(lon,lat),
                              eps = 10, #km
                              MinPts = 200)
    
    df %>%
      mutate(cluster = dbscan_clusters$cluster)
  })) %>%
  mutate(cluster_boxes = purrr::map(clusters,function(cluster){
    purrr::map(unique(cluster$cluster),function(x){
      temp_df <- cluster %>%
        filter(cluster == x)
      data.frame(cluster = x,
                 lon_min = min(temp_df$lon)-5,
                 lat_min = min(temp_df$lat)-5,
                 lon_max = max(temp_df$lon)+5,
                 lat_max = max(temp_df$lat) + 5)
    }) %>%
      bind_rows() %>%
      filter(cluster != "0")
  }))

ggplot() +
  geom_sf(data = world_land) + 
  geom_sf(data = tmp %>% 
            dplyr::select(year_label,clusters) %>% 
            unnest(clusters) %>%
            st_as_sf(coords = c("lon","lat"), crs = 4326),
          aes(color = as.factor(cluster)),
          size = 0.1) + 
  geom_sf(data = tmp %>% 
            dplyr::select(year_label,cluster_boxes) %>% 
            unnest(cluster_boxes)  %>%
            rowwise() %>%
            mutate(geometry = st_geometry(st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
            st_as_sf(sf_column_name = "geometry", crs = 4326),
          fill = NA,
          aes(color = as.factor(cluster))) +
  facet_wrap(year_label~.,ncol=3)+
  scale_color_brewer(type ="qual",
                     palette = "Dark2")
