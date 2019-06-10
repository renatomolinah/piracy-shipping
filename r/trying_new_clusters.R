ASAM_original <- ASAM
world.land <- read_sf(dsn = "raw_data/ne_110m_land", layer = "ne_110m_land", stringsAsFactors = FALSE)
cluster_boxes_original <- purrr::map(unique(ASAM_original$cluster),function(x){
  temp_df <- ASAM_original %>%
    filter(cluster == x)
  data.frame(cluster = x,
             lon_min = min(temp_df$lon),
             lat_min = min(temp_df$lat),
             lon_max = max(temp_df$lon),
             lat_max = max(temp_df$lat))
}) %>%
  bind_rows() %>%
  filter(cluster != "0") %>%
  rowwise() %>%
  mutate(geometry = st_geometry(st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
  st_as_sf(sf_column_name = "geometry")%>%
  `st_crs<-`(st_crs(eez))

cluster_boxes_original_big <- purrr::map(unique(ASAM_original$cluster),function(x){
  temp_df <- ASAM_original %>%
    filter(cluster == x)
  data.frame(cluster = x,
             lon_min = min(temp_df$lon)-5,
             lat_min = min(temp_df$lat)-5,
             lon_max = max(temp_df$lon)+5,
             lat_max = max(temp_df$lat) + 5)
}) %>%
  bind_rows() %>%
  filter(cluster != "0") %>%
  rowwise() %>%
  mutate(geometry = st_geometry(st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
  st_as_sf(sf_column_name = "geometry")%>%
  `st_crs<-`(st_crs(eez))

ASAM_original %>%
  st_as_sf(coords = c("lon","lat")) %>%
  `st_crs<-`(st_crs(eez)) %>%
  ggplot() + 
  geom_sf(aes(color=as.factor(cluster)),size=0.2) +
  geom_sf(data = world.land,
          fill = "darkgrey",
          lwd = 0)+
  geom_sf(data = cluster_boxes_original,color="black",fill=NA) +
  geom_sf(data = cluster_boxes_original_big,color="black",fill=NA) +
  ggtitle("Current hotspots\nCurrent hotspots +/- 5 degrees")

cluster_boxes <- purrr::map(unique(ASAM$cluster),function(x){
  temp_df <- ASAM %>%
    filter(cluster == x)
  data.frame(cluster = x,
             lon_min = min(temp_df$lon),
             lat_min = min(temp_df$lat),
             lon_max = max(temp_df$lon),
             lat_max = max(temp_df$lat))
}) %>%
  bind_rows() %>%
  filter(cluster != "0") %>%
  rowwise() %>%
  mutate(geometry = st_geometry(st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
  st_as_sf(sf_column_name = "geometry")%>%
  `st_crs<-`(st_crs(eez))

cluster_boxes_big <- purrr::map(unique(ASAM$cluster),function(x){
  temp_df <- ASAM %>%
    filter(cluster == x)
  data.frame(cluster = x,
             lon_min = min(temp_df$lon)-5,
             lat_min = min(temp_df$lat)-5,
             lon_max = max(temp_df$lon)+5,
             lat_max = max(temp_df$lat) + 5)
}) %>%
  bind_rows() %>%
  filter(cluster != "0") %>%
  rowwise() %>%
  mutate(geometry = st_geometry(st_polygon(list(rbind(c(lon_min,lat_min), c(lon_max,lat_min), c(lon_max,lat_max), c(lon_min,lat_max), c(lon_min,lat_min)))))) %>%
  st_as_sf(sf_column_name = "geometry")%>%
  `st_crs<-`(st_crs(eez))

ASAM %>%
  st_as_sf(coords = c("lon","lat")) %>%
  `st_crs<-`(st_crs(eez)) %>%
  ggplot() + 
  geom_sf(aes(color=as.factor(cluster)),size=0.2) +
  geom_sf(data = world.land,
          fill = "darkgrey",
          lwd = 0)+
  geom_sf(data = cluster_boxes,color="black",fill=NA)+
  geom_sf(data = cluster_boxes_big,color="black",fill=NA) +
  ggtitle("New hotspots\nNew hotspots +/- 5 degrees")
