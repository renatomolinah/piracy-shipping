library(tidyverse)
library(bigrquery)
library(lubridate)
library(sf)
# Set up bigquery
project <-  "ucsb-gfw"
eez <- read_sf(dsn = "raw_data/World_EEZ_v9_20161021_LR", layer = "eez_lr", stringsAsFactors = FALSE)

ASAM <- read_sf(dsn = "raw_data/ASAM_shp", layer = "ASAM 20 MAR 18", stringsAsFactors = FALSE) %>%
  mutate(Date = as_date(DateOfOcc),
         Year = year(Date)) %>%
  dplyr::select(-DateOfOcc) %>%
  filter(Year > 2008 & Year < 2018) %>%
  `st_crs<-`(st_crs(eez)) %>%
  st_join(eez, join = st_intersects) %>%
  mutate(attack_eez = ifelse(is.na(ISO_Ter1),"High Seas",as.character(ISO_Ter1)))

# Define cell size for grid map
cell_size <- 5 #degrees
one_over_cellsize = 1/cell_size

ASAM <- do.call(rbind, st_geometry(ASAM)) %>% 
  as_tibble() %>% 
  cbind(ASAM$Year,
        ASAM$Date,
        ASAM$Aggressor,
        ASAM$Victim,
        ASAM$attack_eez) %>%
  setNames(c("lon","lat","year","date","aggressor","victim","attack_eez")) %>%
  mutate(lon_bin = floor(lon*one_over_cellsize)/one_over_cellsize,
         lat_bin = floor(lat*one_over_cellsize)/one_over_cellsize) 

library(leaflet)
leaflet() %>%
  # fitBounds(0, 60, 35, 72) %>%
  addPolygons(data = eez,
    fillOpacity = 0.15,
    fillColor = "blue",
    stroke=TRUE,
    weight=1,
    color="black",
    popup = ~paste0("<font color=#000000>",
      "<b>EEZ: </b>", GeoName, "<br>",
    "</font>"))  %>%
  addCircleMarkers(data=ASAM %>%
                     mutate(lat_map = lat,
                            lon_map = lon) %>%
                     st_as_sf(coords = c("lon","lat")) %>%
                     `st_crs<-`(st_crs(eez)) %>%
                     filter(year >= 2017) %>%
                     distinct(),
              color="red",
              stroke=FALSE,
              fillOpacity = 0.5,
              radius = 3,
              popup = ~paste0("<font color=#000000>",
                "<b>EEZ: </b>", attack_eez, "<br>",
                "<b>Date: </b>", date, "<br>",
                "<b>Lat: </b>", lat_map, "<br>",
                "<b>Lon: </b>", lon_map, "<br>",
                "<b>Aggressor: </b>", aggressor, "<br>",
                "<b>Victim: </b>", victim, "<br>",
                 "</font>"))
# pick 8 lon and 4 lat for bin

attack_info <- ASAM %>% 
  filter(attack_eez == "NGA") %>% 
  mutate(lat_bin = floor(lat),
         lon_bin=floor(lon),
         area=paste(lon_bin,lat_bin,sep="-")) %>% 
  arrange(area,date) %>%
  filter(date >="2017-08-01" & date <= "2017-11-01") %>% 
  distinct() %>%
  filter(lon_bin==7 & lat_bin==3)

attack_info <- attack_info[3,]

sql <- "
#standardSQL
SELECT
  mmsi,
  lat,
  lon,
  timestamp,
  hours,
  implied_speed,
  avg_distance_km
FROM
  `world-fishing-827.gfw_research.pipeline_p_p550_daily`
WHERE
  FLOOR(lat) >= 2
  AND FLOOR(lat) <= 6
  AND FLOOR(lon) >= 2
  AND FLOOR(lon) <= 8
  AND _PARTITIONTIME BETWEEN TIMESTAMP('2017-06-01')
  AND TIMESTAMP('2017-12-31')
  AND mmsi IN (
  SELECT
    mmsi
  FROM
    `piracy.voyages_with_anchorages`)"

#bq_table(project = project,table = "nga_cell_figure",dataset = "piracy") %>% 
#  bq_table_delete()

#bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "nga_cell_figure",dataset = "piracy"),
#                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

world_land <-
  st_as_sf(rworldmap::countriesLow) ## data(countriesLow) is from the rworldmap package


nga_gfw <- bq_project_query(project, "SELECT * FROM `piracy.nga_cell_figure`") %>%
  bq_table_download(max_results = Inf)

cell_figure <- nga_gfw %>%
  mutate(lat_bin = floor(lat),
         lon_bin = floor(lon)) %>%
  filter(lat_bin == attack_info$lat_bin & lon_bin ==attack_info$lon_bin) %>%
  mutate(date = date(timestamp)) %>%
  mutate(mmsi = as.character(mmsi)) %>%
  filter(implied_speed < 50) %>%
  filter(date >= attack_info$date %m+% months(-2))
  

mmsi_start_date <- cell_figure %>%
  group_by(mmsi) %>%
  summarize(min_date = min(date)) %>%
  ungroup() %>%
  mutate(date_bin = case_when(min_date >= attack_info$date %m+% months(-2) & min_date < attack_info$date %m+% months(-1) ~ -2,
                              min_date >= attack_info$date %m+% months(-1) & min_date < attack_info$date ~ -1,
                              min_date >= attack_info$date & min_date < attack_info$date %m+% months(1) ~ 1,
                              min_date >= attack_info$date %m+% months(1) & min_date < attack_info$date %m+% months(2) ~ 2,
                              TRUE~NA_real_))

cell_figure %>%
  left_join(mmsi_start_date,by="mmsi") %>%
  filter(!is.na(date_bin)) %>%
  arrange(mmsi,timestamp) %>%
  ggplot() +
  geom_path(aes(x = lon,y=lat,group=mmsi,color=mmsi)) +
  facet_wrap(~date_bin) + 
  coord_fixed() +
  geom_point(data = attack_info,aes(x = lon,y=lat),color="red",size=3) +
  geom_sf(data=world_land, color = NA, fill="grey30") +
  xlim(c(attack_info$lon_bin,attack_info$lon_bin+1)) +
  ylim(c(attack_info$lat_bin,attack_info$lat_bin+1)) +
  theme(panel.background = element_rect(fill ="black",color="black"),
        panel.grid.major =  element_line(color = "black"),
        panel.grid.minor = element_line(color = "black")) +
  guides(color=FALSE) +
  ggtitle("Transits through 1x1 cell in Nigeria waters\n2 months prior to attack to 2 months after attack\nAttack location shown as red dot")

cell_figure %>%
  left_join(mmsi_start_date,by="mmsi") %>%
  filter(!is.na(date_bin)) %>%
  arrange(mmsi,timestamp) %>%
  mutate(lat_bin = floor(lat*100)/100,
         lon_bin = floor(lon*100)/100) %>%
  group_by(lat_bin,lon_bin,date_bin) %>%
  summarize(hours = sum(hours)) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = lon_bin,y=lat_bin,fill = hours)) +
  facet_wrap(~date_bin) + 
  coord_fixed() +
  geom_point(data = attack_info,aes(x = lon,y=lat),color="red",size=3) +
  geom_sf(data=world_land, color = NA, fill="grey30") +
  xlim(c(attack_info$lon_bin,attack_info$lon_bin+1)) +
  ylim(c(attack_info$lat_bin,attack_info$lat_bin+1)) +
  theme(panel.background = element_rect(fill ="black",color="black"),
        panel.grid.major =  element_line(color = "black"),
        panel.grid.minor = element_line(color = "black")) +
  ggtitle("Transit time spent in 0.01 x 0.01 degree cells in Nigeria waters\n2 months prior to attack to 2 months after attack\nAttack location shown as red dot")+
  scale_fill_gradientn(colours = pals::parula(100),
                       "Transit hours",
                       guide = "colourbar",
                       trans = "log",
                       breaks = scales::log_breaks(n = 7, base = 2),
                       labels = scales::comma)
