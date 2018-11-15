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
         lat_bin = floor(lat*one_over_cellsize)/one_over_cellsize,
         attack_id = 1:n())

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
                     filter(date >= "2017-03-01" & date <= "2017-11-01") %>%
                     distinct(),
              color="red",
              stroke=FALSE,
              fillOpacity = 0.5,
              radius = 3,
              popup = ~paste0("<font color=#000000>",
                              "<b>Attack ID: </b>", attack_id, "<br>",
                "<b>EEZ: </b>", attack_eez, "<br>",
                "<b>Date: </b>", date, "<br>",
                "<b>Lat: </b>", lat_map, "<br>",
                "<b>Lon: </b>", lon_map, "<br>",
                "<b>Aggressor: </b>", aggressor, "<br>",
                "<b>Victim: </b>", victim, "<br>",
                 "</font>"))

sql <- "#standardSQL
SELECT
EXTRACT(date FROM departure_timestamp) date,
FLOOR(start_lat / 0.01) * 0.01 lat_bin,
FLOOR(start_lon / 0.01) * 0.01 lon_bin,
SUM(hours) hours
FROM
`piracy.voyage_ais_positions`
GROUP BY
date,
lat_bin,
lon_bin"

# bq_table(project = project,table = "cell_data",dataset = "piracy") %>% 
#   bq_table_delete()
# 
# bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "cell_data",dataset = "piracy"),
#                 use_legacy_sql = FALSE, allowLargeResults = TRUE)

attacks_som <- ASAM %>%
  filter(attack_eez == "SOM")  %>%
  filter(date >= "2017-03-01" & date <= "2017-11-01")

som_filter <- glue::glue("lat_bin >= {min(attacks_som$lat) - 0.5} AND lat_bin <= {max(attacks_som$lat) + 0.5} AND lon_bin >= {min(attacks_som$lon) - 0.5} AND lon_bin <= {max(attacks_som$lon) + 0.5}")

malacca_filter <- glue::glue("lat_bin >= 0 AND lat_bin <= 2 AND lon_bin >= 98 AND lon_bin <= 105.5")

nga_filter <- glue::glue("lat_bin >= 1 AND lat_bin <= 8 AND lon_bin >= 1 AND lon_bin <= 10")

sql <- glue::glue("
#standardSQL
SELECT
  *
FROM
  `piracy.cell_data`
WHERE
  ({som_filter}) OR ({malacca_filter}) OR ({nga_filter})")


# bq_table(project = project,table = "figure_cell_data",dataset = "piracy") %>% 
#   bq_table_delete()
# 
# bq_project_query(project,query = sql, destination_table =  bq_table(project = project,table = "figure_cell_data",dataset = "piracy"),
#                  use_legacy_sql = FALSE, allowLargeResults = TRUE)

# cell_data <- bq_project_query(project, "#standardSQL 
# SELECT * 
# FROM `piracy.figure_cell_data`") %>%
#   bq_table_download(max_results = Inf)

#write_csv(cell_data,path="Data sets/cell_data.csv")
cell_data <- read_csv("Data sets/cell_data.csv")
world_land <-
  st_as_sf(rworldmap::countriesLow) ## data(countriesLow) is from the rworldmap package


all_attack_info <- ASAM %>% 
  mutate(lat_bin = floor(lat),
         lon_bin=floor(lon),
         area=paste(lon_bin,lat_bin,sep="-")) %>% 
  arrange(attack_eez,area,date) %>%
  filter(date >="2017-08-01" & date <= "2017-11-01") %>% 
  distinct(lon,lat,date,.keep_all=TRUE)

fig_ids <- c(220,221,222) # first NGA fig

fig_ids <- 156 # second NGA fig

attack_info <- all_attack_info %>%
  filter(attack_id %in% fig_ids)

min_lat = mean(attack_info$lat) - 0.5
max_lat = mean(attack_info$lat) + 0.5
min_lon = mean(attack_info$lon) - 0.5
max_lon = mean(attack_info$lon) + 0.5
min_date = min(attack_info$date)
max_date = max(attack_info$date)

attack_info <- all_attack_info %>%
  filter(lat > min_lat & lat < max_lat & lon > min_lon & lat < max_lat & date >= min_date & date <= max_date)

min_lat = mean(attack_info$lat) - 0.5
max_lat = mean(attack_info$lat) + 0.5
min_lon = mean(attack_info$lon) - 0.5
max_lon = mean(attack_info$lon) + 0.5
min_date = min(attack_info$date)
max_date = max(attack_info$date)

cell_figure <- cell_data %>%
  filter(lat_bin >= min_lat & lat_bin <= max_lat & lon_bin >= min_lon & lat_bin <= max_lat) %>%
  mutate(date_bin = case_when(date >= min_date %m+% months(-2) & date < min_date %m+% months(-1) ~ -2,
                              date >= min_date %m+% months(-1) & date < min_date ~ -1,
                              date >= max_date & date < max_date %m+% months(1) ~ 1,
                              date >= max_date %m+% months(1) & date < max_date %m+% months(2) ~ 2,
                              TRUE~NA_real_)) %>%
  filter(!is.na(date_bin))

fig <- cell_figure %>%
  group_by(lon_bin,lat_bin,date_bin) %>%
  summarize(hours = sum(hours)) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(x = lon_bin,y=lat_bin,fill = hours)) +
  facet_wrap(~date_bin) + 
  coord_fixed() +
  geom_point(data = attack_info,aes(x = lon,y=lat),color="red",size=3) +
  geom_sf(data=world_land, color = NA, fill="grey30") +
  xlim(c(min_lon,max_lon)) +
  ylim(c(min_lat,max_lat)) +
  theme(panel.background = element_rect(fill ="black",color="black"),
        panel.grid.major =  element_line(color = "black"),
        panel.grid.minor = element_line(color = "black")) +
  ggtitle(paste0("Transit time spent in 0.01 x 0.01 degree cells\n2 months prior to first attack to 2 months after last attack\nAttack location(s) shown as red dot(s)\nAttack(s) took place on ",paste(unique(attack_info$date),collapse=", ")))+
  scale_fill_gradientn(colours = pals::parula(100),
                       "Transit hours",
                       guide = "colourbar",
                       trans = "log",
                       breaks = scales::log_breaks(n = 7, base = 2),
                       labels = scales::comma) +
  xlab("") +
  ylab("")

ggsave(filename = "figs/nga_cell_2.eps", plot = fig, device = cairo_ps, width = 8.5, height = 8.5)

