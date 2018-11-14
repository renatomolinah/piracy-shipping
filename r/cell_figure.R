library(tidyverse)
library(bigrquery)
library(lubridate)
# Set up bigquery
project <-  "ucsb-gfw"

ASAM_binned <- read_csv("processed_data/ASAM_binned.csv")

# pick 8 lon and 4 lat for bin

attack_info <- ASAM %>% 
  filter(attack_eez == "NGA") %>% 
  mutate(lat_bin = floor(lat),
         lon_bin=floor(lon),
         area=paste(lon_bin,lat_bin,sep="-")) %>% 
  arrange(area,date) %>%
  filter(date >="2017-08-01" & date <= "2017-10-01") %>% 
  distinct() %>%
  filter(lon_bin==7 & lat_bin==4)

attack_info <- attack_info[nrow(attack_info),]

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
  ggplot() +
  geom_line(aes(x = lon,y=lat,group=mmsi,color=mmsi)) +
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
