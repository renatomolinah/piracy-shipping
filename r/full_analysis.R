##########################################################################################
## Full analysis script for global piracy shipping paper with Renato Molina
## Gavin McDonald
## May 1, 2018
##########################################################################################
library(sf)
library(lubridate)
library(tidyverse)
library(bigrquery)
library(zoo)
library(bigrquery)
library(fpc)
library(fossil)

# Set up bigquery
project <-  "ucsb-gfw"

# Read in EEZ data
# Marine Regions V9
eez <- read_sf(dsn = "raw_data/World_EEZ_v9_20161021_LR", layer = "eez_lr", stringsAsFactors = FALSE)

## load anti-shipping data
# Data from http://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_pageLabel=msi_portal_page_65
## Process dates, and filter to only 2011 - 2017, to match GFW data
# Join to eez data to figure out which eez each attack happens in
ASAM <- read_sf(dsn = "raw_data/ASAM_shp", layer = "ASAM 20 MAR 18", stringsAsFactors = FALSE) %>%
  mutate(Date = as_date(DateOfOcc),
         Year = year(Date)) %>%
  dplyr::select(-DateOfOcc) %>%
  filter(Year > 2010 & Year < 2018) %>%
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

# Group by bins, summarize, and add grid id
ASAM_binned <- ASAM %>%
  group_by(year,lon_bin,lat_bin) %>%
  summarize(number_attacks = n()) %>%
  ungroup()

grids <- distinct(ASAM_binned %>% dplyr::select(lon_bin,lat_bin)) %>%
  mutate(grid_id = 1:n())


ASAM_binned <- ASAM_binned %>%
  left_join(grids, by = c("lat_bin","lon_bin"))

ASAM <- ASAM %>%
  left_join(ASAM_binned %>%
              dplyr::select(year,lon_bin,lat_bin,grid_id),by = c("year", "lon_bin", "lat_bin")) %>%
  dplyr::select(date,year,lat,lon,lat_bin,lon_bin,grid_id,aggressor,victim,attack_eez)

# Figure out cluster for each year
# Cluster attacks together based on max distance between attacks and minimum number of attacks
ASAM <- purrr::map(unique(ASAM$year),function(x){
  temp_asam <- ASAM %>%
    filter(year == x)
  
  dist_temp<- earth.dist(temp_asam %>%
                           dplyr::select(lat,lon), dist=T)
  
  # calcualte clusters for attacks
  DBSCAN_temp <- dbscan(dist_temp,
                        eps = 200, #km
                        MinPts = 25, # number of attacks per cluster
                        method = "dist")
  
  temp_asam$cluster <-DBSCAN_temp$cluster
  temp_asam
  
}) %>%
  bind_rows() %>%
  mutate(cluster = paste0(year,"_",cluster))

# Creat bounding box for each cluster, filter out 0 cluster since those are non-clustered attacks
cluster_boxes <- purrr::map(unique(ASAM$cluster),function(x){
  temp_df <- ASAM %>%
    filter(cluster == x)
  data.frame(year = temp_df$year[1],
             cluster = x,
             lon_min = min(temp_df$lon),
             lat_min = min(temp_df$lat),
             lon_max = max(temp_df$lon),
             lat_max = max(temp_df$lat))
}) %>%
  bind_rows() %>%
  filter(!str_detect(cluster,"_0"))

write_csv(cluster_boxes,"processed_data/cluster_boxes.csv")

# Create sql filters for each cluter
cluster_filters <- cluster_boxes %>%
  mutate(cluster_filter = paste0("(start_lat < ",lat_max, " AND start_lat > ",lat_min, " AND start_lon < ",lon_max," AND start_lon > ",lon_min,")")) %>%
  group_by(year) %>%
  summarize(cluster_filter = paste0(cluster_filter,collapse = " OR "))

cluster_filters2 <- paste0("(year = ",cluster_filters$year," AND (",cluster_filters$cluster_filter,"))") %>%
  paste0(collapse = " OR ")

# Cache attack data for later
write_csv(ASAM,"processed_data/attacks.csv")

# Create expanded attack dataframe for every grid and date combination
# To create summary stats for each combination
# Will then join this to vessel track info in big query
date_range <- seq(ymd('2011-01-01'),ymd('2017-12-31'), by = '1 day')

expanded_asam <- expand.grid(date = date_range,
                             grid_id = unique(ASAM$grid_id)) %>%
  left_join(grids,by="grid_id") %>%
  left_join(ASAM %>%
              dplyr::select(date,
                            lon_bin,
                            lat_bin,
                            attack_eez) %>%
              group_by(date,lon_bin,lat_bin) %>%
              summarize(number_attacks = n()),by=c("date","lon_bin","lat_bin")) %>%
  mutate(number_attacks = ifelse(is.na(number_attacks),0,number_attacks),
         attacks_last_7_days = rollapplyr(number_attacks, width = 7, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_14_days = rollapplyr(number_attacks, width = 14, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_21_days = rollapplyr(number_attacks, width = 21, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_30_days = rollapplyr(number_attacks, width = 30, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_60_days = rollapplyr(number_attacks, width = 60, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_90_days = rollapplyr(number_attacks, width = 90, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_120_days = rollapplyr(number_attacks, width = 120, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_150_days = rollapplyr(number_attacks, width = 150, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_180_days = rollapplyr(number_attacks, width = 180, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_210_days = rollapplyr(number_attacks, width = 210, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_240_days = rollapplyr(number_attacks, width = 240, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_270_days = rollapplyr(number_attacks, width = 270, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_300_days = rollapplyr(number_attacks, width = 300, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_330_days = rollapplyr(number_attacks, width = 330, FUN = sum, na.rm=TRUE, partial = TRUE),
         attacks_last_365_days = rollapplyr(number_attacks, width = 365, FUN = sum, na.rm=TRUE, partial = TRUE)) %>%
  # Calculate elapsed time since last attack
  # See https://stackoverflow.com/questions/26553638/calculate-elapsed-time-since-last-event/26554441
  group_by(grid_id) %>%
  mutate(tmpG = cumsum(c(FALSE, as.logical(diff(number_attacks))))) %>%
  mutate(tmp_a = c(1, diff(date)) * !number_attacks) %>%
  group_by(grid_id,tmpG) %>%
  mutate(days_since_attack = ifelse(tmpG == 0, NA, cumsum(tmp_a)),
         grid_has_previous_attacks = ifelse(is.na(days_since_attack), NA, 1)) %>%
  ungroup() %>%
  select(-c(tmp_a, tmpG, number_attacks))

##########################################################################################
# Run wind analysis to prep wind data for big query
# Takes a super long time - need to run one year at a time, then stich them all together
##########################################################################################
# source("r/wind_analysis.R")# Check - already run!

##########################################################################################
# Scrape fuel data from bunkerindex.com
# Thanks Juan!!
##########################################################################################
# source("r/fuel_scraping.R")# Check - already run!

##########################################################################################
# Run SQL queries
# Only run if necessary!! Some of these are very large and expensive
##########################################################################################
#source("sql/piracy_attacks.R") # Check - already run!
#source("sql/cluster_filters.R") # Check - already run!
#source("sql/vessel_info.R") # Check - already run!
#source("sql/voyages_with_anchorages.R") # Check - already run!
#source("sql/voyage_ais_positions.R")
#source("sql/voyages_gridded.R")
#source("sql/voyages.R") # Need to add total fuel cost (need price data), total emissions (need consumption to cO2 conversion)
#source("sql/gridded_shipping_hours.R") # Need to re-run once we have new voyages table