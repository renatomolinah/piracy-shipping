##########################################################################################
## Full analysis script for global piracy shipping paper with Renato Molina
## Gavin McDonald
## June 7, 2018
##########################################################################################
library(sf)
library(lubridate)
library(tidyverse)
library(bigrquery)
library(zoo)
library(fpc)
library(fossil)
library(readr)
# Set up bigquery
#project <-  "ucsb-gfw"
project <- "ucsb-gfw"
billing_project <- "emlab-gcp"
options(scipen = 20)
# This helps with st_join
sf_use_s2(FALSE)
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
  filter(Year > 2004 & Year < 2018) %>%
  group_by(Reference) %>%
  # Remove duplicates
  filter(row_number() == 1) %>%
  ungroup() %>%
  `st_crs<-`(st_crs(eez)) %>%
  st_join(eez, join = st_intersects) %>%
  mutate(attack_eez = ifelse(is.na(ISO_Ter1),"High Seas",as.character(ISO_Ter1)))

# Upload this to BQ
bq_table(project = project,table = "asam",dataset = "piracy") %>% 
  bq_table_delete()
bq_table(project = project,table = "asam",dataset = "piracy") %>% 
  bq_table_upload(values = ASAM %>% 
                    mutate(point=st_as_text(geometry)) %>%
                    st_set_geometry(NULL) %>%
                    dplyr::select(reference=Reference,
                                  aggressor=Aggressor,
                                  victim=Victim,
                                  date=Date,
                                  year=Year,
                                  attack_eez,
                                  point))

# Define cell size for grid map
cell_size <- 5 #degrees
one_over_cellsize = 5/cell_size

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

write_csv(ASAM_binned,"processed_data/ASAM_binned.csv")

ASAM <- ASAM %>%
  left_join(ASAM_binned %>%
              dplyr::select(year,lon_bin,lat_bin,grid_id),by = c("year", "lon_bin", "lat_bin")) %>%
  dplyr::select(date,year,lat,lon,lat_bin,lon_bin,grid_id,aggressor,victim,attack_eez)

# Figure out clusters across all years
# Cluster attacks together based on max distance between attacks and minimum number of attacks
dist<- earth.dist(ASAM  %>%
                    dplyr::select(lat,lon), dist=T)
# Set seed for reproducibility
set.seed(101)
# calcualte clusters for attacks
DBSCAN_temp <- dbscan(dist,
                      eps = 500, #km
                      MinPts = 200, # number of attacks per cluster
                      method = "dist")

ASAM$cluster <-DBSCAN_temp$cluster

ASAM <- ASAM %>%
  mutate(cluster = case_when(cluster == 1 ~ "hotspot_gulf_of_guinea",
                             cluster == 2 ~ "hotspot_southeast_asia",
                             cluster == 3 ~ "hotspot_gulf_of_aden",
                             TRUE ~ "0")) %>%
  mutate(Hotspot = case_when(cluster == "hotspot_gulf_of_guinea" ~ "Gulf of Guinea",
                             cluster == "hotspot_gulf_of_aden" ~ "Gulf of Aden",
                             cluster == "hotspot_southeast_asia" ~ "Southeast Asia",
                             cluster == "0" ~ "Outside of Hotspots"))

# Creat bounding box for each cluster, filter out 0 cluster since those are non-clustered attacks
cluster_boxes <- purrr::map(unique(ASAM$cluster),function(x){
  temp_df <- ASAM %>%
    filter(cluster == x)
  data.frame(cluster = x,
             lon_min = min(temp_df$lon)-5,
             lat_min = min(temp_df$lat)-5,
             lon_max = max(temp_df$lon)+5,
             lat_max = max(temp_df$lat) + 5)
}) %>%
  bind_rows() %>%
  filter(cluster != "0")

write_csv(cluster_boxes,"processed_data/cluster_boxes.csv")
cluster_boxes<-read_csv("processed_data/cluster_boxes.csv")
# Create sql SELECT clauses for each hotspot cluster
cluster_filters <- cluster_boxes %>%
  mutate(cluster_filter = paste0("(CASE WHEN (lat < ",lat_max, " AND lat > ",lat_min, " AND lon < ",lon_max," AND lon > ",lon_min,") THEN 1 ELSE 0 END) ",cluster))

#cluster_filters <- paste0(cluster_filters$cluster_filter,collapse = ", ")

clusters_aggregated <- cluster_boxes %>%
  mutate(cluster_filter = paste0("(CASE WHEN SUM(",cluster,") > 0 THEN 1 ELSE 0 END) ",cluster))

clusters_aggregated <- paste0(clusters_aggregated$cluster_filter,collapse = ", ")

clusters_aggregated_2 <- paste0(cluster_boxes$cluster,collapse = ", ")

# Cache attack data for later
write_csv(ASAM,"processed_data/attacks.csv")

# Create expanded attack dataframe for every grid and date combination
# To create summary stats for each combination
# Will then join this to vessel track info in big query
ASAM <- read_csv("processed_data/attacks.csv")

# Define cell size for grid map
cell_size <- 0.5 #degrees
one_over_cellsize = 1/cell_size

ASAM <- ASAM %>%
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


date_range <- seq(ymd('2010-01-01'),ymd('2017-12-31'), by = '1 day')

expanded_asam <- expand.grid(date = date_range,
                             grid_id = grids$grid_id) %>%
  as_tibble() %>%
  left_join(grids,by="grid_id") %>%
  left_join(ASAM %>%
              dplyr::select(date,
                            lon_bin,
                            lat_bin,
                            attack_eez) %>%
              group_by(date,lon_bin,lat_bin) %>%
              summarize(number_attacks = n()),by=c("date","lon_bin","lat_bin")) %>%
  mutate(number_attacks = ifelse(is.na(number_attacks),0,number_attacks)) %>%
  group_by(grid_id) %>%
  nest() %>%
  ungroup() %>%
  crossing(month = c(seq(1,24))) %>% 
  mutate(data = map2(data,month,function(.x,.y){
    .x %>% 
      mutate(attacks_window_last = rollapplyr(number_attacks, width = 30*abs(.y ), FUN = sum, na.rm=TRUE, partial = TRUE),
             attacks_window_next = rollapplyr(number_attacks, width = 30*abs(.y ) + 1, FUN = function(x) sum(x[-1], na.rm = TRUE), partial = TRUE, align = "left"))
  })) %>%
  unnest(data) %>%
  mutate(month = paste0(abs(month),"_month")) %>%
  #mutate(month = ifelse(month>0,paste0("next_",month,"_month"),paste0("last_",abs(month),"_month"))) %>%
  pivot_wider(names_from = "month",
              values_from = c("attacks_window_next","attacks_window_last"))  %>%
  group_by(grid_id) %>%
  mutate(tmpG = cumsum(c(FALSE, as.logical(diff(number_attacks))))) %>%
  mutate(tmp_a = c(1, diff(date)) * !number_attacks) %>%
  group_by(grid_id,tmpG) %>%
  mutate(days_since_attack = ifelse(tmpG == 0, NA, cumsum(tmp_a)),
         grid_has_previous_attacks = ifelse(is.na(days_since_attack), 0, 1)) %>%
  ungroup() %>%
  select(-c(tmp_a, tmpG, number_attacks)) %>%
  filter(date > ymd('2011-12-31'))

# Make something similar, except for hotspots intsead of grids
expanded_asam_hotspot <- expand.grid(date = date_range,
                                     Hotspot = unique(ASAM$Hotspot)) %>%
  as_tibble() %>%
  left_join(ASAM %>%
              group_by(date,Hotspot) %>%
              summarize(number_attacks = n()),by=c("date","Hotspot")) %>%
  mutate(number_attacks = ifelse(is.na(number_attacks),0,number_attacks)) %>%
  group_by(Hotspot) %>%
  nest() %>%
  ungroup() %>%
  crossing(month = c(seq(3,12,3))) %>%
  mutate(data = map2(data,month,function(.x,.y){
    .x %>% 
      mutate(hotspot_attacks_window_last = rollapplyr(number_attacks, width = 30*abs(.y ), FUN = sum, na.rm=TRUE, partial = TRUE))#,
    #hotspot_attacks_window_next = rollapplyr(number_attacks, width = 30*abs(.y ) + 1, FUN = function(x) sum(x[-1], na.rm = TRUE), partial = TRUE, align = "left"))
  })) %>%
  unnest(data) %>%
  mutate(month = paste0(abs(month),"_month")) %>%
  pivot_wider(names_from = "month",
              #values_from = c("hotspot_attacks_window_next","hotspot_attacks_window_last")) 
              values_from = c("hotspot_attacks_window_last"),
              names_prefix = "hotspot_attacks_window_last") %>%
  dplyr::select(-number_attacks)

write_csv(expanded_asam_hotspot,"processed_data/piracy_attacks_hotspot.csv")



write_csv(expanded_asam,glue::glue("processed_data/piracy_attacks_",cell_size,".csv"))

expanded_asam <- read.csv(glue::glue("processed_data/piracy_attacks_",cell_size,".csv"),stringsAsFactors = FALSE) %>%
  as_tibble()

window_names <- tibble(names = colnames(expanded_asam)) %>%
  filter(!(names %in% c("grid_id","date","lon_bin","lat_bin","days_since_attack","grid_has_previous_attacks"))) %>%
  .$names

window_names_select <- window_names%>%
  paste(collapse = ",")


window_names_sum <- paste0("SUM(",window_names,") ",window_names) %>%
  paste(collapse = ",")

window_names_last <- tibble(names = colnames(expanded_asam)) %>%
  filter(str_detect(names,"last")) %>%
  .$names

window_names_null <-tibble(names = colnames(expanded_asam),
                           month = as.numeric(str_extract(names, "(\\d)+"))) %>%
  filter(str_detect(names,"last") | month <= 12) %>%
  .$names

window_names_null <- paste0("IF(",window_names_null," IS NULL,0,",window_names_null,") ",window_names_null) %>%
  paste(collapse = ",")

window_names_next <- tibble(names = colnames(expanded_asam),
                            month = as.numeric(str_extract(names, "(\\d)+")),
                            year = case_when(month > 12~"2015")) %>%
  filter(month > 12) %>%
  filter(str_detect(names,"next")) %>%
  mutate(term = paste0("(CASE WHEN year > ",year," THEN NULL WHEN ",names," IS NULL THEN 0 ELSE ",names," END) ",names)) %>%
  .$term %>%
  paste(collapse = ",")


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
#source("r/sql/piracy_attacks.R") # Check - already run!
#source("r/sql/piracy_attacks_hotspot.R")
#source("r/sql/cluster_filters.R") # Check - already run!
#source("sql/vessel_info.R") # Check - already run!
#source("sql/voyages_with_anchorages.R") # Check - already run!
#source("sql/voyage_ais_positions.R")
#source("sql/voyages_gridded.R")#
#source("sql/voyages.R")
#source("sql/gridded_shipping_hours.R")
#source("sql/hotspot_summary.R")

#### New table for Renato
#### Renato will use this in an instrumental variable approach to determine
#### causal effect of pirate attacks on shipping, using EEZ-level development instruments
project <- "emlab-gcp"
billing_project <- "emlab-gcp"

# Table summarizes shipping activity by eez-year
table_name <- "shipping_activity_by_year_eez"

bq_project_query(billing_project,
                 query = read_file(here::here(glue::glue("sql/{table_name}.sql"))), 
                 destination_table = bq_table(project = project,
                                              table = table_name,
                                              dataset = "piracy"),
                 use_legacy_sql = FALSE, 
                 allowLargeResults = TRUE,
                 write_disposition = "WRITE_TRUNCATE")


bq_project_query(billing_project, glue::glue("SELECT * FROM `emlab-gcp.piracy.{table_name}`")) %>%
  bq_table_download(n_max = Inf) %>%
  write_csv(here::here(glue::glue("data/{table_name}.csv")))

#### New table for Grant
#### Grant will use this to try an ML approach that predicts where vessels should be
## Need to re-upload attack info to emlab-gcp
condensed_attacks <- expanded_asam %>% 
  dplyr::select(date,
                lon_bin,
                lat_bin,
                days_since_attack,
                attacks_window_last_1_month,
                attacks_window_last_2_month,
                attacks_window_last_3_month,
                attacks_window_last_4_month,
                attacks_window_last_5_month,
                attacks_window_last_6_month,
                attacks_window_last_7_month,
                attacks_window_last_8_month,
                attacks_window_last_9_month,
                attacks_window_last_10_month,
                attacks_window_last_11_month,
                attacks_window_last_12_month)

bq_table(project = project,
         table = glue::glue("piracy_attacks_",str_replace_all(cell_size,"\\.","_")),
         dataset = "piracy") %>% 
  bq_table_upload(values = condensed_attacks,
                  fields = as_bq_fields(condensed_attacks),
                  write_disposition = "WRITE_TRUNCATE")

# New table for Grant on March 10, 2022
table_name <- "ungridded_data_ml"

# Table summarizes shipping activity, voyage info, attacks, and wind by mmsi-voyage-day
bq_project_query(billing_project,
                 query = read_file(here::here(glue::glue("sql/{table_name}.sql"))), 
                 destination_table = bq_table(project = project,
                                              table = table_name,
                                              dataset = "piracy"),
                 use_legacy_sql = FALSE, 
                 allowLargeResults = TRUE,
                 write_disposition = "WRITE_TRUNCATE")

table_name <- "gridded_data_ml"

# Table summarizes shipping activity, voyage info, attacks, and wind by mmsi-voyage-day-lat_bin-lon_bin
bq_project_query(billing_project,
                 query = read_file(here::here(glue::glue("sql/{table_name}.sql"))), 
                 destination_table = bq_table(project = project,
                                              table = table_name,
                                              dataset = "piracy"),
                 use_legacy_sql = FALSE, 
                 allowLargeResults = TRUE,
                 write_disposition = "WRITE_TRUNCATE")

table_name <- "gridded_data_ml_top_route"

# Table summarizes shipping activity, voyage info, attacks, and wind by mmsi-voyage-day-lat_bin-lon_bin
# Only for the most traveled route
# For each trip-lat_bin-lon_bin, just take first day
bq_project_query(billing_project,
                 query = read_file(here::here(glue::glue("sql/{table_name}.sql"))), 
                 destination_table = bq_table(project = project,
                                              table = table_name,
                                              dataset = "piracy"),
                 use_legacy_sql = FALSE, 
                 allowLargeResults = TRUE,
                 write_disposition = "WRITE_TRUNCATE")


bq_project_query(billing_project, glue::glue("SELECT * FROM `emlab-gcp.piracy.{table_name}`")) %>%
  bq_table_download(n_max = Inf) %>%
  write_csv(here::here(glue::glue("data/{table_name}.csv")))
