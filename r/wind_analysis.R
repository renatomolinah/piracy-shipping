library(rWind)
library(tidyverse)
library(lubridate)
library(bigrquery)
project <-  "ucsb-gfw"
# Define cell size for grid map
cell_size <- 5 #degrees
one_over_cellsize = 1/cell_size

# Pull in all data from 2012 through 2017
date_range <- seq(ymd_hms(paste(2012,1,1,00,00,00, sep="-")),
                  ymd_hms(paste(2017,12,31,21,00,00, sep="-")),by="3 hours")

rwind_series <- wind.dl_2(date_range,0,359.5,-90,90)
all_wind_data <- tidy(rwind_series)%>%
  # Group into bins, take mean
  mutate(lon_bin = floor(lon*one_over_cellsize)/one_over_cellsize,
         lat_bin = floor(lat*one_over_cellsize)/one_over_cellsize,
         date = date(time)) %>%
  group_by(date,lon_bin,lat_bin)  %>%
  # Take circular mean of direction
  # https://en.wikipedia.org/wiki/Mean_of_circular_quantities
  summarize(direction_degrees = circ.mean(dir),
            speed_m_s = mean(speed,na.rm=TRUE))%>%
  ungroup()

bq_table(project = project,table = "wind",dataset = "piracy") %>% 
  bq_table_upload(values = all_wind_data)
