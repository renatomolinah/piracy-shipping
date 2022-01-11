library(rWind)
library(tidyverse)
library(lubridate)
library(bigrquery)
project <-  "emlab-gcp"
# Define cell size for grid map
cell_size <- 5 #degrees
one_over_cellsize = 1/cell_size

# some trigonemetric functions
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

# Function for the circular mean 
# https://en.wikipedia.org/wiki/Mean_of_circular_quantities
circ.mean <- function(deg){
  rad.m <- (deg * pi) / (180)
  mean.cos <- mean(cos(rad.m),na.rm=TRUE)
  mean.sin <- mean(sin(rad.m),na.rm=TRUE)
  
  theta <- rad2deg(atan(mean.sin/mean.cos))
  if(mean.cos < 0) theta <- theta + 180
  if((mean.sin < 0) & (mean.cos > 0)) theta <- theta + 360
  
  theta
}
date_range <- seq(ymd_hms(paste(2012,1,1,12,00,00, sep="-")),
                  ymd_hms(paste(2017,12,31,12,00,00, sep="-")),by="1 days")


rwind_series <- wind.dl_2(date_range,0,359.5,-90,90)

all_wind_data <- tidy(rwind_series)%>%
  # Group into bins, take mean
  mutate(lon_bin = floor(lon*one_over_cellsize)/one_over_cellsize,
         lat_bin = floor(lat*one_over_cellsize)/one_over_cellsize,
         date = date(time)) %>%
  group_by(date,lon_bin,lat_bin)  %>%
  # Take circular mean of direction
  # https://en.wikipedia.org/wiki/Mean_of_circular_quantities
  summarize(wind_direction_degrees = circ.mean(dir),
            wind_speed_m_s = mean(speed,na.rm=TRUE))%>%
  ungroup()

bq_table(project = project,table = "wind",dataset = "piracy") %>% 
  bq_table_upload(values = all_wind_data,
                  fields = as_bq_fields(all_wind_data),
                  write_disposition = "WRITE_TRUNCATE")
