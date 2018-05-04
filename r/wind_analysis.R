library(rWind)
library(tidyverse)
library(lubridate)

# Define cell size for grid map
cell_size <- 5 #degrees
one_over_cellsize = 1/cell_size

years <- seq(2012,2017)
purrr::map(unique(years),function(x){
  
  
  date_range_wind <- seq(ymd(paste0(x,'-01-01')),ymd(paste0(x,'-12-31')), by = '1 day')
  
  # Grab wind data for every date
  # surface wind data from the NOAA Global Forecasting System <https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-systemgfs>
  # For each date, group by lat bin and lon bin. Take average in each bin
  # Also, take average over all time steps in day
  wind_data <- purrr::map(date_range_wind, function(current_date){
    print(current_date)
    # Map over all time periods in each data
    
    tmp1 <- purrr::map(c(00 , 03 , 06 , 09 , 12 , 15 , 18 , 21),function(time_step){
      tmp2 <- wind.dl(year(current_date),month(current_date),day(current_date),time_step,0,355.5,-90,90)
      # Check for nulls - important since not all dates have data. Skip dates that don't
      if(is.null(tmp2)) return()
      print(time_step)
      tmp2 %>%
        wind.fit() %>%
        # Group into bins, take mean
        mutate(lon_bin = floor(lon*one_over_cellsize)/one_over_cellsize,
               lat_bin = floor(lat*one_over_cellsize)/one_over_cellsize) %>%
        group_by(lon_bin,lat_bin)  %>%
        summarize(dir = mean(dir),
                  speed = mean(speed))%>%
        bind_rows() %>%
        ungroup()
    }
    ) 
    # Remove nulls
    tmp1 <- tmp1[!sapply(tmp1, is.null)]
    # If they're all nulls, skip
    if(length(tmp1) == 0) return()
    # Bind together whatever's left
    if(length(tmp1) > 1) tmp1 <- do.call("rbind", tmp1) else tmp1 <- tmp1[[1]]
    
    # For all time steps, take mean for each bin
    tmp1 %>% bind_rows %>%
      group_by(lon_bin,lat_bin)  %>%
      summarize(dir = mean(dir),
                speed = mean(speed))  %>%
      rename(direction_degrees = dir,
             speed_m_s = speed) %>%
      ungroup()  %>%
      mutate(date = current_date) 
    
  }
  ) 
  # Bind together dataframe
  if(length(wind_data) > 1) wind_data <- do.call("rbind", wind_data) else wind_data <- wind_data[[1]]
  
  
  write_csv(wind_data,paste0("../processed_data/wind_data-",x,".csv"))
})

