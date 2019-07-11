library(data.table)
library(readr)
setwd("~/Downloads/fixed")
data <- rbindlist(lapply(list.files(), fread))
data_processed <- data %>%
  dplyr::select(-c("main_fuel_consumption_mt_voyage","aux_fuel_consumption_mt_voyage",  
                   "total_fuel_consumption_mt_voyage","total_fuel_cost_usd_voyage",      
                   "emissions_co2_kg_voyage","emissions_nox_kg_voyage",         
                   "emissions_sox_kg_voyage","main_sfc_g_per_kWH","aux_sfc_g_per_kWH",
                   "mean_distance","sd_distance",                   
                   "min_distance_cutoff_2sd","max_distance_cutoff_2sd",       
                  "min_distance_cutoff_3sd","max_distance_cutoff_3sd",
                  "wind_speed_m_per_s","wind_direction_degrees",
                  "departure_timestamp","arrival_timestamp"))
fwrite(data_processed,file="fixed_data_all.csv")
