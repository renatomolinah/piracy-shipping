rm(list = ls(all.names = TRUE))

require(tidyverse)
library(lubridate)
library(zoo)
require(bigmemory)
library(stringr)
library(magrittr)
library(haven)
library(data.table)


trip_summary <- read.csv("~/Box Sync/Proyectos/piracy-shipping/Data sets/trip_hotspot_summary_full.csv")

# 3 degree file

setwd("~/Box Sync/Proyectos/piracy-shipping/Data sets/3 degree")

WDB <- rbindlist(lapply(list.files(), fread))

WDB <- WDB %>%
  filter(from_country!="",to_country!="") %>%
  mutate(speed_test = implied_speed_knots/design_speed_knots,
         distance_test = total_distance_km/total_haversine_distance_km)

WDB <- WDB %>% mutate(month = month(departure_date),
                      year = year(departure_date),
                      cport = paste(pmin(from_port,to_port), pmax(from_port,to_port)),
                      ccountry = paste(pmin(from_country,to_country), pmax(from_country,to_country)),
                      speed = total_distance_km/total_hours,
                      size = ntile(tonnage_gt,10),
                      hotspot_speed = attack_grid_distance_km/attack_grid_hours)

# Create variables for attacks over time  

WDB <- WDB %>% 
  mutate(attacks_1m = attacks_window_last_1_month,
         attacks_2m = attacks_window_last_2_month-attacks_window_last_1_month,
         attacks_3m = attacks_window_last_3_month-attacks_window_last_2_month,
         attacks_4m = attacks_window_last_4_month-attacks_window_last_3_month,
         attacks_5m = attacks_window_last_5_month-attacks_window_last_4_month,
         attacks_6m = attacks_window_last_6_month-attacks_window_last_5_month,
         attacks_7m = attacks_window_last_7_month-attacks_window_last_6_month,
         attacks_8m = attacks_window_last_8_month-attacks_window_last_7_month,
         attacks_9m = attacks_window_last_9_month-attacks_window_last_8_month,
         attacks_10m = attacks_window_last_10_month-attacks_window_last_9_month,
         attacks_11m = attacks_window_last_11_month-attacks_window_last_10_month,
         attacks_12m = attacks_window_last_12_month-attacks_window_last_11_month,
         attacks_13m = attacks_window_last_13_month-attacks_window_last_12_month,
         attacks_14m = attacks_window_last_14_month-attacks_window_last_13_month,
         attacks_15m = attacks_window_last_15_month-attacks_window_last_14_month,
         attacks_16m = attacks_window_last_16_month-attacks_window_last_15_month,
         attacks_17m = attacks_window_last_17_month-attacks_window_last_16_month,
         attacks_18m = attacks_window_last_18_month-attacks_window_last_17_month,
         attacks_19m = attacks_window_last_19_month-attacks_window_last_18_month,
         attacks_20m = attacks_window_last_20_month-attacks_window_last_19_month,
         attacks_21m = attacks_window_last_21_month-attacks_window_last_20_month,
         attacks_22m = attacks_window_last_22_month-attacks_window_last_21_month,
         attacks_23m = attacks_window_last_23_month-attacks_window_last_22_month,
         attacks_24m = attacks_window_last_24_month-attacks_window_last_23_month,
         
         attacks_f1m = attacks_window_next_1_month,
         attacks_f2m = attacks_window_next_2_month-attacks_window_next_1_month,
         attacks_f3m = attacks_window_next_3_month-attacks_window_next_2_month,
         attacks_f4m = attacks_window_next_4_month-attacks_window_next_3_month,
         attacks_f5m = attacks_window_next_5_month-attacks_window_next_4_month,
         attacks_f6m = attacks_window_next_6_month-attacks_window_next_5_month,
         attacks_f7m = attacks_window_next_7_month-attacks_window_next_6_month,
         attacks_f8m = attacks_window_next_8_month-attacks_window_next_7_month,
         attacks_f9m = attacks_window_next_9_month-attacks_window_next_8_month,
         attacks_f10m = attacks_window_next_10_month-attacks_window_next_9_month,
         attacks_f11m = attacks_window_next_11_month-attacks_window_next_10_month,
         attacks_f12m = attacks_window_next_12_month-attacks_window_next_11_month,
         attacks_f13m = attacks_window_next_13_month-attacks_window_next_12_month,
         attacks_f14m = attacks_window_next_14_month-attacks_window_next_13_month,
         attacks_f15m = attacks_window_next_15_month-attacks_window_next_14_month,
         attacks_f16m = attacks_window_next_16_month-attacks_window_next_15_month,
         attacks_f17m = attacks_window_next_17_month-attacks_window_next_16_month,
         attacks_f18m = attacks_window_next_18_month-attacks_window_next_17_month,
         attacks_f19m = attacks_window_next_19_month-attacks_window_next_18_month,
         attacks_f20m = attacks_window_next_20_month-attacks_window_next_19_month,
         attacks_f21m = attacks_window_next_21_month-attacks_window_next_20_month,
         attacks_f22m = attacks_window_next_22_month-attacks_window_next_21_month,
         attacks_f23m = attacks_window_next_23_month-attacks_window_next_22_month,
         attacks_f24m = attacks_window_next_24_month-attacks_window_next_23_month)

# Identify most common combination across countries 

combos <- WDB %>% 
  group_by(ccountry) %>%
  count(cport, sort = TRUE) %>%
  top_n(1) %>%
  filter(row_number()==1) %>%
  rename(top_combo = cport) %>%
  select(ccountry,top_combo)

# Merge and create database with most common connection between countries

WDB <- WDB %>% 
  left_join(combos ,by = 'ccountry') %>% 
  mutate(top_route = if_else(cport == top_combo,1,0))

WDB[is.na(WDB)] <- "."
WDB[WDB == Inf] <- "."

WDB <- left_join(WDB, trip_summary, by = "trip_id")

# Final manipulations

WDB <- WDB %>%
  rename(
    wind = wind_speed_m_per_s,
    total_fuel = total_fuel_consumption_mt_voyage,
    guinea = hotspot_gulf_of_guinea,
    asia = hotspot_southeast_asia,
    aden = hotspot_gulf_of_aden, 
    country = ccountry
  ) %>%
  mutate(
    labor_cost = crew * total_hours * 5.55/1000,
    fuel_cost = total_hours * 2 * 450/1000,
    total_cost = fuel_cost + labor_cost,
    total_co2 = emissions_co2_mt_voyage,
    total_nox = emissions_nox_kg_voyage,
    total_sox = emissions_sox_kg_voyage,
    total_fuel_cost = total_fuel * 0.450,
    attack = ifelse(attacks_1y>0, 1,0)
  )






fwrite(WDB,file="WDB_3.csv")

# 5 degree file

setwd("~/Box Sync/Proyectos/piracy-shipping/Data sets/5 degree")

WDB <- rbindlist(lapply(list.files(), fread))

WDB <- WDB %>%
  filter(from_country!="",to_country!="") %>%
  mutate(speed_test = implied_speed_knots/design_speed_knots,
         distance_test = total_distance_km/total_haversine_distance_km)

WDB <- WDB %>% mutate(month = month(departure_date),
                      year = year(departure_date),
                      cport = paste(pmin(from_port,to_port), pmax(from_port,to_port)),
                      ccountry = paste(pmin(from_country,to_country), pmax(from_country,to_country)),
                      speed = total_distance_km/total_hours,
                      size = ntile(tonnage_gt,10),
                      hotspot_speed = attack_grid_distance_km/attack_grid_hours)

# Create dummies for attacks over time  

WDB <- WDB %>% 
  mutate(attacks_1m = attacks_window_last_1_month,
         attacks_2m = attacks_window_last_2_month-attacks_window_last_1_month,
         attacks_3m = attacks_window_last_3_month-attacks_window_last_2_month,
         attacks_4m = attacks_window_last_4_month-attacks_window_last_3_month,
         attacks_5m = attacks_window_last_5_month-attacks_window_last_4_month,
         attacks_6m = attacks_window_last_6_month-attacks_window_last_5_month,
         attacks_7m = attacks_window_last_7_month-attacks_window_last_6_month,
         attacks_8m = attacks_window_last_8_month-attacks_window_last_7_month,
         attacks_9m = attacks_window_last_9_month-attacks_window_last_8_month,
         attacks_10m = attacks_window_last_10_month-attacks_window_last_9_month,
         attacks_11m = attacks_window_last_11_month-attacks_window_last_10_month,
         attacks_12m = attacks_window_last_12_month-attacks_window_last_11_month,
         attacks_13m = attacks_window_last_13_month-attacks_window_last_12_month,
         attacks_14m = attacks_window_last_14_month-attacks_window_last_13_month,
         attacks_15m = attacks_window_last_15_month-attacks_window_last_14_month,
         attacks_16m = attacks_window_last_16_month-attacks_window_last_15_month,
         attacks_17m = attacks_window_last_17_month-attacks_window_last_16_month,
         attacks_18m = attacks_window_last_18_month-attacks_window_last_17_month,
         attacks_19m = attacks_window_last_19_month-attacks_window_last_18_month,
         attacks_20m = attacks_window_last_20_month-attacks_window_last_19_month,
         attacks_21m = attacks_window_last_21_month-attacks_window_last_20_month,
         attacks_22m = attacks_window_last_22_month-attacks_window_last_21_month,
         attacks_23m = attacks_window_last_23_month-attacks_window_last_22_month,
         attacks_24m = attacks_window_last_24_month-attacks_window_last_23_month,
         
         attacks_f1m = attacks_window_next_1_month,
         attacks_f2m = attacks_window_next_2_month-attacks_window_next_1_month,
         attacks_f3m = attacks_window_next_3_month-attacks_window_next_2_month,
         attacks_f4m = attacks_window_next_4_month-attacks_window_next_3_month,
         attacks_f5m = attacks_window_next_5_month-attacks_window_next_4_month,
         attacks_f6m = attacks_window_next_6_month-attacks_window_next_5_month,
         attacks_f7m = attacks_window_next_7_month-attacks_window_next_6_month,
         attacks_f8m = attacks_window_next_8_month-attacks_window_next_7_month,
         attacks_f9m = attacks_window_next_9_month-attacks_window_next_8_month,
         attacks_f10m = attacks_window_next_10_month-attacks_window_next_9_month,
         attacks_f11m = attacks_window_next_11_month-attacks_window_next_10_month,
         attacks_f12m = attacks_window_next_12_month-attacks_window_next_11_month,
         attacks_f13m = attacks_window_next_13_month-attacks_window_next_12_month,
         attacks_f14m = attacks_window_next_14_month-attacks_window_next_13_month,
         attacks_f15m = attacks_window_next_15_month-attacks_window_next_14_month,
         attacks_f16m = attacks_window_next_16_month-attacks_window_next_15_month,
         attacks_f17m = attacks_window_next_17_month-attacks_window_next_16_month,
         attacks_f18m = attacks_window_next_18_month-attacks_window_next_17_month,
         attacks_f19m = attacks_window_next_19_month-attacks_window_next_18_month,
         attacks_f20m = attacks_window_next_20_month-attacks_window_next_19_month,
         attacks_f21m = attacks_window_next_21_month-attacks_window_next_20_month,
         attacks_f22m = attacks_window_next_22_month-attacks_window_next_21_month,
         attacks_f23m = attacks_window_next_23_month-attacks_window_next_22_month,
         attacks_f24m = attacks_window_next_24_month-attacks_window_next_23_month)

# Identify most common combination across countries 

combos <- WDB %>% 
  group_by(ccountry) %>%
  count(cport, sort = TRUE) %>%
  top_n(1) %>%
  filter(row_number()==1) %>%
  rename(top_combo = cport) %>%
  select(ccountry,top_combo)

# Merge and create database with most common connection between countries

WDB <- WDB %>% 
  left_join(combos ,by = 'ccountry') %>% 
  mutate(top_route = if_else(cport == top_combo,1,0))

WDB[is.na(WDB)] <- "."
WDB[WDB == Inf] <- "."

WDB <- left_join(WDB, trip_summary, by = "trip_id")

fwrite(WDB,file="WDB_5.csv")


# 7 degree file

setwd("~/Box Sync/Proyectos/piracy-shipping/Data sets/7 degree")

WDB <- rbindlist(lapply(list.files(), fread))

WDB <- WDB %>%
  filter(from_country!="",to_country!="") %>%
  mutate(speed_test = implied_speed_knots/design_speed_knots,
         distance_test = total_distance_km/total_haversine_distance_km)

WDB <- WDB %>% mutate(month = month(departure_date),
                      year = year(departure_date),
                      cport = paste(pmin(from_port,to_port), pmax(from_port,to_port)),
                      ccountry = paste(pmin(from_country,to_country), pmax(from_country,to_country)),
                      speed = total_distance_km/total_hours,
                      size = ntile(tonnage_gt,10),
                      hotspot_speed = attack_grid_distance_km/attack_grid_hours)

# Create dummies for attacks over time  

WDB <- WDB %>% 
  mutate(attacks_1m = attacks_window_last_1_month,
         attacks_2m = attacks_window_last_2_month-attacks_window_last_1_month,
         attacks_3m = attacks_window_last_3_month-attacks_window_last_2_month,
         attacks_4m = attacks_window_last_4_month-attacks_window_last_3_month,
         attacks_5m = attacks_window_last_5_month-attacks_window_last_4_month,
         attacks_6m = attacks_window_last_6_month-attacks_window_last_5_month,
         attacks_7m = attacks_window_last_7_month-attacks_window_last_6_month,
         attacks_8m = attacks_window_last_8_month-attacks_window_last_7_month,
         attacks_9m = attacks_window_last_9_month-attacks_window_last_8_month,
         attacks_10m = attacks_window_last_10_month-attacks_window_last_9_month,
         attacks_11m = attacks_window_last_11_month-attacks_window_last_10_month,
         attacks_12m = attacks_window_last_12_month-attacks_window_last_11_month,
         attacks_13m = attacks_window_last_13_month-attacks_window_last_12_month,
         attacks_14m = attacks_window_last_14_month-attacks_window_last_13_month,
         attacks_15m = attacks_window_last_15_month-attacks_window_last_14_month,
         attacks_16m = attacks_window_last_16_month-attacks_window_last_15_month,
         attacks_17m = attacks_window_last_17_month-attacks_window_last_16_month,
         attacks_18m = attacks_window_last_18_month-attacks_window_last_17_month,
         attacks_19m = attacks_window_last_19_month-attacks_window_last_18_month,
         attacks_20m = attacks_window_last_20_month-attacks_window_last_19_month,
         attacks_21m = attacks_window_last_21_month-attacks_window_last_20_month,
         attacks_22m = attacks_window_last_22_month-attacks_window_last_21_month,
         attacks_23m = attacks_window_last_23_month-attacks_window_last_22_month,
         attacks_24m = attacks_window_last_24_month-attacks_window_last_23_month,
         
         attacks_f1m = attacks_window_next_1_month,
         attacks_f2m = attacks_window_next_2_month-attacks_window_next_1_month,
         attacks_f3m = attacks_window_next_3_month-attacks_window_next_2_month,
         attacks_f4m = attacks_window_next_4_month-attacks_window_next_3_month,
         attacks_f5m = attacks_window_next_5_month-attacks_window_next_4_month,
         attacks_f6m = attacks_window_next_6_month-attacks_window_next_5_month,
         attacks_f7m = attacks_window_next_7_month-attacks_window_next_6_month,
         attacks_f8m = attacks_window_next_8_month-attacks_window_next_7_month,
         attacks_f9m = attacks_window_next_9_month-attacks_window_next_8_month,
         attacks_f10m = attacks_window_next_10_month-attacks_window_next_9_month,
         attacks_f11m = attacks_window_next_11_month-attacks_window_next_10_month,
         attacks_f12m = attacks_window_next_12_month-attacks_window_next_11_month,
         attacks_f13m = attacks_window_next_13_month-attacks_window_next_12_month,
         attacks_f14m = attacks_window_next_14_month-attacks_window_next_13_month,
         attacks_f15m = attacks_window_next_15_month-attacks_window_next_14_month,
         attacks_f16m = attacks_window_next_16_month-attacks_window_next_15_month,
         attacks_f17m = attacks_window_next_17_month-attacks_window_next_16_month,
         attacks_f18m = attacks_window_next_18_month-attacks_window_next_17_month,
         attacks_f19m = attacks_window_next_19_month-attacks_window_next_18_month,
         attacks_f20m = attacks_window_next_20_month-attacks_window_next_19_month,
         attacks_f21m = attacks_window_next_21_month-attacks_window_next_20_month,
         attacks_f22m = attacks_window_next_22_month-attacks_window_next_21_month,
         attacks_f23m = attacks_window_next_23_month-attacks_window_next_22_month,
         attacks_f24m = attacks_window_next_24_month-attacks_window_next_23_month)

# Identify most common combination across countries 

combos <- WDB %>% 
  group_by(ccountry) %>%
  count(cport, sort = TRUE) %>%
  top_n(1) %>%
  filter(row_number()==1) %>%
  rename(top_combo = cport) %>%
  select(ccountry,top_combo)

# Merge and create database with most common connection between countries

WDB <- WDB %>% 
  left_join(combos ,by = 'ccountry') %>% 
  mutate(top_route = if_else(cport == top_combo,1,0))

WDB[is.na(WDB)] <- "."
WDB[WDB == Inf] <- "."

WDB <- left_join(WDB, trip_summary, by = "trip_id")

fwrite(WDB,file="WDB_7.csv")

