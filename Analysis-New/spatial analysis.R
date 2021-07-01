# Load packages
library("tidyverse")
theme_set(theme_bw())
library(sp)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(smoothr)
library(lubridate)
library(fossil)
library(fixest)
sf_use_s2(FALSE)

# Load world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# Characterize  Gulf of Guinea
gulf_guinea <- world %>%
  filter(
    iso_a3 == "LBR" |
    iso_a3 == "CIV" |
    iso_a3 == "GHA" |
    iso_a3 == "TGO" |
    iso_a3 == "BEN" |
    iso_a3 == "NGA" |
    iso_a3 == "CMR" |
    iso_a3 == "GNQ" |
    iso_a3 == "GAB" 
     ) %>%
  st_transform("ESRI:54009") %>%
  summarise()

# Plot just for checking
gulf_guinea %>% 
  ggplot() + geom_sf()

# Read in EEZ data
# Marine Regions V9
eez <- read_sf(dsn = "World_EEZ_v9_20161021_LR", layer = "eez_lr", stringsAsFactors = FALSE) %>%
  st_make_valid()

## load anti-shipping data
# Data from http://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_pageLabel=msi_portal_page_65
## Process dates, and filter to only 2011 - 2017, to match GFW data
# Join to eez data to figure out which eez each attack happens in

attacks <- read_sf(dsn = "ASAM_shp", layer = "ASAM 20 MAR 18", stringsAsFactors = FALSE) %>%
  st_make_valid() %>%
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

attacks$Territory1[which(attacks$Territory1=="Federal Republic of Somalia")] = "Somalia"

# Some plots

attacks %>%
  ggplot() + 
  geom_sf() +
  geom_sf(data = world) +
  coord_sf(xlim = c(-25, 80),
           clip = "on")
  


gulf_guinea %>% ggplot() + geom_sf()

attacks %>% 
  filter(attack_eez == "LBR" |
           attack_eez == "CIV" |
           attack_eez == "GHA" |
           attack_eez == "TGO" |
           attack_eez == "BEN" |
           attack_eez == "NGA" |
           attack_eez == "CMR" |
           attack_eez == "GNQ" |
           attack_eez == "GAB") %>%
  ggplot() + geom_sf() + geom_sf(data = gulf_guinea)



# Match EEZ and attacks 

# Load indicators
indicators <- read.csv("~/Box/Proyectos/piracy-shipping/Analysis-New/PiracyIndicatorsPanelData.csv") %>%
  transform(ef_os = as.numeric(ef_os),
            ps = as.numeric(ps))

# Aggregate attacks by EEZ

db <- attacks %>%
  as_tibble() %>%
  select(Year, Aggressor, Victim,Territory1) %>%
  rename(year = Year,
         agg = Aggressor,
         vic = Victim,
         country = Territory1) %>%
  filter(!is.na(country)) %>%
  group_by(year, country) %>%
  summarise(attacks = n()) %>%
  ungroup() %>% 
  complete(year, country, fill = list(attacks = 0))
  
# Merge attacks/EEZ with socioeconomic indicators

db <- db %>% inner_join(indicators) 

head(db)

###############################################################################################################################################################


# Establishing validity of first stage


m1 = feols(attacks ~ icd_1000, db)

m2 = feols(attacks ~ ef_os, db)

m3 = feols(attacks ~ ps, db)

m4 = feols(attacks ~ unem, db)

m5 = feols(attacks ~ icd_1000 + ef_os + ps + ef_os + unem, db)

etable(m1, m2, m3, m4, m5)

# Political stability appears as the most suitable candidate. Dropping the other variables 

m1 = feols(attacks ~ ps, db)

m2 = feols(attacks ~ ps | year, db)

m3 = feols(attacks ~ gulf | year, db)

m4 = feols(attacks ~ ps + gulf | year, db)

etable(m1, m2, m3, m4)



###############################################################################################################################################################