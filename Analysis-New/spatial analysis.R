################################################################################
# Pricing the effects of piracy on the shipping industry
# Author: Renato Molina
################################################################################

################################################################################
# Load packages
################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidylog, sp, sf, fixest, rnaturalearth,
               rnaturalearthdata, smoothr, lubridate, fossil, here)
options("tidylog.display" = NULL)
theme_set(theme_bw())
sf_use_s2(FALSE)

################################################################################
# Maps and assignments
################################################################################

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

# Plot guinea to make sure things look like they should
gulf_guinea %>% 
  ggplot() + geom_sf()

# Read in EEZ data
# Marine Regions V9
eez <- read_sf(dsn = "World_EEZ_v9_20161021_LR", layer = "eez_lr", 
               stringsAsFactors = FALSE) %>%
  st_make_valid()

################################################################################
# Load and match data on pirate encounters
################################################################################

# Data from http://msi.nga.mil/NGAPortal/MSI.portal?_nfpb=true&_pageLabel=msi_portal_page_65

# Process dates, and filter to only 2011 - 2017, to match GFW data

# Join to eez data to figure out in which eez each attack happens

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

# Change entrances for Somalia

attacks$Territory1[which(attacks$Territory1=="Federal Republic of Somalia")] = "Somalia"

# Visualize Gulf of Guinea

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


################################################################################
# Load and match data on pirate encounters
################################################################################

# Load socio-economic indicators

indicators <- read.csv(here("PiracyIndicatorsPanelData.csv")) %>%
  transform(ef_os = as.numeric(ef_os),
            ps = as.numeric(ps))

# Aggregate attacks by EEZ

db <- attacks %>%
  as_tibble() %>%
  select(Year, Aggressor, Victim, Territory1, attack_eez, ISO_Ter1) %>%
  rename(year = Year,
         agg = Aggressor,
         vic = Victim,
         country = Territory1,
         iso_code = ISO_Ter1
         ) %>%
  filter(!is.na(country)) %>%
  group_by(year, country) %>%
  summarise(attacks = n()) %>%
  ungroup() %>% 
  complete(year, country, fill = list(attacks = 0))
  
  
# Merge attacks/EEZ with socioeconomic indicators

db <- db %>% inner_join(indicators) 

head(db)

################################################################################
# First stage - Socio-Political indicators and pirate encounters
################################################################################

# Establishing validity of first stage

# icd_tot = Best estimate of total deaths due to internal conflict + internationalized internal conflict (data from UCDP/PRIO Armed Conflict Dataset)
# icd_1000 = Deaths due to internal conflict per 1000 people. Calculated using World Bank population estimates 
# ef_os = Overall score for the index of economic freedom 
# ps = World Bank's indicator for political stability and absence of violence/terrorism (-2.5 to +2.5)
# unem = Unemployment, percent of total labor force (World Bank modeled ILO estimate)

# Setting up a dictionary 
setFixest_dict(c(attacks = "Encounters (#)",
                 icd_1000 = "Conflict deaths PK", 
                 
                 ef_os = "Economic freedom",
                 
                 ps = "Political stability",
                 
                 unem = "Unemployemnt",

                 country = "Country",
                 
                 year = "Year"

))


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


asd <-  db %>% 
  group_by(year, gulf) %>%
  summarise(ps = mean(ps),
            attacks = sum(attacks))
  
db %>% ggplot(aes(x = asinh(ps), y = asinh(attacks)))  +geom_point()
  
  

################################################################################
# Save first stage set-up
################################################################################

# National indicators and encounters

write.csv(x = db, file = "NT_E.csv")

###############################################################################################################################################################