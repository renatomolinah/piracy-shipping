# Libraries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidylog, here, fst, data.table, 
               fixest)
# Data --------------------------------------------------------------------

# Load dataset (takes a while)
# Previously filtered to only include trips that pass through hotspots,
# as well as trips that are consistent with vessel design (not too fast - not too long).

wdb = read_fst(here("data/wdb.fst")); setDT(wdb)

## Remove a bunch of unnecessary columns (to free up memory)
rcols = grep(paste(setdiff(1:24, seq(3, 12, 3)), collapse="|"), 
             names(wdb), value = TRUE)
rcols = rcols[!grepl("12|total_co2", rcols)] ## Minor catches for 12mo periods and co2
wdb[, (rcols) := NULL]
rm(rcols); gc()

## Cheaper to have 1/0 vars as TRUE/FALSE
bvars = c("guinea", "aden", "asia")
wdb[ , (bvars) := lapply(.SD, as.logical), .SDcols = bvars]
rm(bvars)

wdb[, hotspot := fcase(guinea, "Guinea",
                       aden, "Aden",
                       asia, "Asia",
                       default = "None")]
setnames(wdb,
         old = c("total_distance_km", "total_hours",
                 paste0("attacks_window_last_", seq(3, 12, 3), "_month"),
                 paste0("hotspot_attacks_window_last", seq(3, 12, 3), "_month")),
         new = c("distance", "time",
                 paste0("attacks_past_", seq(3, 12, 3)),
                 paste0("h_attacks_past_", seq(3, 12, 3))))

wdb[, `:=` (odds_past_3 = h_attacks_past_3/unique_hotspot_vessels_last_3_month,
            odds_past_6 = h_attacks_past_6/unique_hotspot_vessels_last_6_month,
            odds_past_9 = h_attacks_past_9/unique_hotspot_vessels_last_9_month,
            odds_past_12 = h_attacks_past_12/unique_hotspot_vessels_last_12_month)]

indicator_attacks <- read.csv(here("NT_E.csv")) %>%
  std_state = str_to_lower(std_state)

NT_E <- read_csv("NT_E.csv")

################################################################################
# Sub-setting trips around Africa to match with socioeconomic indicators
################################################################################

# Drop voyages not passing through Guinea or Aden

wdb <- wdb %>% filter(guinea | aden) 

# Group voyages by year, origin, country, hotspot and port combination

agg_trips <- wdb %>% 
  filter(top_route == 1) %>% # keep only the most common port-to-port connection
  group_by(year, orig, guinea, country, cport) %>%
    summarise(speed = mean(speed),
              dist = mean(distance/1000),
              days = mean(time/24),
              n_trips = n(),
              hotspot = ifelse(guinea, "guinea", "aden"))

agg_fs <- NT_E %>%
  rename(hotspot = gulf) %>%
  group_by(year, hotspot) %>%
  summarise(attacks = sum(attacks),
            political_stability = mean(ps))

reg1 <- lm(attacks ~ political_stability + hotspot, data = agg_fs)
summary(reg1)

## Working on this. Need to think what the appropriate level of aggregation is
## 

