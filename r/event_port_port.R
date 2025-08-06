# Load packages ----------------------------------------------------------------
library(DIDmultiplegtDYN)
library(tidyr)
library(dplyr)
library(data.table)
library(here)

setDTthreads(threads = 15)

# Load data --------------------------------------------------------------------
panel <- readRDS(file = here("processed_data/daily_attacks_and_activity_for_event_study.rds")) |> 
  mutate(n_trips = asinh(n_trips),
         id = paste(from_country, from_port, to_country, to_port, sep = "_")) |> 
  slice_sample(by = id, prop = 0.1) %>% 
  select(n_trips, id, date, days_with_attack)

gc()
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
b <- did_multiplegt_dyn(df = panel,
                        effects = 5,
                        placebo = 5,
                        outcome = "n_trips",
                        group = "id",
                        time = "date",
                        treatment = "days_with_attack")

saveRDS(b, "b_port_to_port.rds")

b <- readRDS("b_port_to_port.rds")
