################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Event study but using did_multiplegt_dyn
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(DIDmultiplegtDYN)
library(tidyverse)
library(furrr)
library(here)


# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data",
                                        "attacks_and_activity_by_grid.rds")) %>% 
  mutate(time_hours = asinh(time_hours),
         time_vessel = asinh(time_hours / n_vessels),
         time_trip = asinh(time_hours / n_trips),
         distance_km = asinh(distance_km),
         distance_vessel = asinh(distance_km / n_vessels),
         distance_trip = asinh(distance_km / n_trips),
         n_vessels = asinh(n_vessels),
         n_trips = asinh(n_trips))

## PROCESSING ##################################################################

effects <- 7
placebos <- 5
group <- "grid_id"
time <- "date"
treatment <- "number_previous_attacks_grid_1_month"

wrap <- function(outcome) {
  reg <- did_multiplegt_dyn(df = grid_level_panel,
                            outcome = outcome,
                            effects = effects,
                            placebo = placebos,
                            group = group,
                            time = time,
                            treatment = treatment)
  
  saveRDS(reg, file = here("output_data", paste0("es_mod_", outcome, ".rds")))
}

outcomes <- c(#"time_hours",
              "time_vessel",
              "time_trip",
              "distance_km",
              "distance_vessel",
              "distance_trip",
              "n_vessels",
              "n_trips")

plan("multisession", workers = 8)
future_walk(outcomes, wrap)
plan(sequential)


## VISUALIZE ###################################################################
files <- list.files("output_data", pattern = "es_mod", full.names = T)
mods <- files %>% 
  map(readRDS) %>% 
  set_names(basename(files)) %>% 
  map(pluck, 4) %>% 
  map(function(x){x + geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = "dashed") + labs(title = "")})


theme_set(theme_linedraw())

# X ----------------------------------------------------------------------------
cowplot::plot_grid(plotlist = mods,
                   labels = str_remove_all(names(mods), "es_mod_|\\.rds"),
                   ncol = 3)

