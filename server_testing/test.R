################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################
setwd("./piracy")

# Load packages ----------------------------------------------------------------
library(DIDmultiplegtDYN)
library(tidyverse)
library(data.table)
# library(furrr)
library(here)

data.table::setDTthreads(192)

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data/attacks_and_activity_by_grid.rds")) %>%
  mutate(time_hours = asinh(time_hours),
         distance_km = asinh(distance_km),
         n_vessels = asinh(n_vessels),
         n_trips = asinh(n_trips))

## PROCESSING ##################################################################

effects <- 7
placebos <- 5
group <- "grid_id"
time <- "date"
treatment <- "number_previous_attacks_grid_1_month"

wrap <- function(outcome) {
  out_file <- here("output_data", paste0("es_mod_", outcome, ".rds"))

  if(!file.exists(out_file)) {
    print(paste("File", out_file, "not found, proceeding to estimate model"))
    reg <- did_multiplegt_dyn(df = grid_level_panel,
                              outcome = outcome,
                              effects = effects,
                              placebo = placebos,
                              group = group,
                              time = time,
                              treatment = treatment)

    saveRDS(reg, file = out_file)
  }
}

# outcomes <- c("time_hours",
#               "distance_km",
#               "n_vessels",
#               "n_trips")

# outcomes <- "time_hours"

wrap(outcome = "time_hours")

# plan("multisession", workers = 4)
# future_walk(outcomes, wrap)
# plan(sequential)


## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
