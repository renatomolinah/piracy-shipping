################################################################################
#
# Estimates gridcell analysis in event study and post approaches
#
################################################################################

# Load packages
library(here)
library(conleyreg)
library(tidyverse)
library(patchwork)
library(fixest)
library(modelsummary)

# =============================================================================
# 1. HELPER FUNCTIONS
# =============================================================================

load_ev_panel <- function() {
  panel <- readRDS(here("data", "processed", "ev_panel.rds"))
  return(panel)
}

# =============================================================================
# 2. PRE/POST-REGRESSION ANALYSIS
# =============================================================================

# Set up dictionary for variable names
setFixest_dict(c(
  post = "Post-Attack",
  "time_hours/n_vessels" = "Time per Vessel (hrs)",
  "distance_km/n_vessels" = "Distance per Vessel (km)",
  time_hours = "Total Time (hrs)",
  distance_km = "Total Distance (km)",
  n_vessels = "Number of Vessels"
))

# Run post-regressions for different outcome variables using conleyreg with asinh transformation
m1_total_time <- conleyreg(
  asinh(time_hours) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m2_total_distance <- conleyreg(
  asinh(distance_km) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m3_time_per_vessel <- conleyreg(
  asinh(time_hours/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m4_distance_per_vessel <- conleyreg(
  asinh(distance_km/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

obs_reg_1 <- feols(
  asinh(time_hours) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )

obs_reg_2 <- feols(
  asinh(distance_km) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )

obs_reg_3 <- feols(
  asinh(time_hours/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )

obs_reg_4 <- feols(
  asinh(distance_km/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )


# Create rows for additional information
rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Observations", as.character(nobs(obs_reg_1) %>% format(big.mark = ",")),
  as.character(nobs(obs_reg_2) %>% format(big.mark = ",")),
  as.character(nobs(obs_reg_3) %>% format(big.mark = ",")),
  as.character(nobs(obs_reg_4) %>% format(big.mark = ",")),
  "", "", "", "", "",
  "Grid Cell FE", "X", "X", "X", "X",
  "Year-Month FE", "X", "X", "X", "X",
  "Day of Week FE", "X", "X", "X", "X",
  "Subregion FE", "X", "X", "X", "X"
)

# =============================================================================
# EXPORT ALL MODELS
# # =============================================================================
save(m1_total_time, m2_total_distance, m3_time_per_vessel, m4_distance_per_vessel,
     rows, file = here("data", "output", "gridcell_models", "gridcell_models.RData"))
