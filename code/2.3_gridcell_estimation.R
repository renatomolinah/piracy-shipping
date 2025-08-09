################################################################################
#
# Estimates gridcell analysis in event study and post approaches
#
################################################################################

# Load packages
library(here)
library(conleyreg)
library(tidyverse)
library(fixest)

# =============================================================================
# 1. LOAD PANEL DATA
# =============================================================================
panel <- readRDS(here("data", "processed", "ev_panel.rds"))


# =============================================================================
# 2. PRE/POST-REGRESSION ANALYSIS
# =============================================================================

# Run post-regressions for different outcome variables using conleyreg with asinh transformation
m1_total_time <- conleyreg(
  asinh(time_hours) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = panel,
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m2_total_distance <- conleyreg(
  asinh(distance_km) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = panel,
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m3_time_per_vessel <- conleyreg(
  asinh(time_hours/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = panel,
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m4_distance_per_vessel <- conleyreg(
  asinh(distance_km/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = panel,
  dist_cutoff = 50,
  lag_cutoff = Inf
)

# Conleyreg doesn't return the number of observations, which we'll need for the tables
# So now we use fixest to fit a model with the same specification, and we'll extract the number of observations
# into a data.frame to be exported along with the models.
obs_reg_1 <- feols(
  asinh(time_hours) ~ post | id + year^month + day_of_week + asam_subregion,
  data = panel
  )

obs_reg_2 <- feols(
  asinh(distance_km) ~ post | id + year^month + day_of_week + asam_subregion,
  data = panel
  )

obs_reg_3 <- feols(
  asinh(time_hours/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  data = panel
  )

obs_reg_4 <- feols(
  asinh(distance_km/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  data = panel
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
save(panel,
     m1_total_time,
     m2_total_distance,
     m3_time_per_vessel,
     m4_distance_per_vessel,
     rows, file = here("data", "output", "gridcell_models.RData"))
