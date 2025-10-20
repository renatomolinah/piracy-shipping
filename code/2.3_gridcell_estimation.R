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
panel <- function() {
  readRDS(here("data", "processed", "ev_panel.rds")) |>
    mutate(time_vessels = time_hours/n_vessels,
           dist_vessels = distance_km/n_vessels,
           attack_cluster = case_when(attack_cluster == "GoA" ~ "G. of Aden",
                                      attack_cluster == "GoG" ~ "G. of Guinea",
                                      attack_cluster == "SEA" ~ "S.E. Asia",
                                      T ~ attack_cluster))
}


# =============================================================================
# 2. PRE/POST-REGRESSION ANALYSIS
# =============================================================================

# A function to perform the estimation
run_estimation <- function(outcome_var, hotspot = "Global") {
# If hotspot is provided, we filter the data to only include the hotspot
# If hotspot is not provided, we use the entire dataset
  if(hotspot != "Global") {
    data <- panel() |> filter(attack_cluster == hotspot)
  } else {
    data <- panel()
  }

fml <- as.formula(paste(outcome_var, "~ post | id + year^month + day_of_week + asam_subregion"))

# Run the estimation
results <- conleyreg(formula = fml,
                     unit = "id",
                     time = "date",
                     lat = "lat_bin",
                     lon = "lon_bin",
                     data = data,
                     dist_cutoff = 50,
                     lag_cutoff = Inf,
                     gof = TRUE)

return(results)
}

# Outcomes of interest
outcome_vars <- c("asinh(time_hours)",
                  "asinh(distance_km)",
                  "asinh(n_vessels)",
                  "asinh(n_trips)",
                  "asinh(time_vessels)",
                  "asinh(dist_vessels)")

# Sub-sample specifications
hotspots <- c("Global", sort(unique(panel()$attack_cluster)))

# Run post-regressions for different outcome variables using conleyreg with asinh transformation
models <- expand_grid(outcome_var = outcome_vars,
                      hotspot = hotspots) |>
  filter(!hotspot == "None") |>
  mutate(model = map2(outcome_var, hotspot, run_estimation),
         coefficients = map(model, coefficients),
         n = map_dbl(model, nobs))

names(models$coefficients) <- models$hotspot

# =============================================================================
# EXPORT ALL MODELS
# # =============================================================================
save(models,
     file = here("data", "output", "gridcell_models.RData"))
