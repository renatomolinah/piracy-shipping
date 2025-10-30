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
panel <- function(res) {
  readRDS(here("data", "processed", paste0("ev_panel_", res, ".rds"))) |>
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
run_estimation <- function(outcome_var, hotspot = "Global", res) {
# If hotspot is provided, we filter the data to only include the hotspot
# If hotspot is not provided, we use the entire dataset
  if(hotspot != "Global") {
    data <- panel(res) |> filter(attack_cluster == hotspot)
  } else {
    data <- panel(res)
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
hotspots <- c("Global", sort(unique(panel("0_5")$attack_cluster)))

# Run post-regressions for different outcome variables using conleyreg with asinh transformation
models <- expand_grid(outcome_var = outcome_vars,
                      hotspot = hotspots,
                      res = c("0_1", "0_5", "1")) |>
  filter(!hotspot == "None") |>
  mutate(model = pmap(.l = list(outcome_var, hotspot, res), run_estimation),
         coefficients = map(model, coefficients),
         n = map_dbl(model, nobs))

names(models$coefficients) <- models$hotspot

# Robustness on AIS disabling events
AIS_disab_models <- expand_grid(outcome_var = "asinh(n_ais_disabling)",
                                hotspot = c("Global", "G. of Aden", "G. of Guinea"),
                                res = "0_5") |>
  mutate(model = pmap(.l = list(outcome_var, hotspot, res), run_estimation),
         coefficients = map(model, coefficients),
         n = map_dbl(model, nobs))

names(AIS_disab_models$coefficients) <- AIS_disab_models$hotspot

# =============================================================================
# EXPORT ALL MODELS
# # =============================================================================
save(models,
     file = here("data", "output", "gridcell_models.RData"))
