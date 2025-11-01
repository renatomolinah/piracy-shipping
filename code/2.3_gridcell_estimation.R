################################################################################
#
# Estimates gridcell analysis in event study and post approaches
#
################################################################################

# Load packages
library(here)
library(conleyreg)
library(tidyverse)

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
run_estimation <- function(outcome_var,
                           spec = "~ post | id + year^month + day_of_week",
                           hotspot = "Global",
                           res = "0_5") {
  # If hotspot is provided, we filter the data to only include the hotspot
  # If hotspot is not provided, we use the entire dataset
  if(hotspot != "Global") {
    data <- panel(res) |> filter(attack_cluster == hotspot)
  } else {
    data <- panel(res)
  }

  # Main specification formula
  fml <- as.formula(paste(outcome_var, spec))

  cat("Estimating for", outcome_var, "in", hotspot, "at a resolution of", res)


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


# Define all outcomes, specifications, resolutions and samples -----------------

# Outcomes of interest
outcome_vars <- c("asinh(time_hours)",
                  "asinh(distance_km)",
                  "asinh(n_vessels)",
                  "asinh(n_trips)",
                  "asinh(time_vessels)",
                  "asinh(dist_vessels)")

# All specifications
all_specs <- c("~ post",
               "~ post | id ",
               "~ post | id + year^month")

# Sub-sample specifications
hotspots <- c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia")

# Different resolutions
res <- c("0_1", "0_5", "1")


# Run main post-regressions ----------------------------------------------------
models <- expand_grid(outcome_var = outcome_vars,
                      hotspot = hotspots,
                      spec = "~ post | id + year^month + day_of_week",
                      res = res) |>
  mutate(model = pmap(.l = list(outcome_var = outcome_var, spec = spec, hotspot = hotspot, res = res), run_estimation),
         coefficients = map(model, coefficients),
         n = map_dbl(model, nobs))

names(models$coefficients) <- models$hotspot

# Run specification tables post-regressions ------------------------------------
spec_tables <- expand_grid(outcome_var = outcome_vars,
                           spec = all_specs) |>
  mutate(model = pmap(.l = list(outcome_var = outcome_var, spec = spec),
                      run_estimation,
                      hotspot = "Global",
                      res = "0_5"),
         coefficients = map(model, coefficients),
         n = map_dbl(model, nobs))


# Robustness on AIS disabling events
AIS_disab_models <- expand_grid(outcome_var = "asinh(n_ais_disabling)",
                                spec = "~ post | id + year^month + day_of_week",
                                hotspot = c("Global", "G. of Aden", "G. of Guinea"),
                                res = "0_5") |>
  mutate(model = pmap(.l = list(outcome_var = outcome_var,
                                spec = spec,
                                hotspot = hotspot,
                                res = res), run_estimation),
         coefficients = map(model, coefficients),
         n = map_dbl(model, nobs))

names(AIS_disab_models$coefficients) <- AIS_disab_models$hotspot

# =============================================================================
# EXPORT ALL MODELS
# # =============================================================================
save(models,
     file = here("data", "output", "gridcell_models.RData"))
save(spec_tables,
     file = here("data", "output", "gridcell_spec_models.RData"))
save(AIS_disab_models,
     file = here("data", "output", "gridcell_AIS_disabling.RData"))
