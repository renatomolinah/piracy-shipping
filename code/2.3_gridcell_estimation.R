################################################################################
#
# Estimates gridcell analysis in event study and post approaches
#
################################################################################

# Load packages
library(here)
library(conleyreg)
library(tidyverse)

processKBLoutput <- function(file_path) {
  
  # Read the content of the LaTeX file
  lines <- readLines(file_path, warn = FALSE)
  
  # Define the patterns for hotspots as they appear in the document, prepared for regex matching
  hotspots_patterns <- c(
    "\\\\hspace\\{1em\\}G\\. of Aden",
    "\\\\hspace\\{1em\\}Southeast Asia",
    "\\\\hspace\\{1em\\}G\\. of Guinea",
    "\\\\hspace\\{1em\\}Rest of the World"
  )
  
  # Process each line to remove the specified hotspot text
  modified_lines <- lines
  for (i in seq_along(modified_lines)) {
    for (pattern in hotspots_patterns) {
      # This ensures we're matching the exact string with regex, and replace it
      modified_lines[i] <- gsub(pattern, "\\\\hspace{1em}", modified_lines[i])
    }
  }
  
  # Write the modified lines back to the same file or a new file
  writeLines(modified_lines, file_path)
  
  cat("The LaTeX file has been successfully processed and saved.\n")
}



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

# Table of summary stats -------------------------------------------------------
P5 <- function(x) quantile(x, 0.05, na.rm = TRUE)
P95 <- function(x) quantile(x, 0.95, na.rm = TRUE)

by_cluster <- datasummary(attack_cluster * (Mean + SD + P5 + P95) ~ distance_km + time_hours + n_trips + n_vessels,
                          data = panel("0_5") %>%
                            mutate(attack_cluster = ifelse(attack_cluster == "None", "Rest of the world", attack_cluster),
                                   attack_cluster = fct_relevel(attack_cluster, "G. of Aden", "G. of Guinea", "S.E. Asia", "Rest of the world")),
                          output = "dataframe") %>%
  mutate(
    distance_km = as.numeric(distance_km),
    time_hours = as.numeric(time_hours),
    n_trips = as.numeric(n_trips),
    n_vessels = as.numeric(n_vessels)
  ) %>%
  mutate(across(c(distance_km, time_hours, n_trips, n_vessels), ~scales::comma(., accuracy = 0.1))) %>%
  select(-attack_cluster)


kbl(x = by_cluster,
    booktabs = TRUE,
    label = "grid_summary",
    caption = "Summary Statistics for Daily Ship Transit by Grid Cell.",
    col.names = c("", "Distance (km)", "Occupancy (hr)", "Voyages (\\#)", "Unique vessels (\\#)"),
    align = c("l", "r", "r", "r", "r"),
    linesep = "",
    format = "latex",
    escape = FALSE) %>%
  kable_styling() %>%
  pack_rows("Gulf of Aden", 1, 4) %>%
  pack_rows("Gulf of Guinea", 5, 8) %>%
  pack_rows("Southeast Asia", 9, 12) %>%
  pack_rows("Rest of the World", 13, 16) %>%
  footnote(general = "The unit of observation is a grid cell-day on a $0.5^\\\\circ \\\\times 0.5^\\\\circ$ grid. The sample includes 618 grid cells with at least one pirate encounter during the 2012--2023 period. Each column reports a different measure of daily shipping activity within a cell. P5 and P95 denote the 5th and 95th percentiles, respectively.",
           general_title = "",
           escape = FALSE,
           threeparttable = TRUE) %>%
  cat(file = here("results", "figures_and_tables", "grid_summary_stats.tex"))

processKBLoutput(here("results", "figures_and_tables", "grid_summary_stats.tex"))


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
                                hotspot = c("Global", "G. of Aden", "G. of Guinea"), # SE Asia excluded because outcome is constant at 0.
                                res = "0_5") |>
  mutate(model = pmap(.l = list(outcome_var = outcome_var,
                                spec = spec,
                                hotspot = hotspot,
                                res = res), run_estimation),
         coefficients = map(model, coefficients),
         n = map_dbl(model, nobs))

names(AIS_disab_models$coefficients) <- AIS_disab_models$hotspot

# # =============================================================================
# EXPORT ALL MODELS
# # =============================================================================
save(models,
     file = here("data", "output", "gridcell_models.RData"))
save(spec_tables,
     file = here("data", "output", "gridcell_spec_models.RData"))
save(AIS_disab_models,
     file = here("data", "output", "gridcell_AIS_disabling.RData"))
