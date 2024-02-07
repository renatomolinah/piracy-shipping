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

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  fixest,
  kableExtra,
  modelsummary,
  panelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data",
                                        "attacks_and_activity_by_grid.rds"))

## PROCESSING ##################################################################

# Modify the panel -------------------------------------------------------------
reg_data <- grid_level_panel %>% 
  # Rename the variable of interest for consistency with Renato's regressions
  rename(TNE = number_previous_attacks_grid_3_months) %>% 
  # Replace missing values (no transit detected in AIS) with zeroes.            << -------------- NOTE THIS!
  replace_na(replace = list(distance_km = 0,
                            time_hours = 0,
                            n_trips = 0,
                            n_vessels = 0,
                            n_ais_messages = 0)) %>% 
  mutate(year = year(date),
         month = month(date),
         ym = paste(year, month, sep = "-"))
  
# Table of summary stats -------------------------------------------------------
by_cluster <- datasummary(attack_cluster * (Mean + SD + Median + Max) ~ distance_km + time_hours + n_trips,
                          data = reg_data %>% 
                            mutate(attack_cluster = ifelse(attack_cluster == "None", "Rest of the world", attack_cluster),
                                   attack_cluster = fct_relevel(attack_cluster, "GoA", "GoG", "SEA", "Rest of the world")),
                          output = "dataframe")

kbl(x = by_cluster,
    booktabs = T,
    label = "grid_summary",
    caption = "Summary statistics for daily ship transit by grid cell",
    col.names = c("", "", "Distance (km)", "Occupancy (hours)", "Trips (#)"),
    linesep = "",
    format = "latex") %>% 
  cat(file = here("tables", "grid_summary_stats.tex"))

## ESTIMATE ####################################################################
# Just distance and slowly adding FEs
dist_mod <- feols(data = reg_data,
                  fml = distance_km ~ TNE | csw(0, grid_id, year ^ month ^ asam_subregion),
                  # SE specifictions
                  vcov = vcov_conley(lat = "lat_bin",
                                     lon = "lon_bin",
                                     cutoff = 100),
                  panel.id = ~grid_id + date)

etable(dist_mod)

# Full estimation --------------------------------------------------------------
mod <- feols(data = reg_data,
             fml =
               # Outcome varibles
               c(distance_km,
                 time_hours,
                 n_trips) ~
               # Regressors
               TNE | 
               # Fixed effects
               grid_id + year ^ month ^ asam_subregion,
             # SE specifictions
             vcov = vcov_conley(lat = "lat_bin",
                                lon = "lon_bin",
                                cutoff = 100),
             panel.id = ~grid_id + date,
             fsplit = ~attack_cluster,
             split.drop = "None",
             lean = TRUE)

# Quick local model inspection -------------------------------------------------
etable(mod)

# Summary of the models --------------------------------------------------------
fixest::models(mod)

## BUILD TABLES ################################################################
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Observations", 0,
              "vcov.type", "SE", 0,
              "FE: grid_id", "FE: Grid ID", 0,
              "FE: year^month^asam_subregion", "FE: ASAM subregion-year-month", 0
)

panelsummary(mod[c(1, 4, 7, 10)],
             mod[c(2, 5, 8, 11)],
             mod[c(3, 6, 9, 12)],
             caption = "\\label{grid_reg}Linear regression estimates for the average piracy effect on ship transit.",
             colnames = c(" ",
                          "Global",
                          "G. of Aden",
                          "G. of Guinea",
                          "South East Asia"),
             panel_labels = c("Panel (A): Total Distance (km)",
                              "Panel (B): Occupancy (hours)",
                              "Panel (C): Number of trips (count)"),
             stars = T,
             coef_map = c("TNE" = "TNE (3 mo)"),
             gof_map = gm,
             collapse_fe = T,
             pretty_num = T,
             format = "latex") %>% 
  add_footnote(threeparttable = T,
               c("Note: Standard errors in parentheses are Conley HAC (100 km cutoff). The unit of of observation is a grid cell. Every column is a different regression analysis for different samples. The first column refers to the global sample, while the rest only takes into account grid cells within a hotspot.",
                 "*p<0.05, **p<0.01, ***p<0.001")) %>% 
  cat(file = here("tables", "gridcell-dist-time.tex"))
