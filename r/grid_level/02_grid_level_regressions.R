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

# Define functions -------------------------------------------------------------
# Conley HAC method
# From: https://github.com/lrberge/fixest/issues/350#issuecomment-1671226930
vcov_conley_hac <- function(x, id, time, lat, lon, cutoff, lag) {
  # Spatial portion
  vcov_conley <-
    fixest::vcov_conley(
      x = x,
      lat = lat,
      lon = lon,
      cutoff = cutoff,
      distance = "spherical")
  
  # Panel portion
  vcov_hac <-
    fixest::vcov_NW(
      x = x,
      unit = id,
      time = time,
      lag = lag)
  # Heteroskedasticity
  vcov_robust <-
    fixest::vcov_cluster(
      x = x,
      cluster = id)
  
  
  vcov_conley_hac <- vcov_conley +
    vcov_hac -
    vcov_robust
  
  if(any(diag(vcov_conley_hac) < 0)){
    # We 'fix' it
    all_attr <- attributes(vcov_conley_hac)
    vcov_conley_hac <- fixest:::mat_posdef_fix(vcov_conley_hac)
    attributes(vcov_conley_hac) <- all_attr
    message("Variance contained negative values in the diagonal and was 'fixed' (a la Cameron, Gelbach & Miller 2011).")
  }
  
  return(vcov_conley_hac)
}

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data",
                                        "attacks_and_activity_by_grid.rds"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
reg_data <- grid_level_panel %>% 
  rename(TNE = attacks_window_last_12_month) %>% # Rename the variable of interest for consistency
  # Relace cases where we know tehre should have been a trip, but there wasn't on a given date
  replace_na(replace = list(distance_km = 0,
                            hours = 0,
                            n_trips = 0,
                            n_vessels = 0,
                            n_ais_messages = 0)) %>% 
  mutate(bin = as.character(bin(TNE, "bin::5")),
         bin = fct_reorder(bin, TNE),
         dist_div_vessel = distance_km / n_vessels,
         dist_div_trip = distance_km / n_trips,
         hours_div_vessel = hours / n_vessels,
         hours_div_trip = hours / n_trips,
         year = year(date),
         month = month(date),
         ym = paste(year, month, sep = "-"))
  
# Table of summary stats -------------------------------------------------------
by_cluster <- datasummary(attack_cluster * (Mean + SD + Median + Max) ~ distance_km + hours + n_trips,
                          data = reg_data %>% 
                            mutate(attack_cluster = ifelse(attack_cluster == "None", "Rest of the world", attack_cluster),
                                   attack_cluster = fct_relevel(attack_cluster, "GoA", "GoG", "SEA", "Rest of the world")),
                          output = "dataframe")

kbl(x = by_cluster,
    booktabs = T,
    label = "grid_summary",
    caption = "Summary statistics for daily ship transit by grid cell",
    col.names = c("", "", "Distance (km)", "Occupancy (hours)", "Trips (#)"),
    format = "latex") %>% 
  cat(file = here("tables", "grid_summary_stats.tex"))

## ESTIMATE ####################################################################
# Cuadratic estimation ---------------------------------------------------------
quad_mod <- feols(data = reg_data,
                  fml =
                    # Outcome varibles
                    c(distance_km,
                      hours,
                      n_trips) ~
                    # Regressors
                    TNE + TNE ^ 2 | 
                    # Fixed effects
                    grid_id + ym,
                  # SE specifictions
                  vcov = function(x)vcov_conley_hac(x,
                    id = ~grid_id,
                    time = ~date,
                    lat = ~lat_bin,
                    lon = ~lon_bin,
                    cutoff = 75,
                    lag = 14),
                  # vcov = "cluster",
                  panel.id = ~grid_id + date,
                  fsplit = ~attack_cluster,
                  split.drop = "None",
                  lean = TRUE)

panelsummary(quad_mod[c(1, 4, 7, 10)],
             quad_mod[c(2, 5, 8, 11)],
             quad_mod[c(3, 6, 9, 12)],
             caption = "\\label{grid_reg}Linear regression estimates for the average piracy effect on ship transit.",
             colnames = c(" ",
                          "Global",
                          "G. of Aden",
                          "G. of Guinea",
                          "South East Asia"),
             panel_labels = c("Panel A: Total Distance (km)",
                              "Panel B: Occupancy (hours)",
                              "Panel C: Number of trips"),
             stars = T,
             coef_map = c("TNE" = "One year ago",
                          "I(TNE^2)" = "(One year ago)2"),
             gof_omit = "R|Std.",
             collapse_fe = T,
             pretty_num = T,
             format = "latex") %>% 
  add_footnote(threeparttable = T,
               c("Note: Standard errors in parentheses are Conley HAC (75 km cutoff, 14 day lag). The unit of of observation is a grid cell. Every column is a different regression analysis for different samples. The first column refers to the global sample, while the rest only takes into account grid cells within a hotspot.",
                 "*p<0.05, **p<0.01, ***p<0.001")) %>% 
  cat(file = here("tables", "gridcell-dist-time.tex"))

# Bined estimation -------------------------------------------------------------
bin_mod <- feols(data = reg_data,
                 fml =
                   # Outcome varibles
                   c(distance_km,
                     # dist_div_vessel,
                     # dist_div_trip,
                     hours) ~
                     # hours_div_vessel,
                   # Regressors
                   bin | 
                   # Fixed effects
                   grid_id + ym,
                 # SE specifictions
                 vcov = vcov_conley(
                   lat = ~lat_bin,
                   lon = ~lon_bin,
                   cutoff = 75,
                   distance = "spherical"),
                 panel.id = ~grid_id + date,
                 fsplit = ~attack_cluster,
                 split.drop = "None",
                 lean = TRUE)



