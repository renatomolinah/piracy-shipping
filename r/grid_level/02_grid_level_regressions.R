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
  modelsummary,
  tidyverse
)

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
         hours_div_trip = hours / n_trips)
  
## ESTIMATE ####################################################################
# Cuadratic estimation ---------------------------------------------------------
quad_mod <- feols(data = reg_data,
                  fml =
                    # Outcome varibles
                    c(dist_div_vessel,
                      dist_div_trip,
                      hours_div_vessel,
                      hours_div_trip) ~
                    # Regressors
                    TNE + TNE ^ 2 | 
                    # Fixed effects
                    grid_id + date,
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

tidy_quad_mod <- quad_mod %>%
  map_dfr(broom::tidy, .id = "model") %>% 
  mutate(sample = str_extract(model, "Full sample|GoG|GoA|SEA"),
         outcome = str_extract(model, "dist_div_vessel|dist_div_trip|hours_div_vessel|hours_div_trip")) %>% 
  select(-model)

ggplot(data = tidy_quad_mod %>% filter(term == "TNE"),
       aes(x = sample, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = estimate-std.error, ymax = estimate+std.error)) +
  facet_wrap(~outcome, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  ggtitle("Coefficient on TNE")

modelsummary(quad_mod,
             stars = T,
             mc.cores = 5,
             gof_omit = c("Adj|With"))

# Bined estimation -------------------------------------------------------------
bin_mod <- feols(data = reg_data,
                 fml =
                   # Outcome varibles
                   c(dist_div_vessel,
                     dist_div_trip,
                     hours_div_vessel,
                     hours_div_trip) ~
                   # Regressors
                   bin | 
                   # Fixed effects
                   grid_id + date,
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

bin_mod %>%
  map_dfr(broom::tidy, .id = "model") %>% 
  mutate(sample = str_extract(model, "Full sample|GoG|GoA|SEA"),
         outcome = str_extract(model, "dist_div_vessel|dist_div_trip|hours_div_vessel|hours_div_trip")) %>% 
  select(-model) %>% 
  mutate(term = as.numeric(str_extract(term, "[:digit:]+")),
         bin = paste0("(", term, ",", term + 4,")"),
         bin = fct_reorder(bin, term)) %>% 
  ggplot(aes(x = bin, y = estimate, color = sample)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(ymin = estimate-std.error, ymax = estimate+std.error)) +
  facet_wrap(~outcome, scales = "free_y", ncol = 2) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  labs(x = "Estimate", y = "Binned # of attacks") +
  scale_color_brewer(palette = "Set1")
