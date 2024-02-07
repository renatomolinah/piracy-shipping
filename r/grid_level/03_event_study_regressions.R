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
  sf,
  fixest,
  modelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data",
                             "attacks_and_activity_by_grid.rds"))

# Define some windows
bef_window <- 7
aft_window <- 7

# Questions for 2017-03-23 on 7.5_49.5:
#   - Why is "number of previous atcks grid 1 month" jumping from 0 to 2?
#   - Why are there no records of AIS activity in the area

## PROCESSING ------------------------------------------------------------------
# Identify all dates with attacks
attacks <- grid_level_panel %>% 
  filter(days_since_attack == 0) %>% 
  group_by(grid_id) %>% 
  arrange(date) %>%
  mutate(data_days = date - min(date)) %>% # Build a temporary variable that tells me how many days have passed between this observation and the first one, for each cell
  filter(data_days >= bef_window) %>%  # Remove attacks for which we don't have enough enough observations leading to it
  select(grid_id, attack_date = date) %>% 
  mutate(attack_id = paste(grid_id, attack_date)) %>% 
  ungroup()

# The attack described above is being dropped out because there were only two attacks fo thtis cell, but they are 7 days appart.
# Need to think of a better way to build this sample.

# Now build the panel
event_study_panel <- attacks %>% 
  # First, we perform a many-to-many match
  left_join(grid_level_panel,
            by = "grid_id",
            relationship = "many-to-many") %>% 
  # Then, for each attack date, we keep data that are within the before and after window defined above
  filter(between(date,
                 left = (attack_date - bef_window),
                 right = (attack_date + aft_window))) %>%
  # Extract year and month from the date
  mutate(year = year(date),
         month = month(date)) %>% 
  group_by(grid_id) %>% 
  arrange(date) %>% 
  mutate(attacks = cumsum(days_since_attack == 0)) %>%  # For each grid and date, calculate the number of historical attacks up to that point in time
  ungroup() %>% 
  arrange(grid_id, attack_date, date) %>% 
  group_by(grid_id, attack_date) %>% 
  # Build the event-time variable, with date of attack = 0
  mutate(event = as.numeric(date - attack_date),
         pre = 1 * (event < 0)) %>%
  ungroup() %>% 
  mutate(attack_bin = bin(attacks, "bin::10")) %>% 
  select(grid_id, attack_cluster, attack_id,
         lat_bin, lon_bin, attack_date,
         date, year, month, asam_subregion, event, days_since_attack, attack_bin, attacks, contains("dist"), contains("hours"), n_trips)


  
## ESTIMATION ##################################################################
# Fit the model
es_mod <- feols(c(distance_km, time_hours, n_trips) ~ i(event, ref = "0") | 
                  # Fixed effects
                  grid_id + year ^ month ^ asam_subregion,
                # SE specifictions
                vcov = vcov_conley(lat = "lat_bin",
                                   lon = "lon_bin",
                                   cutoff = 100),
                panel.id = ~attack_id + date,
                fsplit= ~attack_cluster,
                split.drop = "None",
                lean = TRUE,
                data = event_study_panel)

# Extract model coefficients and SEs
plot_data <- es_mod  %>% 
  map_dfr(broom::tidy, .id = "sample", conf.int = TRUE, conf.level = 0.95) %>% 
  filter(str_detect(term, "event")) %>% 
  mutate(var = str_extract(sample, "distance_km|hours|n_trips"),
         var = case_when(var == "distance_km" ~ "Distance traveled (km)",
                         var == "hours" ~ "Occupancy (hours)",
                         var == "n_trips" ~ "Trips (#)"),
         sample = str_extract(sample, "sample: .+;"),
         sample = str_remove_all(sample, "sample|[:punct:]| "),
         event = as.numeric(str_remove_all(term, "event::|:attack_bin")))

# Build the event-study plot
plot <- ggplot(data = plot_data,
       aes(x = event, y = estimate, shape = var)) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = estimate - std.error,
                    ymax = estimate + std.error),
                color = "black",
                linewidth = 0.5,
                width = 0) +
  geom_point(color = "black",
             fill = "#08519B",
             size = 1) +
  scale_x_continuous(breaks = c(-7, -4, 0, 4, 7), labels = c(-7, -4, 0, 4, 7)) +
  scale_shape_manual(values = c(21, 22, 24)) +
  facet_wrap(sample ~ var, scales = "free_y",
             ncol = 3,
             labeller = function(x){x[1]}) +
  theme_bw() +
  guides(shape = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(x = "Days since attack",
       y = "Estimate (with SE)",
       shape = "Outcome of interest") +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size = 8))

ggsave(plot = plot,
       filename = here("figs", "grid_level_event_study.pdf"),
       width = 9,
       height = 11,
       units = "cm")









