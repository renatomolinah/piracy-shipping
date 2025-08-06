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
  zoo,
  fixest,
  modelsummary,
  tidyverse
)

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("data",
                                        "processed",
                                        "attacks_and_activity_by_grid.rds")) |>
  filter(!attack_cluster == "None")

# Define some windows
bef_window <- 5
aft_window <- 10

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
         post = 1 * (event >= 0)) %>%
  ungroup() %>%
  select(grid_id, attack_cluster, attack_id,
         lat_bin, lon_bin, attack_date,
         date, year, month, asam_region, asam_subregion, post, event, days_since_attack, attacks, n_vessels, contains("dist"), contains("hours"), n_trips)



## ESTIMATION ##################################################################
# Fit the model
es_mod_global <- feols(c(time_hours,
                         time_hours/n_trips,
                         time_hours/n_vessels,
                         distance_km,
                         distance_km/n_trips,
                         distance_km/n_vessels) ~
                         i(event, "0") |
                         # Fixed effects
                         csw0(asam_subregion, year^month^asam_region, grid_id),
                       data = event_study_panel,
                       panel.id = ~attack_id + date,
                       # SE specification
                       vcov = vcov_conley(lat = "lat_bin",
                                          lon = "lon_bin",
                                          cutoff = 100))


es_mod_cluster <- feols(c(time_hours,
                          time_hours/n_trips,
                          time_hours/n_vessels,
                          distance_km,
                          distance_km/n_trips,
                          distance_km/n_vessels) ~
                          i(event, "0") |
                          # Fixed effects
                          asam_subregion + year^month^asam_region + grid_id,
                        data = event_study_panel,
                        panel.id = ~attack_id + date,
                        # SE specification
                        vcov = vcov_conley(lat = "lat_bin",
                                           lon = "lon_bin",
                                           cutoff = 100),
                        split = ~attack_cluster)

# Extract model coefficients and SEs
plot_data <- es_mod_global  %>%
  map_dfr(broom::tidy, .id = "sample", conf.int = TRUE, conf.level = 0.95) %>%
  filter(str_detect(term, "event")) %>%
  mutate(var = str_remove(sample, ".+lhs: "),
         var = case_when(var == "distance_km" ~ "Distance (km)",
                         var == "distance_km/n_trips" ~ "Distance per voyage (km/voyage)",
                         var == "distance_km/n_vessels" ~ "Distance per vessel (km/vessel)",
                         var == "time_hours" ~ "Occupancy (hr)",
                         var == "time_hours/n_trips" ~ "Occupancy per voyage (hr/voyage)",
                         var == "time_hours/n_vessels" ~ "Occupancy per vessel (hr/vessel)"),
         fixef = str_extract(sample, "fixef: .+;"),
         fixef = str_remove_all(fixef, "fixef: |;"),
         fixef = case_when(fixef == 1 ~ "None",
                           fixef == "asam_subregion" ~ "Group",
                           fixef == "asam_subregion + year^month^asam_region" ~ "Group + Time",
                           fixef == "asam_subregion + year^month^asam_region + grid_id" ~ "Group + Time + Grid id"),
         event = as.numeric(str_remove_all(term, "event::")))

plot_data <- plot_data %>%
  bind_rows(expand_grid(event = 0,
                        estimate = 0,
                        std.error = 0,
                        conf.low = 0,
                        conf.high = 0,
                        var = unique(plot_data$var),
                        fixef = unique(plot_data$fixef)))

# Build the event-study plot
main_plot <- ggplot(data = plot_data %>%
                      filter(fixef == "Group + Time + Grid id"),
                    aes(x = event, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point() +
  geom_point(x = 0, y = 0, size = 1, inherit.aes = F) +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high),
                 color = "black",
                 linewidth = 0.2) +
  geom_pointrange(aes(ymin = estimate - std.error,
                      ymax = estimate + std.error),
                  size = 0.25,
                  linewidth = 1) +
  facet_wrap(~var, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = c(-5:5)) +
  theme_minimal(base_size = 7) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  labs(x = "Time-to-encounter",
       y = "Estimate ± (std.error & 95%CI)")

main_plot


ggsave(plot = main_plot,
       filename = here("figures", "grid_level_event_study.pdf"),
       width = 12.1,
       height = 6,
       units = "cm")

# Build event-study plot with cumulative FEs for SM
supp_plot <- ggplot(data = plot_data,
                    aes(x = event, y = estimate, color = fixef),) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(x = 0, y = 0, size = 1, inherit.aes = F) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, group = term),
                 position = position_dodge(width = 0.8),
                 color = "black",
                 linewidth = 0.2) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  position = position_dodge(width = 0.8),
                  size = 0.25,
                  linewidth = 1) +
  facet_wrap(~var, scales = "free_y", ncol = 3) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = c(-5:5)) +
  theme_minimal(base_size = 7) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fil = "transparent",
                                         color = "black")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  labs(x = "Time-to-encounter",
       y = "Estimate ± (std.error & 95%CI)",
       color = "Fixed-effects specification")

supp_plot


ggsave(plot = supp_plot,
       filename = here("figures", "grid_level_event_study_FEs.pdf"),
       width = 18.4,
       height = 9,
       units = "cm")



# By cluster
supp_plot_cluster <- es_mod_cluster %>%
  map_dfr(broom::tidy, .id = "sample", conf.int = TRUE, conf.level = 0.95) %>%
  filter(str_detect(term, "event")) %>%
  mutate(var = str_remove(sample, ".+lhs: "),
         var = case_when(var == "distance_km" ~ "Distance (km)",
                         var == "distance_km/n_trips" ~ "Distance per voyage (km/voyage)",
                         var == "distance_km/n_vessels" ~ "Distance per vessel (km/vessel)",
                         var == "time_hours" ~ "Occupancy (hr)",
                         var == "time_hours/n_trips" ~ "Occupancy per voyage (hr/voyage)",
                         var == "time_hours/n_vessels" ~ "Occupancy per vessel (hr/vessel)"),
         cluster = str_extract(sample, "GoA|SEA|GoG"),
         event = as.numeric(str_remove_all(term, "event::"))) %>%
  ggplot(aes(x = event, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(x = 0, y = 0, size = 1, inherit.aes = F) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, group = term),
                 position = position_dodge(width = 0.8),
                 color = "black",
                 linewidth = 0.2) +
  geom_pointrange(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  position = position_dodge(width = 0.8),
                  size = 0.25,
                  linewidth = 1) +
  facet_wrap(cluster~var,
             scales = "free_y",
             ncol = 3) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = c(-5:5)) +
  theme_minimal(base_size = 7) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fil = "transparent",
                                         color = "black")) +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5)) +
  labs(x = "Time-to-encounter",
       y = "Estimate ± (std.error & 95%CI)",
       color = "Fixed-effects specification")

ggsave(plot = supp_plot_cluster,
       filename = here("figures", "grid_level_event_study_clusters.pdf"),
       width = 18.4,
       height = 18.4,
       units = "cm")
