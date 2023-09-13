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

sf_use_s2(F)
# Load data --------------------------------------------------------------------
data <- readRDS(file = here("processed_data",
                             "attacks_and_activity_by_grid.rds"))

grid <- data %>% 
  select(grid_id, lat_bin, lon_bin) %>% 
  distinct() %>% 
  st_as_sf(coords = c("lon_bin", "lat_bin"), crs = 4326)

fao_regions <- st_read(dsn = here("data", "fao_regions.gpkg"))

grid_fao <- grid %>% 
  st_join(fao_regions, join = st_nearest_feature) %>% 
  st_drop_geometry()

# Grid-level attack panel
grid_level_panel <- data %>% 
  mutate(days_since_attack = as.numeric(days_since_attack)) %>% 
  replace_na(replace = list(distance_km = 0,
                            hours = 0,
                            n_trips = 0,
                            n_vessels = 0,
                            n_ais_messages = 0)) %>% 
  mutate(hours_div_vessel = hours / n_vessels,
         dist_div_vessel = distance_km / n_vessels,
         year = year(date),
         month = month(date),
         ym = paste(year, month, sep = "-")) %>% 
  replace_na(replace = list(hours_div_vessel = 0,
                            dist_div_vessel = 0))

# Define some windows
lag_window <- 7
bef_window <- 7
aft_window <- 7

## PROCESSING ------------------------------------------------------------------
attacks <- grid_level_panel %>% 
  filter(days_since_attack == 0) %>% 
  group_by(grid_id) %>% 
  arrange(date) %>%
  mutate(data_days = date - min(date)) %>% 
  filter(data_days >= bef_window + lag_window) %>%  # Remove observations that don't have enough pre-attack observations to calculate the moving mean
  select(grid_id, attack_date = date) %>% 
  mutate(attack_id = paste(grid_id, attack_date))

event_study_panel <- grid_level_panel %>% 
  left_join(attacks,
            by = "grid_id",
            relationship = "many-to-many") %>% 
  left_join(grid_fao, by = "grid_id") %>% 
  filter(between(date, attack_date - (bef_window + lag_window), attack_date + aft_window)) %>% 
  mutate(zone_year = paste(zone, year, sep = "_")) %>% 
  group_by(grid_id) %>% 
  arrange(date) %>% 
  mutate(attacks = cumsum(days_since_attack == 0)) %>% 
  ungroup() %>% 
  arrange(grid_id, attack_date, date) %>% 
  group_by(grid_id, attack_date) %>% 
  mutate(hours_w = rollapply(data = hours,
                           width = lag_window,
                           FUN = mean,
                           fill = NA,
                           align = "right"),
         distance_w = rollapply(data = distance_km,
                                width = lag_window,
                                FUN = mean,
                                fill = NA,
                                align = "right"),
         event = as.numeric(date - attack_date),
         pre = 1 * (event < 0)) %>%
  ungroup() %>% 
  filter(between(date, attack_date - bef_window, attack_date + aft_window)) %>% 
  select(grid_id, attack_cluster, attack_id, lat_bin, lon_bin, attack_date, date, ym, zone_year, event, days_since_attack, attacks, contains("dist"), contains("hours"), n_trips)

es_mod <- feols(c(hours, distance_km, n_trips) ~ attacks + days_since_attack + i(event, ref = "-1") | 
                  # Fixed effects
                  grid_id + zone_year,
                # SE specifictions
                vcov = function(x)vcov_conley_hac(x,
                                                  id = ~attack_id,
                                                  time = ~date,
                                                  lat = ~lat_bin,
                                                  lon = ~lon_bin,
                                                  cutoff = 75,
                                                  lag = 14),
                panel.id = ~attack_id + date,
                fsplit= ~attack_cluster,
                split.drop = "None",
                lean = TRUE,
                data = event_study_panel)




plot_data <- es_mod  %>% 
  map_dfr(broom::tidy, .id = "sample", conf.int = TRUE, conf.level = 0.95) %>% 
  filter(str_detect(term, "event")) %>% 
  mutate(var = str_extract(sample, "distance_km|hours|n_trips"),
         var = case_when(var == "distance_km" ~ "Distance (km)",
                         var == "hours" ~ "Time (hours)",
                         var == "n_trips" ~ "Trips (#)"),
         sample = str_extract(sample, "sample: .+;"),
         sample = str_remove_all(sample, "sample|[:punct:]| "),
         event = as.numeric(str_remove(term, "event::")))

plot <- ggplot(data = plot_data,
       aes(x = event, y = estimate, shape = var)) + 
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  # geom_errorbar(aes(ymin = conf.low,
                    # ymax = conf.high)) +
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
  guides(shape = guide_legend(title.position = "top")) +
  labs(x = "Days since attack",
       y = "Estimate (with SE)",
       shape = "Dependent variable") +
  theme(strip.background = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size = 8))

ggsave(plot = plot,
       filename = here("figs", "grid_level_event_study.pdf"),
       width = 9,
       height = 11,
       units = "cm")  









