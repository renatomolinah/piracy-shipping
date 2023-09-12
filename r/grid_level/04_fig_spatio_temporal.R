# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  magrittr,
  tidyverse
)

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
piracy <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "piracy",
  # # billing = "juancv-stanford",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

## PROCESSING ##################################################################



coast <- rnaturalearth::ne_countries(scale = "large",
                                     country = c("Malaysia", "Indonesia", "Singapore"),
                                     returnclass = "sf")


event_study_panel %>%
  mutate(post = 1 * (event >= 0)) %>% 
  group_by(attack_id, post) %>% 
  summarize(distance_km = sum(n_trips)) %>% 
  pivot_wider(names_from = post,
              values_from = distance_km,
              names_prefix = "d_") %>% 
  mutate(d = d_1 - d_0) %>% 
  arrange(d)


# 162 2013-06-19    343   130  -213

focus_grid <- "162" #"124" 
focus_date <- ymd("2013-06-19") #ymd("2017-10-17")
focus_grid_coords <- grid_level_panel %>% 
  filter(grid_id == focus_grid) %>% 
  select(lat_bin, lon_bin) %>% 
  distinct()

focus_lat <- focus_grid_coords$lat_bin
focus_lon <- focus_grid_coords$lon_bin


tracks <- tbl(piracy, "gridded_data_ml") %>% 
  filter(between(lat_bin, focus_lat - 3, focus_lat + 3),
         between(lon_bin, focus_lon - 3, focus_lon + 3),
         between(date, sql("date('2013-06-12')"), sql("date('2013-06-26')"))) %>%
  mutate(post = date > sql("date('2013-06-19')")) %>% 
  group_by(date, lat_bin, lon_bin, post) %>% 
  summarize(distance_km = sum(distance_km, na.rm = T),
            hours = sum(hours, na.rm = T),
            n_trips = n_distinct(trip_id)) %>% 
  collect() %>% 
  ungroup()

ts <- tracks %>% 
  filter(lat_bin == focus_lat,
         lon_bin == focus_lon) %>% 
  #grid_level_panel %>% 
  # filter(grid_id == focus_grid,
         # between(date, focus_date - 7, focus_date + 7)) %>% 
  group_by(post, date) %>% 
  summarize(distance_km = sum(n_trips)) %>% 
  group_by(post) %>% 
  mutate(distance_km = distance_km,
         mean_dist = mean(distance_km),
         sd_dist = sd(distance_km)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = distance_km)) +
  geom_vline(xintercept = focus_date) +
  geom_ribbon(aes(ymin = mean_dist - sd_dist,
                  ymax = mean_dist + sd_dist,
                  group = post),
              alpha = 0.25) +
  geom_line(aes(y = mean_dist, group = post),
            linetype = "dashed") +
  geom_line(linewidth = 1,
            color = "steelblue") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size = 8)) +
  labs(x = "Date",
       y = "Number of trips")

spat <- tracks %>% 
  mutate(post = 1 * (date >= focus_date)) %>%
  group_by(lat_bin, lon_bin, post) %>%
  summarize(distance_km = mean(n_trips)) %>%
  pivot_wider(names_from = post,
              values_from = distance_km,
              names_prefix = "d_") %>% 
  mutate(d = (d_1 - d_0) / d_0) %>% 
  ggplot(aes(x = lon_bin, y = lat_bin, fill = d)) + 
  geom_raster() +
  geom_sf(data = coast, inherit.aes = F) +
  geom_point(aes(x = focus_lon, y = focus_lat),
             color = "black",
             shape = "x",
             size = 4) +
  scale_fill_gradient2(mid = "white", labels = scales::percent) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightblue1", color = "black"),
        panel.grid = element_line(color = "black",
                                  linewidth = 0.1,
                                  linetype = "dashed"),
        legend.position = "top",
        axis.title = element_blank(),
        text = element_text(size = 8)) +
  guides(fill = guide_colorbar(frame.colour = "black",
                               ticks.colour = "black",
                               title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 13,
                               barheight = 0.5)) +
  scale_x_continuous(limits = c(focus_lon - 3, focus_lon + 3), expand = expansion(0.01, 0)) +
  scale_y_continuous(limits = c(focus_lat -3, focus_lat + 3), expand = expansion(0.01, 0)) +
  labs(fill = "% Change in daily trips")

p <- cowplot::plot_grid(spat, ts,
                        rel_heights = c(3.5, 1),
                        labels = "AUTO",
                        ncol = 1,
                        axis = "l",
                        align = "hv")

ggsave(plot = p,
       filename = here("figs", "spatio_temporal_figure.pdf"),
       height = 15,
       width = 10,
       units = "cm")


