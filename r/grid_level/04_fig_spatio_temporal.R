# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  magrittr,
  zoo,
  cowplot,
  tidyverse
)

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
piracy <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "piracy",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

coast <- rnaturalearth::ne_countries(scale = "large",
                                     country = c("Malaysia", "Indonesia", "Singapore"),
                                     returnclass = "sf")

# coast <- rnaturalearth::ne_countries(scale = "large",
#                                      country = c("China", "South Korea", "North Korea"),
#                                      returnclass = "sf")

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data",
                                        "attacks_and_activity_by_grid.rds"))

## PROCESSING ##################################################################



event_study_panel %>%
  filter(attack_date <= ymd("2021-07-01")) %>% 
  mutate(post = 1 * (event >= 0)) %>%
  group_by(attack_id, post) %>%
  summarize(m = sum(n_trips, na.rm = T)) %>%
  pivot_wider(names_from = post,
              values_from = m,
              names_prefix = "m_") %>%
  mutate(m = m_1 - m_0,
         mp = m / m_0) %>%
  arrange(m) %>% 
  View()


# On 26 March, duty crewman on routine rounds onboard a bulk carrier anchored near position 03-43S 114-25E, Taboneo Anchorage, noticed the forecastle store room door lock was broken. Further checks made on the forecastle indicated that the hawse pipe cover securing arrangements were cut through. The crewman informed the bridge and alarm was raised. Crew mustered and went to the forecastle and found ship's stores were stolen. Port Control informed
# Source: https://msi.nga.mil/queryResults?publications/asam?filter=none&minOccurDate=2017-03-01&maxOccurDate=2017-03-30&sort=date&output=html
focus_grid <- "-4_114" #"-1.5_117" #"124" 
focus_date <- ymd("2017-03-26") #ymd("2013-06-19") #ymd("2017-10-17")
focus_grid_coords <- grid_level_panel %>% 
  filter(grid_id == focus_grid) %>% 
  select(lat_bin, lon_bin) %>% 
  distinct()

focus_lat <- focus_grid_coords$lat_bin
focus_lon <- focus_grid_coords$lon_bin

# Get data #####################################################################
# Grid-level info --------------------------------------------------------------
g_tracks <- tbl(piracy, "gridded_data_0_5") %>% 
  filter(lat_bin == focus_lat,
         lon_bin == focus_lon) %>%
  mutate(post = date > sql("('2017-03-26')")) %>%
  group_by(date, lat_bin, lon_bin, post) %>% 
  summarize(distance_km = sum(distance_km, na.rm = T),
            hours = sum(hours, na.rm = T),
            n_trips = n_distinct(trip_id)) %>% 
  collect() %>% 
  ungroup() %>% 
  filter(between(date, focus_date - 30, focus_date + 30)) %>% 
  arrange(date) %>% 
  mutate(n_trips_w = rollapply(data = n_trips,
                               width = 5,
                               FUN = mean,
                               fill = NA,
                               align = "right"))
         
# # Track-level info -------------------------------------------------------------
tracks <- tbl(piracy, "ungridded_data") %>%
  filter(between(lat, focus_lat - 3, focus_lat + 3),
         between(lon, focus_lon - 3, focus_lon + 3),
         between(date, sql("date('2017-03-19')"), sql("date('2017-04-02')"))) %>%
  mutate(post = ifelse(date > sql("date('2017-03-26')"), "After", "Before")) %>%
  arrange(trip_id, date) %>%
  collect()

ts <- g_tracks %>% 
  rename(m = n_trips,
         mw = n_trips_w) %>% 
  mutate(mean_m = mean(m[!post]),
         sd_m = sd(m[!post])) %>% 
  ggplot(aes(x = date)) +
  geom_vline(xintercept = focus_date, linetype = "dotted") +
  geom_ribbon(aes(ymin = mean_m - sd_m,
                  ymax = mean_m + sd_m),
              alpha = 0.25) +
  geom_line(aes(y = mean_m),
            linetype = "dashed") +
  geom_point(aes(y = m)) +
  # geom_smooth() +
  geom_line(aes(y = mw), 
            linewidth = 1,
            color = "steelblue") +
  # scale_x_continuous(labels = c(focus_date -7, focus_date -3, focus_date, focus_date + 3, focus_date + 7),
                     # breaks = c(focus_date -7, focus_date -3, focus_date, focus_date + 3, focus_date + 7)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        text = element_text(size = 8)) +
  labs(x = "Date",
       y = "Number of trips")

colors <- c("#67001F", "#B1182B", "#D5604D", "#F3A481", "#FCDAC6", "#F6F6F6", "#D0E4EF", "#91C4DD", "#4392C2", "#2166AB")

spat <- ggplot(tracks %>% 
                 mutate(post = fct_relevel(post, "Before", "After")),
               aes(x = lon, y = lat)) +
  geom_density2d_filled(alpha = 0.75,
                        bins = 10,
                        contour_var = "ndensity") +
  geom_sf(data = coast, inherit.aes = F) +
  geom_point(aes(group = trip_id),
             pch = ".",
             color = "black",
             alpha = 0.1) +
  facet_wrap(~post) +
  geom_point(aes(x = focus_lon, y = focus_lat),
             color = "black", shape = "X", size = 4) +
  guides(fill = guide_legend(nrow = 1, title.position = "top")) +
  scale_fill_discrete(type = rev(colors)) +
  scale_x_continuous(limits = c(focus_lon - 3, focus_lon + 3), expand = expansion(0.01, 0)) +
  scale_y_continuous(limits = c(focus_lat - 3, focus_lat + 3), expand = expansion(0.01, 0)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#D0E4EF", color = "black"),
        panel.grid = element_line(color = "black",
                                  linewidth = 0.1,
                                  linetype = "dashed"),
        strip.background = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        text = element_text(size = 8)) +
  labs(fill = "Normalized density")

ref <- ggplot() +
  geom_sf(data = coast,
          fill = "black", color = "black") +
  geom_rect(
    aes(
      xmin = focus_lon - 3,
      xmax = focus_lon + 3,
      ymin = focus_lat - 3,
      ymax = focus_lat + 3
    ),
    fill = "transparent",
    color = "red"
  ) +
  theme_void() + 
  theme(panel.background = element_rect(fill = "white",
                                        colour = "black")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) 

p <- cowplot::plot_grid(spat, ts,
                        rel_heights = c(3, 1),
                        labels = "AUTO",
                        ncol = 1,
                        axis = "l",
                        align = "hv")
map <- ggdraw() +
  draw_plot(p) +
  cowplot::draw_plot(
    ref,
    x = 0.0615,
    y = 1,
    hjust = 0,
    vjust = 1,
    width = 0.25,
    height = 0.2168
  )

ggsave(plot = map,
       filename = here("figs", "spatio_temporal_figure.pdf"),
       height = 15,
       width = 18,
       units = "cm")
