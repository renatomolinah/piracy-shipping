# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  magrittr,
  sf,
  rnaturalearth,
  zoo,
  cowplot,
  tidyverse
)

theme_map <- function(){
  theme_minimal(base_size = 7) %+replace%
    theme(panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          # axis.text.x = element_blank(),
          # axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          strip.background = element_rect(fill=NA,color=NA))
}

colors <- c("#67001F", "#B1182B", "#D5604D", "#F3A481", "#FCDAC6", "#F6F6F6", "#D0E4EF", "#91C4DD", "#4392C2", "#2166AB")

sf_use_s2(F)
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

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("processed_data",
                                        "attacks_and_activity_by_grid.rds"))

## PROCESSING ##################################################################
# event_study_panel %>%
#   filter(attack_date <= ymd("2022-07-01")) %>% 
#   mutate(post = 1 * (event >= 0)) %>%
#   group_by(attack_cluster, attack_id, post) %>%
#   summarize(m = sum(n_trips, na.rm = T)) %>%
#   pivot_wider(names_from = post,
#               values_from = m,
#               names_prefix = "m_") %>%
#   mutate(m = m_1 - m_0,
#          mp = m / m_0) %>%
#   arrange(m) %>% 
#   group_by(attack_cluster) %>%
#   slice_min(m)

# # A tibble: 4 × 6
# # Groups:   attack_cluster [4]
# attack_cluster attack_id              m_0   m_1     m     mp
# <chr>          <chr>                <int> <int> <int>  <dbl>
# 1 GoA            22.5_69.5 2015-10-29   208   175   -33 -0.159
# 2 GoG            6_3 2017-09-20         144   118   -26 -0.181
# 3 None           38.5_119 2019-01-30   1075   844  -231 -0.215
# 4 SEA            -1.5_117 2013-06-19    270    98  -172 -0.637

# On 26 March, duty crewman on routine rounds onboard a bulk carrier anchored near position 03-43S 114-25E, Taboneo Anchorage, noticed the forecastle store room door lock was broken. Further checks made on the forecastle indicated that the hawse pipe cover securing arrangements were cut through. The crewman informed the bridge and alarm was raised. Crew mustered and went to the forecastle and found ship's stores were stolen. Port Control informed
# Source: https://msi.nga.mil/queryResults?publications/asam?filter=none&minOccurDate=2017-03-01&maxOccurDate=2017-03-30&sort=date&output=html
get_pars <- function(attack_id){
  split <- str_split(string = attack_id,
                     pattern = " ",
                     simplify = T)
  focus_grid <- split[1] #"22.5_69.5"# "-4_114" #"-1.5_117" #"124" 
  focus_date <- split[2]# ymd("2015-10-29")# ymd("2017-03-26") #ymd("2013-06-19") #ymd("2017-10-17")
  focus_grid_coords <- grid_level_panel %>% 
    filter(grid_id == focus_grid) %>% 
    select(lat_bin, lon_bin) %>% 
    distinct()
  
  focus_lat <- focus_grid_coords$lat_bin
  focus_lon <- focus_grid_coords$lon_bin
  
  pars <- list(focus_date = focus_date,
               focus_lat = focus_lat,
               focus_lon = focus_lon)
  
  return(pars)
}

# Grid-level info --------------------------------------------------------------
get_grid_activity <- function(pars){
  focus_lat <- pars$focus_lat
  focus_lon <- pars$focus_lon
  focus_date <- lubridate::ymd(pars$focus_date)
  
  g_tracks <- tbl(piracy, "gridded_data_0_5_v_20240307") %>% 
    filter(lat_bin == focus_lat,
           lon_bin == focus_lon) %>%
    mutate(post = date > sql(paste0("date('", focus_date, "')"))) %>%
    group_by(date, lat_bin, lon_bin, post) %>% 
    summarize(distance_km = sum(distance_km, na.rm = T),
              hours = sum(hours, na.rm = T),
              n_trips = n_distinct(trip_id),
              .groups = "drop") %>% 
    collect() %>% 
    filter(between(date, focus_date - 30, focus_date + 30)) %>% 
    arrange(date) %>% 
    mutate(n_trips_w = rollapply(data = n_trips,
                                 width = 5,
                                 FUN = mean,
                                 fill = NA,
                                 align = "right")) %>% 
    mutate(n_trips_m = mean(n_trips[!post]),
           n_trips_sd = sd(n_trips[!post]),
           lat_bin = lat_bin + 0.25,
           lon_bin = lon_bin + 0.25)
  
  return(g_tracks)
}
         
# Track-level info -------------------------------------------------------------
get_tracks <- function(pars) {
  focus_date <- pars$focus_date
  focus_lat <- pars$focus_lat
  focus_lon <- pars$focus_lon
  
  
  tracks <- tbl(piracy, "ungridded_data_v_20240228") %>%
    mutate(date = sql("EXTRACT(DATE from timestamp)")) %>% 
    filter(between(lat, focus_lat - 3, focus_lat + 3),
           between(lon, focus_lon - 3, focus_lon + 3),
           between(date, sql(paste0("date('", focus_date, "') - 7")), sql(paste0("date('", focus_date, "') + 7")))) %>%
    mutate(post = ifelse(date > sql(paste0("date('", focus_date, "')")), "After encounter", "Before encounter")) %>%
    arrange(trip_id, date) %>%
    collect()
  
  return(tracks)
}


# Build time-series plot
make_ts_plot <- function(grid_activity, pars) {
  focus_date <- lubridate::ymd(pars$focus_date)
  
  grid_activity %>% 
    ggplot(aes(x = date)) +
    geom_vline(xintercept = focus_date, linetype = "dotted") +
    geom_ribbon(aes(ymin = n_trips_m - n_trips_sd,
                    ymax = n_trips_m + n_trips_sd),
                alpha = 0.25) +
    geom_line(aes(y = n_trips_m),
              linetype = "dashed") +
    geom_point(aes(y = n_trips)) +
    geom_line(aes(y = n_trips_w), 
              linewidth = 1,
              color = "cadetblue") +
    theme_minimal(base_size = 7) +
    theme(axis.title.x = element_blank()) +
    labs(y = "# Voyages")
}

# Build spatial plot
make_spat_plot <- function(tracks, pars) {
  focus_lat <- pars$focus_lat
  focus_lon <- pars$focus_lon
  
  
  all_countries <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
    st_crop(y = c(xmin = focus_lon - 3,
                  ymin = focus_lat - 3,
                  xmax = focus_lon + 3,
                  ymax = focus_lat + 3)) %>% 
    pull(name) %>% 
    unique()
  
  coast <- rnaturalearth::ne_countries(scale = "large",
                                       country = all_countries,
                                       returnclass = "sf")
  
  ggplot(tracks %>% 
           mutate(post = fct_relevel(post, "Before encounter", "After encounter")),
         aes(x = lon, y = lat)) +
    geom_density2d_filled(alpha = 0.75,
                          bins = 10,
                          contour_var = "ndensity") +
    geom_sf(data = coast, inherit.aes = F) +
    geom_point(pch = ".",
               color = "black",
               alpha = 0.2) +
    facet_wrap(~post) +
    geom_point(aes(x = focus_lon + 0.25, y = focus_lat + 0.25),
               color = "darkorange3", shape = "X", size = 3) +
    guides(fill = guide_colorsteps(ticks = T)) +
    # scale_fill_discrete(type = rev(colors)) +
    # scale_fill_discrete(type = wesanderson::wes_palette("Zissou1", n = 10, type = "continuous")) +
    # scale_fill_viridis_d(option = "inferno") +
    scale_fill_discrete_sequential("Teal") +
    scale_x_continuous(limits = c(focus_lon - 3, focus_lon + 3), expand = expansion(0.01, 0)) +
    scale_y_continuous(limits = c(focus_lat - 3, focus_lat + 3), expand = expansion(0.01, 0)) +
    theme_map() +
    theme(panel.spacing.x = unit(2, "lines")) +
    labs(fill = "Density")
}


case_plot <- function(attack_id) {
  pars <- get_pars(attack_id)
  tracks <- get_tracks(pars)
  grid_activity <- get_grid_activity(pars)
  ts <- make_ts_plot(grid_activity, pars)
  spat <- make_spat_plot(tracks, pars)  
  
  cowplot::plot_grid(spat, ts,
                     rel_heights = c(3, 1),
                     labels = "AUTO",
                     label_x = 0.9,
                     label_y = c(1, 1.2),
                     ncol = 1,
                     axis = "l",
                     align = "hv")
}

# Get data #####################################################################
pars <- get_pars("-1.5_117 2013-06-19")
tracks <- get_tracks(pars)
grid_activity <- get_grid_activity(pars)
ts <- make_ts_plot(grid_activity)
spat <- make_spat_plot(tracks, pars)

goa_plot <- case_plot(attack_id = "22.5_69.5 2015-10-29")
gog_plot <- case_plot(attack_id = "6_3 2017-09-20")
sea_plot <- case_plot(attack_id = "-1.5_117 2013-06-19")
oth_plot <- case_plot(attack_id = "38.5_119 2019-01-30")

# 1 GoA            22.5_69.5 2015-10-29   208   175   -33 -0.159
# 2 GoG            6_3 2017-09-20         144   118   -26 -0.181
# 3 None           38.5_119 2019-01-30   1075   844  -231 -0.215
# 4 SEA            -1.5_117 2013-06-19    270    98  -172 -0.637

ggsave(plot = sea_plot,
       filename = here("figures", "spatio_temporal_figure.pdf"),
       width = 12.1,
       height = 8,
       units = "cm")


tracks2 <- tracks %>%
  mutate(lon_bin = (floor(lon / 0.25) * 0.25) + 0.125,
         lat_bin = (floor(lat / 0.25) * 0.25) + 0.125) %>%
  group_by(post, lat_bin, lon_bin) %>%
  summarize(h = n_distinct(mmsi), .groups = "drop") %>% 
  complete(lat_bin, lon_bin, post)


ggplot(tracks2, aes(x = lon_bin, y = lat_bin, fill = h)) +
  geom_raster() +
  geom_sf(data = coast, inherit.aes = F) +
  geom_point(data = tracks %>% 
               mutate(post = fct_relevel(post, "Before encounter", "After encounter")),
             mapping = aes(x = lon, y = lat), pch = ".", inherit.aes = F) +
  facet_wrap(~post) +
  geom_point(aes(x = focus_lon + 0.25, y = focus_lat + 0.25),
             color = "black", shape = "X", size = 3) +
  guides(fill = guide_colorsteps(ticks = T)) +
  # scale_fill_discrete(type = rev(colors)) +
  # scale_fill_discrete(type = wesanderson::wes_palette("Zissou1", n = 10, type = "continuous")) +
  scale_fill_viridis_c(option = "mako", trans = "log10", na.value = 0) +
  scale_x_continuous(limits = c(focus_lon - 3, focus_lon + 3), expand = expansion(0.01, 0)) +
  scale_y_continuous(limits = c(focus_lat - 3, focus_lat + 3), expand = expansion(0.01, 0)) +
  theme_map() +
  theme(panel.spacing.x = unit(2, "lines")) +
  labs(fill = "Density")
