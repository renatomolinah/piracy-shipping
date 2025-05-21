# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  DBI,
  bigrquery,
  magrittr,
  asam,
  sf,
  tidyverse
)

sf_use_s2(F)

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
          legend.box.spacing = unit(0, "inch"),
          strip.background = element_rect(fill=NA,color=NA))
}

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

focus_trip_id <- "308592000-3d9fa163d-da2c-0bf7-da5f-6a1236e3b332-01757325fd98"
focus_trip_id <- "257928000-1f8522c95-57a7-cecd-b21c-33f5bebff4af-015a715a22e8"
focus_trip_id <- "636091770-e47453787-75ba-5133-dfc7-996e6610c9ff-015cdae0c698" # From renato's test

voyage_info <- tbl(piracy, "voyage_info_v_20240228")
grided_data <- tbl(piracy, "gridded_data_5_v_20240307") %>% 
  mutate(lat_bin = lat_bin + 2.5,
         lon_bin = lon_bin + 2.5)
gridded_attacks <- tbl(piracy, "gridded_pirate_attacks_5_v_20240314")

trip_info <- voyage_info %>% 
  filter(trip_id == focus_trip_id) %>% 
  mutate(focus_trip_departure_date = sql("DATE(departure_timestamp)"),
         focus_trip_6_mo_before = sql("DATE_SUB(DATE(departure_timestamp), INTERVAL 6 MONTH)")) %>% 
  select(focus_trip_id = trip_id,
         from_port, to_port,
         focus_trip_departure_date,
         focus_trip_6_mo_before)

trip_grids <- grided_data %>% 
  filter(trip_id == focus_trip_id) %>% 
  collect()

trip_path <- tbl(piracy, "ungridded_data_v_20240228") %>% 
  filter(trip_id == focus_trip_id) %>% 
  collect()

other_trips <- voyage_info %>% 
  filter(!trip_id == focus_trip_id) %>% 
  mutate(departure_date = sql("DATE(departure_timestamp)")) %>% 
  inner_join(trip_info, by = c("from_port", "to_port")) %>% 
  filter(departure_date <= focus_trip_departure_date,
         departure_date > focus_trip_6_mo_before) %>% 
  select(trip_id)

other_trip_grids <- grided_data %>% 
  inner_join(other_trips, by = "trip_id") %>% 
  collect()

other_trip_paths <- tbl(piracy, "ungridded_data_v_20240228") %>%
  inner_join(other_trips, by = "trip_id") %>%
  collect()

attacks <- gridded_attacks %>% 
  filter(lat_bin %in% !!(other_trip_grids$lat_bin - 2.5),
         lon_bin %in% !!(other_trip_grids$lon_bin - 2.5),
         between(date, "2016-12-24", "2017-06-24")) %>% 
  collect() %>% 
  mutate(lat_bin = lat_bin +2.5, lon_bin = lon_bin +2.5) %>% 
  inner_join(other_trip_grids %>% select(lat_bin, lon_bin) %>% distinct(), by = c("lat_bin", "lon_bin")) %>% 
  select(lat_bin, lon_bin) %>% 
  distinct()

coast <- rnaturalearth::ne_countries(returnclass = "sf") %>% 
  st_crop(xmin = min(other_trip_grids$lon_bin) - 5,
          xmax = max(other_trip_grids$lon_bin) + 5,
          ymin = min(other_trip_grids$lat_bin) - 5,
          ymax = max(other_trip_grids$lat_bin) + 5)

p1 <- ggplot() +
  geom_sf(data = coast) +
  geom_tile(data = trip_grids,
            mapping = aes(x = lon_bin, y = lat_bin),
            fill = "red",
            alpha = 0.5) +
  geom_point(data = trip_path,
             mapping = aes(x = lon, y = lat),
             size = 0.1) +
  geom_point(data = attacks,
             mapping = aes(x = lon_bin, y = lat_bin),
             shape = 17,
             color = "black",
             size = 4) +
  theme_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("A) Focus trip")

p2 <- ggplot() +
  geom_sf(data = coast) +
  geom_tile(data = other_trip_grids,
            mapping = aes(x = lon_bin, y = lat_bin),
            fill = "blue",
            alpha = 0.5) +
  geom_point(data = other_trip_paths,
             mapping = aes(x = lon, y = lat, color = trip_id),
             size = 0.1) +
  geom_point(data = attacks,
             mapping = aes(x = lon_bin, y = lat_bin),
             shape = 17,
             color = "black",
             size = 4) +
  theme_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none") +
  ggtitle("B) Surrogate trips")

p3 <- ggplot() +
  geom_sf(data = coast) +
  geom_tile(data = other_trip_grids,
            mapping = aes(x = lon_bin, y = lat_bin),
            fill = "blue",
            alpha = 0.5) +
  geom_point(data = other_trip_paths,
             mapping = aes(x = lon, y = lat, color = trip_id),
             size = 0.1) +
  geom_tile(data = trip_grids,
            mapping = aes(x = lon_bin, y = lat_bin),
            fill = "red",
            alpha = 0.5) +
  geom_point(data = trip_path,
             mapping = aes(x = lon, y = lat),
             size = 0.1) +
  geom_point(data = attacks,
             mapping = aes(x = lon_bin, y = lat_bin),
             shape = 17,
             color = "black",
             size = 4) +
  theme_map() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none") +
  ggtitle("C) Overlay of A and B")


p <- cowplot::plot_grid(p1, p2, p3, ncol = 3)

ggsave(plot = p,
       filename = here("figures", "measure_construction_map.pdf"),
       width = 12.1,
       height = 8,
       units = "cm")
