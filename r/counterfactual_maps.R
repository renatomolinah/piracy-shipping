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
  DBI,
  bigrquery,
  magrittr,
  ggsflabel,
  rnaturalearth,
  asam,
  sf,
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

# Load data --------------------------------------------------------------------
coast <- rnaturalearth::ne_countries(returnclass = "sf")
coastline <- rnaturalearth::ne_coastline(returnclass = "sf")

sf_use_s2(F)

asam_regions <- asam_subregions() %>% 
  select(asam_region = REGION) %>% 
  st_buffer(dist = 0.01) %>% 
  group_by(asam_region) %>% 
  summarize() %>% 
  ungroup()

track_info <- tbl(piracy, "gridded_data_0_5") %>% 
  mutate(year = sql("EXTRACT(YEAR FROM date)")) %>% 
  select(year, lon_bin, lat_bin, trip_id) %>% 
  group_by(trip_id) %>% 
  add_count() %>% 
  ungroup()

pred_info <- tbl(piracy, "super_light_full_pred_global")

# Build data --------------------------------------------------------------------
grided <- pred_info %>% 
  left_join(track_info, by = "trip_id") %>% 
  group_by(year, lat_bin, lon_bin) %>% 
  summarize(cost = sum(cost / n, na.rm = T), 
            co2 = sum(co2 / n, na.rm = T),
            nox = sum(nox / n, na.rm = T),
            sox = sum(sox / n, na.rm = T),
            .groups = "drop") 

local_grided <- collect(grided)

total_grided <- local_grided %>% 
  filter(!is.na(lat_bin) | !is.na(lon_bin)) %>% 
  group_by(lat_bin, lon_bin) %>% 
  summarize(cost = sum(cost, na.rm = T), 
            co2 = sum(co2, na.rm = T),
            nox = sum(nox, na.rm = T),
            sox = sum(sox, na.rm = T))

## PROCESSING ##################################################################
# Get cost by ASAM region
total_by_asam <- total_grided %>% 
  st_as_sf(coords = c("lon_bin", "lat_bin"),
           crs = 4326) %>% 
  st_join(asam_regions, join = st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  group_by(asam_region) %>% 
  summarize_all(sum, na.rm = T)

total_by_asam %>%
  arrange(cost) %>%
  mutate(pct_cost = cost / sum(cost), 
         cumsum_pct_cost = cumsum(pct_cost),
         rank = rank(cost)) %>%
  ggplot(aes(x = rank, y = cumsum_pct_cost)) +
  geom_line()

zonal_stats <- ggplot(total_by_asam,
       aes(x = as.character(asam_region), y = cost, fill = log10(cost))) + 
  geom_col(color = "black") +
  theme_minimal() +
  scale_fill_viridis_c(option = "A") +
  theme(legend.position = "None") +
  labs(x = "ASAM Region",
       y = "Cost (Million USD)") +
  coord_flip()

plot_grid(cost_map,
          zonal_stats,
          ncol = 2,
          align = "hv",
          rel_widths = c(3, 1),
          axis = "l")
  

## VISUALIZE ###################################################################

# Build a figure with four panels: Costs, CO2, NOx, SOx
# Each of them is a map, showing the costs / emissions apportioned along the
# trip id, for the entirety of the study period. 
# Then, add a bar chart by ASMR region, or perhaps a Lorenz curve.
# For supplementary figure purposes, build a single figure by metric, and include
# one map per year.

# X ----------------------------------------------------------------------------

make_map <- function(data,
                     var,
                     option = c("A", "D", "E", "F"),
                     legend = "Cost (Million USD)\nlog10-transformed") {
  ggplot() +
    geom_tile(data = data,
              aes(x = lon_bin, y = lat_bin, fill = log10({{var}}))) +
    geom_sf(data = asam_regions,
            fill = "transparent",
            color = "white") +
    geom_sf(data = coastline,
            color = "white") +
    geom_sf(data = coast,
            fill = "black",
            color = "black") +
    geom_sf_label(data = asam_regions,
                  aes(label = asam_region)) +
    scale_fill_viridis_c(option = option) + 
    scale_x_continuous(expand = c(0, 1)) +
    scale_y_continuous(expand = c(0, 1)) +
    theme_void() +
    theme(panel.background = element_rect(fill = "gray20"),
          legend.position = "top") +
    guides(fill = guide_colorbar(title = legend,
                                 title.position = "top",
                                 title.hjust = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black", barwidth = unit(10, "cm")))
}

cost_map <- make_map(data = total_grided,
                     var = cost,
                     option = "A",
                     legend = "Cost (Million USD)\nlog10-transformed")
co2_map <- make_map(total_grided,
                    var = co2,
                    option = "D",
                    legend = "CO2 (Tons)\nlog10-transformed")
nox_map <- make_map(total_grided,
                    var = nox,
                    option = "E",
                    legend = "NOx (Tons USD)\nlog10-transformed")
sox_map <- make_map(total_grided,
                    var = sox,
                    option = "F",
                    legend = "Sox (Tons USD)\nlog10-transformed")



cowplot::plot_grid(
  cost_map,
  co2_map,
  nox_map,
  sox_map)

# Now facts by year and measure
ggplot() +
  geom_tile(data = local_grided,
            aes(x = lon_bin, y = lat_bin, fill = log10(cost + 1))) +
  geom_sf(data = asam_regions, fill = "transparent", color = "white") +
  geom_sf(data = coast, fill = "black", color = "black") +
  scale_fill_viridis_c(option = "A") +
  labs(fill = "Cost (Million USD)") +
  guides(fill = guide_colorbar(title.position = "top",
                               frame.colour = "black",
                               ticks.colour = "black")) +
  theme_void() +
  facet_wrap(~year)

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------