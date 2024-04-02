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

theme_map <- function(){
  theme_minimal(base_size = 7) %+replace%
    theme(panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "black"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
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

track_info <- tbl(piracy, "gridded_data_0_5_v_20240307") %>% 
  mutate(year = sql("EXTRACT(YEAR FROM date)")) %>% 
  select(year, lon_bin, lat_bin, trip_id) %>% 
  group_by(trip_id) %>% 
  add_count() %>% 
  ungroup() %>% 
  mutate(lon_bin = lon_bin + 0.25,
         lat_bin = lat_bin + 0.25)

pred_info <- tbl(piracy, "full_pred_global_v_20240328") %>% 
  mutate(cost = p_total - np_total,
         co2 = p_co2 - np_co2,
         nox = p_nox - np_nox,
         sox = p_sox - np_sox) %>% 
  select(trip_id, cost, co2, nox, sox)

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


# From https://www.whitehouse.gov/wp-content/uploads/2021/02/TechnicalSupportDocument_SocialCostofCarbonMethaneNitrousOxide.pdf
# Using 3% discount rate estimates for 2020 emissions
sc_co2 <- 51
sc_nox <- 18000
# From table 3 at https://www.ifo.de/DocDL/wp-2021-360-mier-adelowo-weissbart-social-cost-air-pollution-carbon.pdf
sc_sox <- 11217 * 1.31

total_grided <- local_grided %>% 
  filter(!is.na(lat_bin) | !is.na(lon_bin)) %>% 
  group_by(year, lat_bin, lon_bin) %>% 
  summarize(cost = sum(cost, na.rm = T), 
            co2 = sum(co2, na.rm = T),
            nox = sum(nox, na.rm = T),
            sox = sum(sox, na.rm = T),
            .groups = "drop") %>% 
  select(-year) %>% 
  group_by(lat_bin, lon_bin) %>% 
  summarize_all(mean, na.rm = T, .groups = "drop") %>% 
  mutate(total = cost + (co2 * sc_co2) + (nox * sc_nox), (sox * sc_nox))

## PROCESSING ##################################################################
# Get cost by ASAM region
total_by_asam <- total_grided %>% 
  st_as_sf(coords = c("lon_bin", "lat_bin"),
           crs = 4326) %>% 
  st_join(asam_regions, join = st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  group_by(asam_region) %>% 
  summarize_all(sum, na.rm = T) %>% 
  mutate()

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
                     option = c("B", "D", "E", "F"),
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
                  aes(label = asam_region),
                  size = 2,
                  label.size = 0.1,
                  label.pading = unit(0.01, "lines")) +
    scale_fill_viridis_c(option = option) + 
    scale_x_continuous(expand = c(0, 1)) +
    scale_y_continuous(expand = c(0, 1)) +
    theme_map() +
    theme(panel.background = element_rect(fill = "gray20"),
          legend.position = "top") +
    guides(fill = guide_colorbar(title = legend,
                                 title.position = "top",
                                 title.hjust = 0.5,
                                 frame.colour = "black",
                                 ticks.colour = "black",
                                 barwidth = unit(5, "cm"),
                                 barheight = unit(0.5, "cm")))
}

cost_map <- make_map(data = total_grided,
                     var = cost,
                     option = "B",
                     legend = "Cost (Million USD)\nlog10-transformed")
co2_map <- make_map(total_grided,
                    var = co2,
                    option = "D",
                    legend = "CO2 (Tons)\nlog10-transformed")
nox_map <- make_map(total_grided,
                    var = nox,
                    option = "E",
                    legend = "NOx (Tons)\nlog10-transformed")
sox_map <- make_map(total_grided,
                    var = sox,
                    option = "F",
                    legend = "SOx (Tons)\nlog10-transformed")


total_map <- make_map(data = total_grided,
                      var = total,
                      option = "G",
                      legend = "Total Cost (Million USD)\nlog10-transformed")

zonal_stats <- ggplot(total_by_asam,
                      aes(x = as.character(asam_region),
                          y = total / 1e6)) + 
  geom_col(color = "black",
           fill = "gray50") +
  theme_minimal(base_size = 7) +
  theme(legend.position = "None") +
  labs(x = "ASAM Region",
       y = "Total costs (Million USD)")


counterfactual_maps <- cowplot::plot_grid(cost_map,
                                          co2_map,
                                          nox_map,
                                          sox_map)

p <- cowplot::plot_grid(counterfactual_maps,
                        zonal_stats,
                        ncol = 1,
                        rel_heights = c(3, 1))

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------
ggsave(plot = p,
       filename = here("figures", "counterfactual_maps.pdf"),
       width = 18.4,
       height = 17,
       units = "cm")