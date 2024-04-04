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
coast <- rnaturalearth::ne_countries(returnclass = "sf")
coastline <- rnaturalearth::ne_coastline(returnclass = "sf")

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

pred_info <- tbl(piracy, "full_pred_global_v_20240404") %>% 
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

## PROCESSING ##################################################################
# From https://www.whitehouse.gov/wp-content/uploads/2021/02/TechnicalSupportDocument_SocialCostofCarbonMethaneNitrousOxide.pdf
# Using 3% discount rate estimates for 2020 emissions
sc_co2 <- 51
sc_nox <- 18000
# From table 3 at https://www.ifo.de/DocDL/wp-2021-360-mier-adelowo-weissbart-social-cost-air-pollution-carbon.pdf
sc_sox <- 11217 * 1.31 #convert 2015 to 2020 USD

total_grided <- local_grided %>% 
  filter(!is.na(lat_bin) | !is.na(lon_bin)) %>% 
  group_by(year, lat_bin, lon_bin) %>% 
  summarize(cost = sum(cost, na.rm = T) / 1e3,                                  # Cost is reported in K USD so we convert to M USD 
            co2 = sum(co2, na.rm = T),
            nox = sum(nox, na.rm = T) / 1e3,                                    # NOx is reported in Kg so convert to Ton
            sox = sum(sox, na.rm = T) / 1e3,                                    # SOx is reported in Kg so convert to Ton
            .groups = "drop") %>% 
  select(-year) %>% 
  group_by(lat_bin, lon_bin) %>% 
  summarize_all(mean, na.rm = T, .groups = "drop") %>% 
  mutate(private = cost,
         public = (co2 * sc_co2 / 1e6) + (nox * sc_nox / 1e6) + (sox * sc_sox / 1e6),
         total = private + public)

saveRDS(object = total_grided,
        file = here("output_data", "gridded_public_and_private_counterfactual_predictions.rds"))

# Get cost by ASAM region
total_by_asam <- total_grided %>% 
  st_as_sf(coords = c("lon_bin", "lat_bin"),
           crs = 4326) %>% 
  st_join(asam_regions, join = st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  group_by(asam_region) %>% 
  summarize_all(sum, na.rm = T) %>% 
  mutate(asam_region = as.character(asam_region))

## VISUALIZE ###################################################################

# Build a figure with four panels: Costs, CO2, NOx, SOx
# Each of them is a map, showing the costs / emissions apportioned along the
# trip id, for the entirety of the study period. 
# Then, add a bar chart by ASMR region.
# X ----------------------------------------------------------------------------

make_map <- function(data,
                     var,
                     option = "G",
                     legend = "Cost (Million USD)\nlog10-transformed") {
  ggplot() +
    geom_tile(data = data,
              aes(x = lon_bin, y = lat_bin, fill = {{var}})) +
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
                  label.padding = unit(0.1, "lines")) +
    scale_fill_viridis_c(option = option, trans = "log10") + 
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

# A map of total costs, not part of the paper but goof to have
total_map <- make_map(data = total_grided,
                      var = total / 1e3,
                      option = "G",
                      legend = "Total Cost (Billion USD)")

# Map of costs to shippers
cost_map <- make_map(data = total_grided,
                     var = cost,
                     option = "B",
                     legend = "Cost (Million USD)")

# Map of tons of pollutants emitted
co2_map <- make_map(total_grided,
                    var = co2,
                    option = "D",
                    # legend = bquote(atop(CO[2],"(Metric tons) log10-transformed"))
                    legend = expression(CO[2]~"(Metric tons)")
                    )
nox_map <- make_map(total_grided,
                    var = nox,
                    option = "E",
                    legend = "NOx (Metric tons)")
sox_map <- make_map(total_grided,
                    var = sox,
                    option = "F",
                    legend = "SOx (Metric tons)")

zonal_stats <- total_by_asam %>% 
  select(asam_region, private, public) %>% 
  pivot_longer(cols = c(private, public)) %>% 
  mutate(asam_region = fct_reorder(.f = asam_region, .x = value, .fun = "sum", .desc = T)) %>%
  ggplot(mapping = aes(x = asam_region,
                       y = value,
                       fill = str_to_title(name))) + 
  geom_col(color = "black") +
  geom_text(data = total_by_asam,
            mapping = aes(x = asam_region,
                          y = total,
                          label = format(x = round(total),
                                         big.mark = ",")),
            nudge_y = 150, 
            inherit.aes = F,
            size = 2) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 10) +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) +
  labs(x = "ASAM Region",
       y = "Total costs (Million USD)",
       fill = "Sector")


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
       height = 17.5,
       units = "cm")
