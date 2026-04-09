# =============================================================================
# TRIP FEATURE REGRESSIONS
# =============================================================================
# This script performs regression analysis on trip-level features (distance, time, speed)
# examining how pirate attacks affect shipping behavior across different regions.
# =============================================================================

library(here)
library(tidyverse)
library(fixest)
library(modelsummary)

# Specify output directory
output_dir <- here("results", "figures_and_tables")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

# Load the main dataset
wdb <- readRDS(here("data", "processed", "voyages.rds")) %>%
  mutate(
    drop = guinea + aden + asia
  ) %>%
  filter(drop <= 1, best_vessel_type_cargo) %>%
  mutate(
    hotspot = ifelse(guinea == 1, "Guinea",
                     ifelse(aden == 1, "Aden",
                            ifelse(asia == 1, "Asia", "None")))
  )

# Create attack variable
wdb <- wdb %>% mutate(attacks_7day_num = number_previous_attacks_7_days_5_degrees)

# =============================================================================
# 2. SET UP FORMULAS AND CONTROLS
# =============================================================================

# Define weather controls (now including wave height)
setFixest_fml(..wctrl = ~ wind_speed + wind_vector + wave_height)

# Set up variable dictionary for clean output
setFixest_dict(c(
  time = "Total Time (hrs)",
  distance = "Total Distance (km)",
  speed = "Average Speed (km/hr)",
  attacks_7day_num = "Encounters (7 day)",
  hotspot = "Hotspot",
  vessel_type = "Vessel type",
  tonnage_decile = "Vessel size",
  country_pair = "Country Combo.",
  year = "Year",
  month = "Month",
  top_route = "Top Route",
  'month^year' = "Month-by-Year",
  wave_height = "Wave Height (m)"
))

# =============================================================================
# 3. HELPER FUNCTIONS FOR LATEX TABLES
# =============================================================================
source(here("code", "table_helpers.R"))

# =============================================================================
# 4. MAIN FEATURE REGRESSIONS
# =============================================================================

# Distance regressions
m1_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb,
                 cluster = ~country_pair ^ year,
                 lean = T)

m2_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(aden == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m3_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(guinea == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m4_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(asia == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

# Time regressions
m1_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb,
                 cluster = ~country_pair ^ year,
                 lean = T)

m2_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(aden == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m3_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(guinea == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m4_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(asia == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

# Speed regressions
m1_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb,
                  cluster = ~country_pair ^ year,
                  lean = T)

m2_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(aden == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m3_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(guinea == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m4_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(asia == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

# =============================================================================
# 5. CREATE MAIN FEATURE TABLE
# =============================================================================

# Set up results for manuscript table
regs_dist <- list(m1_dist, m2_dist, m3_dist, m4_dist)
regs_time <- list(m1_time, m2_time, m3_time, m4_time)
regs_speed <- list(m1_speed, m2_speed, m3_speed, m4_speed)

# Create rows for additional information
rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Observations", as.character(nobs(m1_dist) %>% format(big.mark = ",")),
  as.character(nobs(m2_dist) %>% format(big.mark = ",")),
  as.character(nobs(m3_dist) %>% format(big.mark = ",")),
  as.character(nobs(m4_dist) %>% format(big.mark = ",")),
  "", "", "", "", "",
  "Hotspot FE",  "X", "$\\bullet$", "$\\bullet$", "$\\bullet$"
)

# Create main feature table
msummary(list("Panel (A): Total Distance (km)" = regs_dist,
              "Panel (B): Total Time (hr)" = regs_time,
              "Panel (C): Average Speed (km/hr)" = regs_speed),
         coef_omit = c(-1),
         coef_rename = c("Encounters (7 day)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "Effect of Past Pirate Encounters on Shipping Voyages. \\label{tab:feature-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2). Each panel examines an observed feature in terms of total distance in kilometers (km), total time of the voyage in hours (hr), and the average speed of the voyage (km/hr). Each column is a different sample: Global uses the full sample; G. of Aden, G. of Guinea, and S.E. Asia restrict to voyages passing through each hotspot, respectively. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, top route, and month-by-year. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = here(output_dir, "features.tex"))

# Apply formatting
add_adjust_box(here(output_dir, "features.tex"))
replace_table_headers(here(output_dir, "features.tex"), c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia"))
adjust_notes_font_size(here(output_dir, "features.tex"))

# =============================================================================
# 6. SPECIFICATION ANALYSIS - DISTANCE
# =============================================================================

# Distance specification regressions
spec_dist_1 <- feols(distance ~ attacks_7day_num
                     | month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_dist_2 <- feols(distance ~ attacks_7day_num + ..wctrl
                     | month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_dist_3 <- feols(distance ~ attacks_7day_num + ..wctrl
                     |country_pair+ month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_dist_4 <- feols(distance ~ attacks_7day_num + ..wctrl
                     |country_pair+ vessel_type + tonnage_decile + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_dist_5 <- feols(distance ~ attacks_7day_num + ..wctrl
                     |country_pair+ vessel_type + tonnage_decile + hotspot + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_dist_6 <- feols(distance ~ attacks_7day_num + ..wctrl
                     |country_pair+ vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_dist_7 <- feols(distance ~ attacks_7day_num + ..wctrl
                     | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

# Set up results for distance specification table
spec_dist <- list(spec_dist_1, spec_dist_2, spec_dist_3, spec_dist_4, spec_dist_5, spec_dist_6, spec_dist_7)

# Create rows for distance specification table
rows_spec <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "",
  "Observations", as.character(nobs(spec_dist_1) %>% format(big.mark = ",")),
  as.character(nobs(spec_dist_2) %>% format(big.mark = ",")),
  as.character(nobs(spec_dist_3) %>% format(big.mark = ",")),
  as.character(nobs(spec_dist_4) %>% format(big.mark = ",")),
  as.character(nobs(spec_dist_5) %>% format(big.mark = ",")),
  as.character(nobs(spec_dist_6) %>% format(big.mark = ",")),
  as.character(nobs(spec_dist_7) %>% format(big.mark = ",")),
  "", "", "", "", "", "", "", "",
  "Country Combo. FE", "", "", "X", "X", "X", "X", "",
  "Vessel Type FE",    "", "", "", "X", "X", "X", "X",
  "Vessel Size FE",    "", "", "", "X", "X", "X", "X",
  "Hotspot FE",        "", "", "", "", "X", "X", "X",
  "Top Route FE",      "", "", "", "", "", "X", "",
  "Port-to-Port FE",   "", "", "", "", "", "", "X",
  "Month-by-Year FE",  "X", "X", "X", "X", "X", "X", "X"
)

# Create distance specification table
msummary(spec_dist,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on Voyage Distance. \\label{tab:spec-dist-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2). Each column is a different specification. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Column (7) replaces country-to-country and top route fixed effects with port-to-port pair fixed effects. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_distance.tex"))

add_adjust_box(here(output_dir, "spec_distance.tex"))
adjust_notes_font_size(here(output_dir, "spec_distance.tex"))

# =============================================================================
# 7. SPECIFICATION ANALYSIS - TIME
# =============================================================================

# Time specification regressions
spec_time_1 <- feols(time ~ attacks_7day_num
                     | month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_time_2 <- feols(time ~ attacks_7day_num + ..wctrl
                     | month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_time_3 <- feols(time ~ attacks_7day_num + ..wctrl
                     |country_pair+ month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_time_4 <- feols(time ~ attacks_7day_num + ..wctrl
                     |country_pair+ vessel_type + tonnage_decile + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_time_5 <- feols(time ~ attacks_7day_num + ..wctrl
                     |country_pair+ vessel_type + tonnage_decile + hotspot + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_time_6 <- feols(time ~ attacks_7day_num + ..wctrl
                     |country_pair+ vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

spec_time_7 <- feols(time ~ attacks_7day_num + ..wctrl
                     | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb,
                     cluster = ~country_pair ^ year,
                     lean = T)

# Set up results for time specification table
spec_time <- list(spec_time_1, spec_time_2, spec_time_3, spec_time_4, spec_time_5, spec_time_6, spec_time_7)

# Create time specification table
msummary(spec_time,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on Voyage Time. \\label{tab:spec-time-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2). Each column is a different specification. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Column (7) replaces country-to-country and top route fixed effects with port-to-port pair fixed effects. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_time.tex"))

add_adjust_box(here(output_dir, "spec_time.tex"))
adjust_notes_font_size(here(output_dir, "spec_time.tex"))

# =============================================================================
# 8. SPECIFICATION ANALYSIS - SPEED
# =============================================================================

# Speed specification regressions
spec_speed_1 <- feols(speed ~ attacks_7day_num
                      | month^year, wdb,
                      cluster = ~country_pair ^ year,
                      lean = T)

spec_speed_2 <- feols(speed ~ attacks_7day_num + ..wctrl
                      | month^year, wdb,
                      cluster = ~country_pair ^ year,
                      lean = T)

spec_speed_3 <- feols(speed ~ attacks_7day_num + ..wctrl
                      |country_pair+ month^year, wdb,
                      cluster = ~country_pair ^ year,
                      lean = T)

spec_speed_4 <- feols(speed ~ attacks_7day_num + ..wctrl
                      |country_pair+ vessel_type + tonnage_decile + month^year, wdb,
                      cluster = ~country_pair ^ year,
                      lean = T)

spec_speed_5 <- feols(speed ~ attacks_7day_num + ..wctrl
                      |country_pair+ vessel_type + tonnage_decile + hotspot + month^year, wdb,
                      cluster = ~country_pair ^ year,
                      lean = T)

spec_speed_6 <- feols(speed ~ attacks_7day_num + ..wctrl
                      |country_pair+ vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb,
                      cluster = ~country_pair ^ year,
                      lean = T)

spec_speed_7 <- feols(speed ~ attacks_7day_num + ..wctrl
                      | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb,
                      cluster = ~country_pair ^ year,
                      lean = T)

# Set up results for speed specification table
spec_speed <- list(spec_speed_1, spec_speed_2, spec_speed_3, spec_speed_4, spec_speed_5, spec_speed_6, spec_speed_7)

# Create speed specification table
msummary(spec_speed,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on Voyage Speed. \\label{tab:spec-speed-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2). Each column is a different specification. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Column (7) replaces country-to-country and top route fixed effects with port-to-port pair fixed effects. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_speed.tex"))

add_adjust_box(here(output_dir, "spec_speed.tex"))
adjust_notes_font_size(here(output_dir, "spec_speed.tex"))

# =============================================================================
# 9. SUMMARY STATISTICS
# =============================================================================

# Print summary of results
cat("Trip feature regressions completed.\n")
cat("Number of observations:", nrow(wdb), "\n")
cat("Tables saved to:", here("results", "figures_and_tables"), "\n")
cat("- features.tex: Main feature table\n")
cat("- spec_distance.tex: Distance specification table\n")
cat("- spec_time.tex: Time specification table\n")
cat("- spec_speed.tex: Speed specification table\n")
