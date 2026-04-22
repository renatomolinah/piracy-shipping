# Re-estimate voyage-level feature regressions clustering by country-pair only (instead of country-pair x year)

library(here)
library(tidyverse)
library(fixest)
library(modelsummary)

output_dir <- here("results", "figures_and_tables")

# --- Setup ---

wdb <- readRDS(here("data", "processed", "voyages.rds")) %>%
  mutate(drop = guinea + aden + asia) %>%
  filter(drop <= 1, best_vessel_type_cargo) %>%
  mutate(
    hotspot = ifelse(guinea == 1, "Guinea",
                     ifelse(aden == 1, "Aden",
                            ifelse(asia == 1, "Asia", "None"))),
    attacks_7day_num = number_previous_attacks_7_days_5_degrees
  )

setFixest_fml(..wctrl = ~ wind_speed + wind_vector + wave_height)

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

source(here("code", "table_helpers.R"))

# --- Regressions ---

m1_dist <- feols(distance ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair, lean = T)
m2_dist <- feols(distance ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(aden == 1), cluster = ~country_pair, lean = T)
m3_dist <- feols(distance ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(guinea == 1), cluster = ~country_pair, lean = T)
m4_dist <- feols(distance ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(asia == 1), cluster = ~country_pair, lean = T)

m1_time <- feols(time ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair, lean = T)
m2_time <- feols(time ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(aden == 1), cluster = ~country_pair, lean = T)
m3_time <- feols(time ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(guinea == 1), cluster = ~country_pair, lean = T)
m4_time <- feols(time ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(asia == 1), cluster = ~country_pair, lean = T)

m1_speed <- feols(speed ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair, lean = T)
m2_speed <- feols(speed ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(aden == 1), cluster = ~country_pair, lean = T)
m3_speed <- feols(speed ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(guinea == 1), cluster = ~country_pair, lean = T)
m4_speed <- feols(speed ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb %>% filter(asia == 1), cluster = ~country_pair, lean = T)

# --- Export ---

regs_dist <- list(m1_dist, m2_dist, m3_dist, m4_dist)
regs_time <- list(m1_time, m2_time, m3_time, m4_time)
regs_speed <- list(m1_speed, m2_speed, m3_speed, m4_speed)

rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Observations", as.character(nobs(m1_dist) %>% format(big.mark = ",")),
  as.character(nobs(m2_dist) %>% format(big.mark = ",")),
  as.character(nobs(m3_dist) %>% format(big.mark = ",")),
  as.character(nobs(m4_dist) %>% format(big.mark = ",")),
  "", "", "", "", "",
  "Hotspot FE", "X", "$\\bullet$", "$\\bullet$", "$\\bullet$"
)

msummary(list("Panel (A): Total Distance (km)" = regs_dist,
              "Panel (B): Total Time (hr)" = regs_time,
              "Panel (C): Average Speed (km/hr)" = regs_speed),
         coef_omit = c(-1),
         coef_rename = c("Encounters (7 day)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "Effect of Past Pirate Encounters on Shipping Voyages. \\label{tab:feature-country-cluster}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2). Each panel examines an observed feature in terms of total distance in kilometers (km), total time of the voyage in hours (hr), and the average speed of the voyage (km/hr). Each column is a different sample: Global uses the full sample; G. of Aden, G. of Guinea, and S.E. Asia restrict to voyages passing through each hotspot, respectively. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, top route, and month-by-year. Standard errors are clustered by country-to-country route."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = here(output_dir, "features_country_cluster.tex"))

add_adjust_box(here(output_dir, "features_country_cluster.tex"))
replace_table_headers(here(output_dir, "features_country_cluster.tex"), c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia"))
adjust_notes_font_size(here(output_dir, "features_country_cluster.tex"))

cat("Done. Output: features_country_cluster.tex\n")
