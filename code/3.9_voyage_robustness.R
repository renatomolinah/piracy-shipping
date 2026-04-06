# =============================================================================
# VOYAGE-LEVEL ROBUSTNESS
# =============================================================================
# Robustness checks for the voyage-level analysis:
#   - Route-port-pair FE (tighter than country-pair FE)
#   - Alternative clustering (country-pair only)
#   - Forward-attack placebo
#   - Prior-route exposure robustness
#
# Outputs:
#   features_routefe.tex
#   features_country_cluster.tex
#   forward_attack_placebo.tex
#   route_history_exposure_robustness.tex
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

add_adjust_box <- function(file, line_before = "\\begin{adjustbox}{width = .9\\textwidth}", line_after = "\\end{adjustbox}", before = "\\begin{threeparttable}", after = "\\end{threeparttable}") {
  lines <- readLines(file)
  after_line <- grep(after, lines, fixed = TRUE)
  before_line <- grep(before, lines, fixed = TRUE)
  lines <- c(lines[1:(before_line-1)], line_before, lines[(before_line):(after_line)], line_after, lines[(after_line+1):length(lines)])
  writeLines(lines, file)
}

replace_table_headers <- function(file, new_headers) {
  lines <- readLines(file)
  header_line_idx <- which(grepl("& \\(", lines))
  if (length(header_line_idx) == 0) {
    stop("Header line not found")
  }
  header_line <- lines[header_line_idx]
  for (i in seq_along(new_headers)) {
    header_line <- sub(paste0("\\(", i, "\\)"), new_headers[i], header_line)
  }
  lines[header_line_idx] <- header_line
  writeLines(lines, file)
}

adjust_notes_font_size <- function(file, font_size_command = "\\scriptsize") {
  lines <- readLines(file)
  item_line_index <- grep("\\item", lines, fixed = TRUE)
  if (length(item_line_index) > 0) {
    lines[item_line_index] <- gsub("\\item", paste0("\\item ", font_size_command), lines[item_line_index], fixed = TRUE)
  }
  writeLines(lines, file)
}


# =============================================================================
# 5.R1 - Comment 6.c: Route fixed effects
# =============================================================================

# Distance regressions
m1_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb,
                 cluster = ~country_pair ^ year,
                 lean = T)

m2_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb %>% filter(aden == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m3_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb %>% filter(guinea == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m4_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb %>% filter(asia == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

# Time regressions
m1_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb,
                 cluster = ~country_pair ^ year,
                 lean = T)

m2_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb %>% filter(aden == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m3_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb %>% filter(guinea == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m4_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                 wdb %>% filter(asia == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

# Speed regressions
m1_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                  wdb,
                  cluster = ~country_pair ^ year,
                  lean = T)

m2_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                  wdb %>% filter(aden == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m3_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                  wdb %>% filter(guinea == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m4_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year,
                  wdb %>% filter(asia == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)


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
         title = "Effect of Past Pirate Encounters on Shipping Voyages. \\label{tab:feature-routefe-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2) in the main text. Each panel examines an observed feature in terms of total distance in kilometers (km), total time of the voyage in hours (hr), and the average speed of the voyage (km/hr). Each column is a different sample: Global uses the full sample; G. of Aden, G. of Guinea, and S.E. Asia restrict to voyages passing through each hotspot, respectively. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. This table replaces country-to-country and top route fixed effects with port-to-port pair fixed effects. Other fixed effects include vessel type, vessel size, hotspot, and month-by-year. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = here(output_dir, "features_routefe.tex"))

# Apply formatting
add_adjust_box(here(output_dir, "features_routefe.tex"))
replace_table_headers(here(output_dir, "features_routefe.tex"), c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia"))
adjust_notes_font_size(here(output_dir, "features_routefe.tex"))


# =============================================================================
# 4.R1 - Comment 6.d: Country-country clustering 
# =============================================================================

# Distance regressions
m1_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb,
                 cluster = ~country_pair,
                 lean = T)

m2_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(aden == 1),
                 cluster = ~country_pair,
                 lean = T)

m3_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(guinea == 1),
                 cluster = ~country_pair,
                 lean = T)

m4_dist <- feols(distance ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(asia == 1),
                 cluster = ~country_pair,
                 lean = T)

# Time regressions
m1_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb,
                 cluster = ~country_pair,
                 lean = T)

m2_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(aden == 1),
                 cluster = ~country_pair,
                 lean = T)

m3_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(guinea == 1),
                 cluster = ~country_pair,
                 lean = T)

m4_time <- feols(time ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(asia == 1),
                 cluster = ~country_pair,
                 lean = T)

# Speed regressions
m1_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb,
                  cluster = ~country_pair,
                  lean = T)

m2_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(aden == 1),
                  cluster = ~country_pair,
                  lean = T)

m3_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(guinea == 1),
                  cluster = ~country_pair,
                  lean = T)

m4_speed <- feols(speed ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(asia == 1),
                  cluster = ~country_pair,
                  lean = T)


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
         title = "Effect of Past Pirate Encounters on Shipping Voyages. \\label{tab:feature-country-cluster-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2) in the main text. Each panel examines an observed feature in terms of total distance in kilometers (km), total time of the voyage in hours (hr), and the average speed of the voyage (km/hr). Each column is a different sample: Global uses the full sample; G. of Aden, G. of Guinea, and S.E. Asia restrict to voyages passing through each hotspot, respectively. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, top route, and month-by-year. Standard errors are clustered by country-to-country route."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = here(output_dir, "features_country_cluster.tex"))

# Apply formatting
add_adjust_box(here(output_dir, "features_country_cluster.tex"))
replace_table_headers(here(output_dir, "features_country_cluster.tex"), c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia"))
adjust_notes_font_size(here(output_dir, "features_country_cluster.tex"))





