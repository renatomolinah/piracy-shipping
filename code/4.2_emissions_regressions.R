# =============================================================================
# EMISSIONS REGRESSIONS ANALYSIS
# =============================================================================
# This script performs regression analysis on shipping emissions (CO2, NOx, SOx)
# examining how pirate attacks affect emissions across different regions.
# =============================================================================

library(here)
library(tidyverse)
library(fixest)
library(modelsummary)

output_dir <- here("results", "figures_and_tables")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

wdb <- readRDS(here("data", "processed", "voyages.rds")) %>%
  mutate(
    drop = guinea + aden + asia
  ) %>%
  filter(drop <= 1, best_vessel_type_cargo) %>%
  mutate(
    hotspot = ifelse(guinea == 1, "Guinea",
                     ifelse(aden == 1, "Aden",
                            ifelse(asia == 1, "Asia", "None"))),
    total_co2 = emissions_co2_mt_inst,
    total_nox = emissions_nox_kg_inst,
    total_sox = emissions_sox_kg_inst
  ) %>%
  mutate(attacks_7day_num = number_previous_attacks_7_days_5_degrees)

# =============================================================================
# 2. SET UP FORMULAS AND CONTROLS
# =============================================================================

setFixest_fml(..wctrl = ~ wind_speed + wind_vector + wave_height)

setFixest_dict(c(
  total_co2 = "Total CO2 (tons)",
  total_nox = "Total NOx (kg)",
  total_sox = "Total SOx (kg)",
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
  for (i in 1:length(new_headers)) {
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
# 4. MAIN EMISSIONS REGRESSIONS
# =============================================================================

# CO2 regressions
m1_co2 <- feols(total_co2 ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb,
                cluster = ~country_pair ^ year,
                lean = T)

m2_co2 <- feols(total_co2 ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(aden == 1),
                cluster = ~country_pair ^ year,
                lean = T)

m3_co2 <- feols(total_co2 ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(guinea == 1),
                cluster = ~country_pair ^ year,
                lean = T)

m4_co2 <- feols(total_co2 ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(asia == 1),
                cluster = ~country_pair ^ year,
                lean = T)

# NOx regressions
m1_nox <- feols(total_nox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb,
                cluster = ~country_pair ^ year,
                lean = T)

m2_nox <- feols(total_nox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(aden == 1),
                cluster = ~country_pair ^ year,
                lean = T)

m3_nox <- feols(total_nox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(guinea == 1),
                cluster = ~country_pair ^ year,
                lean = T)

m4_nox <- feols(total_nox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(asia == 1),
                cluster = ~country_pair ^ year,
                lean = T)

# SOx regressions
m1_sox <- feols(total_sox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb,
                cluster = ~country_pair ^ year,
                lean = T)

m2_sox <- feols(total_sox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(aden == 1),
                cluster = ~country_pair ^ year,
                lean = T)

m3_sox <- feols(total_sox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(guinea == 1),
                cluster = ~country_pair ^ year,
                lean = T)

m4_sox <- feols(total_sox ~ attacks_7day_num + ..wctrl
                | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                wdb %>% filter(asia == 1),
                cluster = ~country_pair ^ year,
                lean = T)

# =============================================================================
# 5. CREATE MAIN EMISSIONS TABLE
# =============================================================================

regs_co2 <- list(m1_co2, m2_co2, m3_co2, m4_co2)
regs_nox <- list(m1_nox, m2_nox, m3_nox, m4_nox)
regs_sox <- list(m1_sox, m2_sox, m3_sox, m4_sox)

rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Observations", as.character(nobs(m1_co2) %>% format(big.mark = ",")),
  as.character(nobs(m2_co2) %>% format(big.mark = ",")),
  as.character(nobs(m3_co2) %>% format(big.mark = ",")),
  as.character(nobs(m4_co2) %>% format(big.mark = ",")),
  "", "", "", "", "",
  "Hotspot FE",  "X", "$\\bullet$", "$\\bullet$", "$\\bullet$"
)

msummary(list("Panel (A): CO2 (tons)" = regs_co2,
              "Panel (B): NOx (kg)" = regs_nox,
              "Panel (C): SOx (kg)" = regs_sox),
         coef_omit = c(-1),
         coef_rename = c("Encounters (7 day)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "Effect of Past Pirate Encounters on Shipping Emissions. \\label{tab:emission-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2) in the main text. Each panel examines a calculated emission in terms of CO2 (tons), NOx (kg), and SOx (kg). Each column is a different sample: Global uses the full sample; G. of Aden, G. of Guinea, and S.E. Asia restrict to voyages passing through each hotspot, respectively. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, top route, and month-by-year. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = here(output_dir, "emissions.tex"))

add_adjust_box(here(output_dir, "emissions.tex"))
replace_table_headers(here(output_dir, "emissions.tex"), c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia"))
adjust_notes_font_size(here(output_dir, "emissions.tex"))

# =============================================================================
# 6. SPECIFICATION ANALYSIS - CO2
# =============================================================================

spec_co2_1 <- feols(total_co2 ~ attacks_7day_num | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_co2_2 <- feols(total_co2 ~ attacks_7day_num + ..wctrl | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_co2_3 <- feols(total_co2 ~ attacks_7day_num + ..wctrl | country_pair + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_co2_4 <- feols(total_co2 ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_co2_5 <- feols(total_co2 ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_co2_6 <- feols(total_co2 ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_co2_7 <- feols(total_co2 ~ attacks_7day_num + ..wctrl | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)

spec_co2 <- list(spec_co2_1, spec_co2_2, spec_co2_3, spec_co2_4, spec_co2_5, spec_co2_6, spec_co2_7)

rows_spec <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)",
  "", "", "", "", "", "", "", "",
  "Observations", as.character(nobs(spec_co2_1) %>% format(big.mark = ",")),
  as.character(nobs(spec_co2_2) %>% format(big.mark = ",")),
  as.character(nobs(spec_co2_3) %>% format(big.mark = ",")),
  as.character(nobs(spec_co2_4) %>% format(big.mark = ",")),
  as.character(nobs(spec_co2_5) %>% format(big.mark = ",")),
  as.character(nobs(spec_co2_6) %>% format(big.mark = ",")),
  as.character(nobs(spec_co2_7) %>% format(big.mark = ",")),
  "", "", "", "", "", "", "", "",
  "Country Combo. FE", "", "", "X", "X", "X", "X", "",
  "Vessel Type FE",    "", "", "", "X", "X", "X", "X",
  "Vessel Size FE",    "", "", "", "X", "X", "X", "X",
  "Hotspot FE",        "", "", "", "", "X", "X", "X",
  "Top Route FE",      "", "", "", "", "", "X", "",
  "Port-to-Port FE",   "", "", "", "", "", "", "X",
  "Month-by-Year FE",  "X", "X", "X", "X", "X", "X", "X"
)

msummary(spec_co2,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on CO2 Emissions. \\label{tab:spec-co2-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2) in the main text. Each column is a different specification. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Column (7) replaces country-to-country and top route fixed effects with port-to-port pair fixed effects. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_co2.tex"))

add_adjust_box(here(output_dir, "spec_co2.tex"))
adjust_notes_font_size(here(output_dir, "spec_co2.tex"))

# =============================================================================
# 7. SPECIFICATION ANALYSIS - NOX
# =============================================================================

spec_nox_1 <- feols(total_nox ~ attacks_7day_num | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_nox_2 <- feols(total_nox ~ attacks_7day_num + ..wctrl | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_nox_3 <- feols(total_nox ~ attacks_7day_num + ..wctrl | country_pair + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_nox_4 <- feols(total_nox ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_nox_5 <- feols(total_nox ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_nox_6 <- feols(total_nox ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_nox_7 <- feols(total_nox ~ attacks_7day_num + ..wctrl | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)

spec_nox <- list(spec_nox_1, spec_nox_2, spec_nox_3, spec_nox_4, spec_nox_5, spec_nox_6, spec_nox_7)

msummary(spec_nox,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on NOx Emissions. \\label{tab:spec-nox-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2) in the main text. Each column is a different specification. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Column (7) replaces country-to-country and top route fixed effects with port-to-port pair fixed effects. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_nox.tex"))

add_adjust_box(here(output_dir, "spec_nox.tex"))
adjust_notes_font_size(here(output_dir, "spec_nox.tex"))

# =============================================================================
# 8. SPECIFICATION ANALYSIS - SOX
# =============================================================================

spec_sox_1 <- feols(total_sox ~ attacks_7day_num | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_sox_2 <- feols(total_sox ~ attacks_7day_num + ..wctrl | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_sox_3 <- feols(total_sox ~ attacks_7day_num + ..wctrl | country_pair + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_sox_4 <- feols(total_sox ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_sox_5 <- feols(total_sox ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_sox_6 <- feols(total_sox ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_sox_7 <- feols(total_sox ~ attacks_7day_num + ..wctrl | route_port_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)

spec_sox <- list(spec_sox_1, spec_sox_2, spec_sox_3, spec_sox_4, spec_sox_5, spec_sox_6, spec_sox_7)

msummary(spec_sox,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on SOx Emissions. \\label{tab:spec-sox-table}",
         notes = list("The unit of observation is a voyage. Estimates are from Eq. (2) in the main text. Each column is a different specification. Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5-degree spatial footprint. The sample spans from 2012 to 2023. Controls include average wind speed, the wind-resistance index, and average wave height along the voyage. Column (7) replaces country-to-country and top route fixed effects with port-to-port pair fixed effects. Standard errors are clustered by country-to-country route by year."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_sox.tex"))

add_adjust_box(here(output_dir, "spec_sox.tex"))
adjust_notes_font_size(here(output_dir, "spec_sox.tex"))

# =============================================================================
# 9. BACK OF ENVELOPE CALCULATIONS
# =============================================================================

# Re-estimate the models used for predictions without lean = TRUE
# These models need to store fixed effects information for predictions
m1_co2_pred <- feols(total_co2 ~ attacks_7day_num + ..wctrl
                     | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                     wdb,
                     cluster = ~country_pair ^ year,
                     lean = FALSE,
                     combine.quick = FALSE)

m1_nox_pred <- feols(total_nox ~ attacks_7day_num + ..wctrl
                     | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                     wdb,
                     cluster = ~country_pair ^ year,
                     lean = FALSE,
                     combine.quick = FALSE)

m1_sox_pred <- feols(total_sox ~ attacks_7day_num + ..wctrl
                     | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                     wdb,
                     cluster = ~country_pair ^ year,
                     lean = FALSE,
                     combine.quick = FALSE)

pred_global <- wdb %>%
  select(trip_id, attacks_7day_num, wind_speed, wind_vector, wave_height,
         country_pair, vessel_type, tonnage_decile, hotspot, top_route, month, year)

pred_global <- pred_global %>%
  mutate(
    p_co2 = predict(m1_co2_pred, newdata = pred_global),
    np_co2 = predict(m1_co2_pred, newdata = pred_global %>% mutate(attacks_7day_num = 0)),
    p_nox = predict(m1_nox_pred, newdata = pred_global),
    np_nox = predict(m1_nox_pred, newdata = pred_global %>% mutate(attacks_7day_num = 0)),
    p_sox = predict(m1_sox_pred, newdata = pred_global),
    np_sox = predict(m1_sox_pred, newdata = pred_global %>% mutate(attacks_7day_num = 0))
  )

write_rds(pred_global, here("data", "processed", "emissions_pred_global.rds"))

# =============================================================================
# 10. SUMMARY STATISTICS
# =============================================================================

cat("Emissions regressions completed.\n")
cat("Number of observations:", nrow(wdb), "\n")
cat("Tables saved to:", here(output_dir), "\n")
cat("- emissions.tex: Main emissions table\n")
cat("- spec_co2.tex: CO2 specification table\n")
cat("- spec_nox.tex: NOx specification table\n")
cat("- spec_sox.tex: SOx specification table\n")
