# =============================================================================
# COST REGRESSIONS ANALYSIS
# =============================================================================
# This script performs regression analysis on shipping costs (fuel, labor, total)
# examining how pirate attacks affect shipping costs across different regions.
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
    labor_cost = crew * time * 5.55/1000,
    fuel_use = total_fuel_consumption_mt_inst,
    fuel_cost = total_fuel_cost_usd_inst/1000,
    total_cost = fuel_cost + labor_cost
  ) |>
  mutate(attacks_7day_num = number_previous_attacks_7_days_5_degrees)

# =============================================================================
# 2. SET UP FORMULAS AND CONTROLS
# =============================================================================

setFixest_fml(..wctrl = ~ wind_speed + wind_vector + wave_height)

setFixest_dict(c(
  fuel_cost = "Fuel Cost (TUSD)",
  labor_cost = "Labor Cost (TUSD)",
  total_cost = "Total Cost (TUSD)",
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
# 4. MAIN COST REGRESSIONS
# =============================================================================

# Fuel cost regressions
m1_fuel <- feols(fuel_cost ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb,
                 cluster = ~country_pair ^ year,
                 lean = T)

m2_fuel <- feols(fuel_cost ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(aden == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m3_fuel <- feols(fuel_cost ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(guinea == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

m4_fuel <- feols(fuel_cost ~ attacks_7day_num + ..wctrl
                 | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                 wdb %>% filter(asia == 1),
                 cluster = ~country_pair ^ year,
                 lean = T)

# Labor cost regressions
m1_labor <- feols(labor_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb,
                  cluster = ~country_pair ^ year,
                  lean = T)

m2_labor <- feols(labor_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(aden == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m3_labor <- feols(labor_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(guinea == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m4_labor <- feols(labor_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(asia == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

# Total cost regressions
m1_total <- feols(total_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb,
                  cluster = ~country_pair ^ year,
                  lean = T)

m2_total <- feols(total_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(aden == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m3_total <- feols(total_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(guinea == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

m4_total <- feols(total_cost ~ attacks_7day_num + ..wctrl
                  | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                  wdb %>% filter(asia == 1),
                  cluster = ~country_pair ^ year,
                  lean = T)

# =============================================================================
# 5. CREATE MAIN COST TABLE
# =============================================================================

regs_fuel <- list(m1_fuel, m2_fuel, m3_fuel, m4_fuel)
regs_labor <- list(m1_labor, m2_labor, m3_labor, m4_labor)
regs_total <- list(m1_total, m2_total, m3_total, m4_total)

rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Observations", as.character(nobs(m1_fuel) %>% format(big.mark = ",")),
  as.character(nobs(m2_fuel) %>% format(big.mark = ",")),
  as.character(nobs(m3_fuel) %>% format(big.mark = ",")),
  as.character(nobs(m4_fuel) %>% format(big.mark = ",")),
  "", "", "", "", "",
  "Hotspot FE",  "X", "$\\bullet$", "$\\bullet$", "$\\bullet$"
)

msummary(list("Panel (A): Fuel Cost (TUSD)" = regs_fuel,
              "Panel (B): Labor Cost (TUSD)" = regs_labor,
              "Panel (C): Total Cost (TUSD)" = regs_total),
         coef_omit = c(-1),
         coef_rename = c("Encounters (7 day)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows,
         title = "Effect of Past Pirate Encounters on Shipping Cost. \\label{tab:cost-table}",
         notes = list("The unit of observation is a voyage. Each panel examines a calculated cost in terms of fuel cost, labor cost, and total cost as the sum of both.
                      All coefficients are in thousands of US\\$. The sample spans from 2012 to 2023.
                      Every column is a different sample: Global is the analysis using the whole sample. G. of Aden, S.E. Asia, and G. of Guinea restrict the sample to vessels passing through one of the hotspots, respectively.
                      Every panel-column combination is a different regression analysis.
                      Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5 degree spatial footprint.
                      Controls include average wind speed along the voyage, the wind-resistance index, and wave height.
                      Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, and a battery of month by year and top port-to-port combination for country-to-country combination dummies."),
         threeparttable = TRUE,
         shape = 'rbind',
         escape = FALSE,
         output = here(output_dir, "cost.tex"))

add_adjust_box(here(output_dir, "cost.tex"))
replace_table_headers(here(output_dir, "cost.tex"), c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia"))
adjust_notes_font_size(here(output_dir, "cost.tex"))

# =============================================================================
# 6. SPECIFICATION ANALYSIS - FUEL COST
# =============================================================================

spec_fuel_1 <- feols(fuel_cost ~ attacks_7day_num | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_fuel_2 <- feols(fuel_cost ~ attacks_7day_num + ..wctrl | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_fuel_3 <- feols(fuel_cost ~ attacks_7day_num + ..wctrl | country_pair + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_fuel_4 <- feols(fuel_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_fuel_5 <- feols(fuel_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_fuel_6 <- feols(fuel_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair ^ year, lean = T)

spec_fuel <- list(spec_fuel_1, spec_fuel_2, spec_fuel_3, spec_fuel_4, spec_fuel_5, spec_fuel_6)

rows_spec <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
  "", "", "", "", "", "", "",
  "Observations", as.character(nobs(spec_fuel_1) %>% format(big.mark = ",")),
  as.character(nobs(spec_fuel_2) %>% format(big.mark = ",")),
  as.character(nobs(spec_fuel_3) %>% format(big.mark = ",")),
  as.character(nobs(spec_fuel_4) %>% format(big.mark = ",")),
  as.character(nobs(spec_fuel_5) %>% format(big.mark = ",")),
  as.character(nobs(spec_fuel_6) %>% format(big.mark = ",")),
  "", "", "", "", "", "", "",
  "Country Combo. FE", "", "", "X", "X", "X", "X",
  "Vessel Type FE",    "", "", "", "X", "X", "X",
  "Vessel Size FE",    "", "", "", "X", "X", "X",
  "Hotspot FE",        "", "", "", "", "X", "X",
  "Top Route FE",      "", "", "", "", "", "X",
  "Month-by-Year FE",  "X", "X", "X", "X", "X", "X"
)

msummary(spec_fuel,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on Fuel Cost. \\label{tab:spec-fuel-table}",
         notes = list("The unit of observation is a voyage.
                      The sample spans from 2012 to 2023.
                      Every column is a different specification.
                      Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5 degree spatial footprint.
                      Controls include average wind speed along the voyage, the wind-resistance index, and wave height.
                      Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, and a battery of month by year and top port-to-port combination for country-to-country combination dummies."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_fuel.tex"))

add_adjust_box(here(output_dir, "spec_fuel.tex"))
adjust_notes_font_size(here(output_dir, "spec_fuel.tex"))

# =============================================================================
# 7. SPECIFICATION ANALYSIS - LABOR COST
# =============================================================================

spec_labor_1 <- feols(labor_cost ~ attacks_7day_num | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_labor_2 <- feols(labor_cost ~ attacks_7day_num + ..wctrl | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_labor_3 <- feols(labor_cost ~ attacks_7day_num + ..wctrl | country_pair + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_labor_4 <- feols(labor_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_labor_5 <- feols(labor_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_labor_6 <- feols(labor_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair ^ year, lean = T)

spec_labor <- list(spec_labor_1, spec_labor_2, spec_labor_3, spec_labor_4, spec_labor_5, spec_labor_6)

msummary(spec_labor,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on Labor Cost. \\label{tab:spec-labor-table}",
         notes = list("The unit of observation is a voyage.
                      The sample spans from 2012 to 2023.
                      Every column is a different specification.
                      Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5 degree spatial footprint.
                      Controls include average wind speed along the voyage, the wind-resistance index, and wave height.
                      Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, and a battery of month by year and top port-to-port combination for country-to-country combination dummies."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_labor.tex"))

add_adjust_box(here(output_dir, "spec_labor.tex"))
adjust_notes_font_size(here(output_dir, "spec_labor.tex"))

# =============================================================================
# 8. SPECIFICATION ANALYSIS - TOTAL COST
# =============================================================================

spec_total_1 <- feols(total_cost ~ attacks_7day_num | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_total_2 <- feols(total_cost ~ attacks_7day_num + ..wctrl | month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_total_3 <- feols(total_cost ~ attacks_7day_num + ..wctrl | country_pair + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_total_4 <- feols(total_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_total_5 <- feols(total_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + month^year, wdb, cluster = ~country_pair ^ year, lean = T)
spec_total_6 <- feols(total_cost ~ attacks_7day_num + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year, wdb, cluster = ~country_pair ^ year, lean = T)

spec_total <- list(spec_total_1, spec_total_2, spec_total_3, spec_total_4, spec_total_5, spec_total_6)

msummary(spec_total,
         coef_rename = c("Encounters (7 day)", "Wind Speed (m/s)", "Wind Resistance Index (m/s)", "Wave Height (m)"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.2f",
         add_rows = rows_spec,
         title = "Effect of Past Pirate Encounters on Total Cost. \\label{tab:spec-total-table}",
         notes = list("The unit of observation is a voyage.
                      The sample spans from 2012 to 2023.
                      Every column is a different specification.
                      Encounters (7 day) is the count of pirate encounters recorded in the projected path of the vessel in the preceding 7 days from the departure date using a 5 degree spatial footprint.
                      Controls include average wind speed along the voyage, the wind-resistance index, and wave height.
                      Fixed effects include country-to-country combination, vessel type, vessel size, hotspot, and a battery of month by year and top port-to-port combination for country-to-country combination dummies."),
         threeparttable = TRUE,
         escape = FALSE,
         output = here(output_dir, "spec_total.tex"))

add_adjust_box(here(output_dir, "spec_total.tex"))
adjust_notes_font_size(here(output_dir, "spec_total.tex"))

# =============================================================================
# 9. BACK OF ENVELOPE CALCULATIONS
# =============================================================================

# Re-estimate the models used for predictions without lean = TRUE
# These models need to store fixed effects information for predictions
m1_fuel_pred <- feols(fuel_cost ~ attacks_7day_num + ..wctrl
                      | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                      wdb,
                      cluster = ~country_pair ^ year,
                      lean = FALSE,
                      combine.quick = FALSE)

m1_labor_pred <- feols(labor_cost ~ attacks_7day_num + ..wctrl
                       | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
                       wdb,
                       cluster = ~country_pair ^ year,
                       lean = FALSE,
                       combine.quick = FALSE)

m1_total_pred <- feols(total_cost ~ attacks_7day_num + ..wctrl
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
    p_fuel = predict(m1_fuel_pred, newdata = pred_global),
    np_fuel = predict(m1_fuel_pred, newdata = pred_global %>% mutate(attacks_7day_num = 0)),
    p_labor = predict(m1_labor_pred, newdata = pred_global),
    np_labor = predict(m1_labor_pred, newdata = pred_global %>% mutate(attacks_7day_num = 0)),
    p_total = predict(m1_total_pred, newdata = pred_global),
    np_total = predict(m1_total_pred, newdata = pred_global %>% mutate(attacks_7day_num = 0))
  )

write_rds(pred_global, here("data", "processed", "cost_pred_global.rds"))

# =============================================================================
# 10. SUMMARY STATISTICS
# =============================================================================

cat("Cost regressions completed.\n")
cat("Number of observations:", nrow(wdb), "\n")
cat("Tables saved to:", here(output_dir), "\n")
cat("- cost.tex: Main cost table\n")
cat("- spec_fuel.tex: Fuel cost specification table\n")
cat("- spec_labor.tex: Labor cost specification table\n")
cat("- spec_total.tex: Total cost specification table\n")

