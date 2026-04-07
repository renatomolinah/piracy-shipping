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
  kableExtra,
  modelsummary,
  tidyverse
)

add_adjust_box <- function(file,
                           line_before = "\\begin{adjustbox}{width = .9\\textwidth}",
                           line_after = "\\end{adjustbox}",
                           before = "\\begin{threeparttable}",
                           after = "\\end{threeparttable}") {
  lines <- readLines(file)
  after_line <- grep(after, lines, fixed = TRUE)
  before_line <- grep(before, lines, fixed = TRUE)
  lines <- c(lines[1:(before_line-1)], line_before, lines[(before_line):(after_line)], line_after, lines[(after_line+1):length(lines)])
  writeLines(lines, file)
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
pred_info <- tbl(piracy, "full_pred_global_v_20251027") %>%
  mutate(
    fuel = p_fuel - np_fuel,
    labor = p_labor - np_labor,
    total = p_total - np_total,
    co2 = p_co2 - np_co2,
    nox = p_nox - np_nox,
    sox = p_sox - np_sox) %>%
  group_by(year, hotspot) %>%
  summarize_at(.vars = c("fuel", "labor", "total", "co2", "nox", "sox"),
               sum,
               na.rm = T)
## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
global_costs <- pred_info %>%
  select(-hotspot) %>%
  group_by(year) %>%
  summarize_all(sum, na.rm = T) %>%
  mutate(hotspot = "Global") %>%
  collect()

pred_info_local <- pred_info %>%
  filter(!hotspot == "None") %>%
  collect() %>%
  bind_rows(global_costs) %>%
  mutate(year = as.character(year)) %>%
  mutate(hotspot = case_when(hotspot == "Guinea" ~ "G. of Guinea",
                             hotspot == "Asia" ~ "Southeast Asia",
                             hotspot == "Aden" ~ "G. of Aden",
                             T ~ hotspot),
         hotspot = fct_relevel(hotspot, "Global", "G. of Aden", "G. of Guinea", "Southeast Asia")) %>%
  mutate(fuel = fuel / 1e3,
         labor = labor / 1e3,
         total = total / 1e3,
         nox = nox / 1e3,
         sox = sox / 1e3) %>%
  rename(Hotspot = hotspot)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
counterfactual_costs <- dsummary((fuel + labor + total) * Hotspot ~ sum * year, data = pred_info_local,
                                 output = "data.frame") %>%
  mutate_at(3:14, as.numeric) %>%
  mutate_at(3:14, ~scales::comma(., accuracy = 1)) %>%
  select(2:14)

kbl(x = counterfactual_costs,
    booktabs = TRUE,
    label = "agg_cost",
    col.names = c("", 2012:2023),
    caption = "Total Costs of Piracy to the Shipping Industry.",
    linesep = "",
    align = c("l", rep("r", 13)),
    format = "latex") %>%
  kable_styling() %>%
  pack_rows("Fuel (Million USD)", 1, 4) %>%
  pack_rows("Labor (Million USD)", 5, 8) %>%
  pack_rows("Total (Million USD)", 9, 12) %>%
  cat(file = here("results", "figures_and_tables", "counterfactual_costs.tex"))

add_adjust_box(here("results", "figures_and_tables", "counterfactual_costs.tex"),
               before = "\\begin{tabular}",
               after = "\\end{tabular}")

# Add threeparttable + footnote
add_threeparttable_note <- function(file, note_text) {
  lines <- readLines(file, warn = FALSE)
  # Find adjustbox open/close
  adj_open <- grep("\\\\begin\\{adjustbox\\}", lines)
  adj_close <- grep("\\\\end\\{adjustbox\\}", lines)
  # Insert \begin{threeparttable} after adjustbox open
  lines <- c(lines[1:adj_open], "\\begin{threeparttable}", lines[(adj_open+1):length(lines)])
  # Re-find adjustbox close (shifted by 1)
  adj_close <- grep("\\\\end\\{adjustbox\\}", lines)
  # Insert tablenotes + \end{threeparttable} before adjustbox close
  note_lines <- c("\\begin{tablenotes}",
                   paste0("\\item \\scriptsize ", note_text),
                   "\\end{tablenotes}",
                   "\\end{threeparttable}")
  lines <- c(lines[1:(adj_close-1)], note_lines, lines[adj_close:length(lines)])
  writeLines(lines, file)
}

add_threeparttable_note(
  here("results", "figures_and_tables", "counterfactual_costs.tex"),
  "Counterfactual costs are derived from the fully specified global voyage-level model (Eq. (2) in the main text, 5-degree grid, 7-day window). For each voyage, we predict operational costs under the observed encounter intensity and under a counterfactual of zero encounters, then take the difference. Fuel costs are calculated using vessel-specific engine characteristics and daily bunker fuel prices. Labor costs are based on crew size (estimated from vessel type and tonnage) and standard seafarer wage rates. Values are aggregated annually by hotspot region."
)

# X ----------------------------------------------------------------------------
counterfactual_emissions <- dsummary((co2 / 1000 + nox + sox) * Hotspot ~ sum * year, data = pred_info_local,
                                     output = "data.frame") %>%
  mutate_at(3:14, as.numeric) %>%
  mutate_at(3:14, ~scales::comma(., accuracy = 1)) %>%
  select(2:14)

kbl(x = counterfactual_emissions,
    booktabs = TRUE,
    label = "counterfactual_emissions",
    col.names = c("", 2012:2023),
    caption = "Total Emission of Air Pollutants due to Piracy",
    linesep = "",
    align = c("l", rep("r", 13)),
    format = "latex") %>%
  kable_styling() %>%
  pack_rows("CO_2 (Thousand metric tons)", 1, 4) %>%
  pack_rows("NOx (Metric tons)", 5, 8) %>%
  pack_rows("SOx (Metric tons)", 9, 12) %>%
  cat(file = here("results", "figures_and_tables", "counterfactual_emissions.tex"))

add_adjust_box(here("results", "figures_and_tables","counterfactual_emissions.tex"),
               before = "\\begin{tabular}",
               after = "\\end{tabular}")

add_threeparttable_note(
  here("results", "figures_and_tables", "counterfactual_emissions.tex"),
  "Counterfactual emissions are derived from the fully specified global voyage-level model (Eq. (2) in the main text, 5-degree grid, 7-day window). For each voyage, we predict emissions under the observed encounter intensity and under a counterfactual of zero encounters, then take the difference. CO$_2$ emissions are calculated using a standard linear fuel-to-carbon conversion. NO$_\\text{x}$ and SO$_\\text{x}$ emissions are calculated using engine-type-specific emission factors. Values are aggregated annually by hotspot region."
)
