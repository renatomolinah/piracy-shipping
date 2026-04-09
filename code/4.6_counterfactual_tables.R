# Generate counterfactual cost and emissions tables by hotspot and year

pacman::p_load(
  here,
  DBI,
  bigrquery,
  kableExtra,
  modelsummary,
  tidyverse
)

source(here("code", "table_helpers.R"))

# --- Setup: BigQuery connection ---

bq_auth("juancarlos@ucsb.edu")

piracy <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "piracy",
  billing = "emlab-gcp",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

# --- Load data ---

pred_info <- tbl(piracy, "full_pred_global_v_20260407") %>%
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

# --- Build tables ---

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

# --- Cost table ---

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

add_threeparttable_note(
  here("results", "figures_and_tables", "counterfactual_costs.tex"),
  "Counterfactual costs are derived from the fully specified global voyage-level model (Eq. (2), 5-degree grid, 7-day window). For each voyage, we predict operational costs under the observed encounter intensity and under a counterfactual of zero encounters, then take the difference. Fuel costs are calculated using vessel-specific engine characteristics and daily bunker fuel prices. Labor costs are based on crew size (estimated from vessel type and tonnage) and standard seafarer wage rates. All costs are deflated to constant 2020 USD using the CPI-U annual average. Values are aggregated annually by hotspot region."
)

# --- Emissions table ---

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
  "Counterfactual emissions are derived from the fully specified global voyage-level model (Eq. (2), 5-degree grid, 7-day window). For each voyage, we predict emissions under the observed encounter intensity and under a counterfactual of zero encounters, then take the difference. CO$_2$ emissions are calculated using a standard linear fuel-to-carbon conversion. NO$_\\text{x}$ and SO$_\\text{x}$ emissions are calculated using engine-type-specific emission factors. Values are aggregated annually by hotspot region."
)
