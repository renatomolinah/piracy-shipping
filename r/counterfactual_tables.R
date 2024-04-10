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
pred_info <- tbl(piracy, "full_pred_global_v_20240328") %>% 
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
  mutate_at(3:11, as.numeric) %>% 
  mutate_at(3:11, ~scales::comma(., accuracy = 1)) %>% 
  select(2:11)

kbl(x = counterfactual_costs,
    booktabs = TRUE,
    label = "tab:agg.cost",
    col.names = c("", 2013:2021),
    caption = "Total Costs of Piracy to the Shipping Industry.",
    linesep = "",
    format = "latex") %>%
  kable_styling() %>%
  pack_rows("Fuel (Million USD)", 1, 4) %>% 
  pack_rows("Labor (Million USD)", 5, 8) %>% 
  pack_rows("Total (Million USD)", 9, 12) %>% 
  cat(file = here("tables", "counterfactual_costs.tex"))

add_adjust_box(here("tables", "counterfactual_costs.tex"),
               before = "\\begin{tabular}",
               after = "\\end{tabular}")


# X ----------------------------------------------------------------------------
counterfactual_emissions <- dsummary((co2 / 1000 + nox + sox) * Hotspot ~ sum * year, data = pred_info_local,
                                     output = "data.frame") %>% 
  mutate_at(3:11, as.numeric) %>% 
  mutate_at(3:11, ~scales::comma(., accuracy = 1)) %>% 
  select(2:11)

kbl(x = counterfactual_emissions,
    booktabs = TRUE,
    label = "tab:counterfactual_emissions",
    col.names = c("", 2013:2021),
    caption = "Total Emission of Air Pollutants due to Piracy",
    linesep = "",
    format = "latex") %>%
  kable_styling() %>%
  pack_rows("CO_2 (Thousand metric tons)", 1, 4) %>% 
  pack_rows("NOx (Metric tons)", 5, 8) %>% 
  pack_rows("SOx (Metric tons)", 9, 12) %>% 
  cat(file = here("tables", "counterfactual_emissions.tex"))

add_adjust_box(here("tables", "counterfactual_emissions.tex"),
               before = "\\begin{tabular}",
               after = "\\end{tabular}")
