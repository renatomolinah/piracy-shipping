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
  mutate(hotspot = case_when(hotspot == "Guinea" ~ "Gulf of Guinea",
                             hotspot == "Asia" ~ "South East Asia",
                             hotspot == "Aden" ~ "Gulf of Aden",
                             T ~ hotspot),
         hotspot = fct_relevel(hotspot, "Global", "Gulf of Aden", "South East Asia", "Gulf of Guinea")) %>% 
  mutate(fuel = fuel / 1e3,
         labor = labor / 1e3,
         total = total / 1e3,
         nox = nox / 1e3,
         sox = sox / 1e3)

## VISUALIZE ###################################################################

# X ----------------------------------------------------------------------------
dsummary(((`Fuel (Million USD)` = fuel) + (`Labor (Million USD)` = labor) + (`Total  (Million USD)` = total)) * (`Hotspot` = hotspot) ~ sum * year, data = pred_info_local,
         fmt = 0,
         output = here("tables", "counterfactual_costs.tex"),
         title = "Total costs of piracy to the shipping industry.\\label{tab:agg.cost}")

# X ----------------------------------------------------------------------------
dsummary(((`CO2 (Metric tones)` = co2) + (`NOx (Metric tones)` = nox) + (`SOx  (Metric tones)` = sox)) * (`Hotspot` = hotspot) ~ sum * year, data = pred_info_local,
         fmt = 0, 
         output = here("tables", "counterfactual_emissions.tex"),
         title = "Total additional emission of air pollutants due to piracy.\\label{tab:emissions}")
