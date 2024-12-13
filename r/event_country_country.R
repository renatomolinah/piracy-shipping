# Load packages ----------------------------------------------------------------
install.packages("pacman")
pacman::p_load(
  DIDmultiplegtDYN,
  tidyverse,
  here,
  data.table
)


setDTthreads(threads = 15)

# Load data --------------------------------------------------------------------
panel <- readRDS(file = here("processed_data/daily_attacks_and_activity_for_event_study.rds")) |> 
  mutate(n_trips = asinh(n_trips),
         id1 = paste(from_country, from_port, to_country, to_port, sep = "_"),
         id = paste(from_country, to_country, sep = "_")) |> 
  slice_sample(by = id1, prop = 0.2) %>% 
  select(n_trips, id, date, days_with_attack)
#id = paste(from_country, from_port, to_country, to_port, sep = "_"))

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------
a <- did_multiplegt_dyn(df = panel,
                        effects = 5,
                        placebo = 5,
                        outcome = "n_trips",
                        group = "id",
                        time = "date",
                        treatment = "days_with_attack")

saveRDS(a, "a_country_to_country.rds")

## VISUALIZE ###################################################################
ggplot2::update_geom_defaults(geom = "line",
                              new = list(color = "black"))
# X ----------------------------------------------------------------------------
a$plot +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_linedraw()
