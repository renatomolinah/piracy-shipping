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
  tidyverse
)

# Authenticate using local token -----------------------------------------------
bq_auth("juancarlos@ucsb.edu")

# Establish a connection to BigQuery -------------------------------------------
piracy <- dbConnect(
  bigquery(),
  project = "emlab-gcp",
  dataset = "piracy",
  billing = "mex-fisheries",
  use_legacy_sql = FALSE,
  allowLargeResults = TRUE
)

# Load data --------------------------------------------------------------------
route_all_time_date_has_attack <- tbl(src = piracy,
                                      from = "route_all_time_date_has_attack_50p_threshold_v_20250116")
# route_prior_date_has_attack <- tbl(src = piracy,
#                                    from = "route_prior_date_has_attack_v_20241105")
vessel_activity <- tbl(src = piracy,
                       from = "ungridded_data_v_20240228")
voyage_info <- tbl(src = piracy,
                   from = "voyage_info_v_20240228")

## PROCESSING ##################################################################

# X ----------------------------------------------------------------------------.
relevant_voyage_info <- voyage_info |> 
  select(mmsi, trip_id, from_country, from_port, to_country, to_port)

daily_vessel_activity_by_trip <- vessel_activity |> 
  mutate(date = sql("EXTRACT(DATE FROM timestamp)")) |> 
  group_by(mmsi, trip_id, date) |> 
  summarize(hours = sum(hours, na.rm = TRUE),
            distance_km = sum(distance_km, na.rm = TRUE),
            .groups = "drop") |> 
  left_join(relevant_voyage_info, by = join_by(mmsi, trip_id)) |> 
  group_by(date, from_country, from_port, to_country, to_port) |> 
  summarize(hours = sum(hours, na.rm = TRUE),
            distance_km = sum(distance_km, na.rm = TRUE),
            .groups = "drop")

# X ----------------------------------------------------------------------------.
daily_activity_and_attack_panel <- route_all_time_date_has_attack |> 
  mutate(attack = ifelse(route_has_attack, 1, 0)) |> 
  select(date, from_country, from_port, to_country, to_port, attack) |> 
  left_join(daily_vessel_activity_by_trip, by = join_by(date, from_country, from_port, to_country, to_port)) |> 
  select(date, from_country, from_port, to_country, to_port, attack,
         #mmsi, trip_id,
         hours, distance_km) |> 
  filter(sql("EXTRACT(YEAR FROM date) = 2019")) |>
  replace_na(replace = list(attack = 0,
                            hours = 0,
                            distance_km = 0)) |> 
  ## DELETE THIS FILTER WHEN RUNNING THEW WHOLE THING
  filter(from_country %in% c("EGY", "SAU"),
         to_country %in% c("EGY", "SAU"))
  

local <- collect(daily_activity_and_attack_panel)

saveRDS(local, "daily_activity_and_attack_panel_2019.rds")

## EXPORT ######################################################################

# X ----------------------------------------------------------------------------

test <- local |> 
  filter(date > ymd("2019-12-01"))

ggplot(test, aes(x = date, y = distance_km)) +
  geom_point(pch = ".") +
  geom_smooth(method = "loess") +
  geom_vline(data = test |> filter(attack == 1), aes(xintercept = date)) +
  facet_wrap(~paste(pmin(from_country, to_country), pmax(from_country, to_country)),
             scales = "free_y")













