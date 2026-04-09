# Build a route-day panel dataset with attack indicators inferred from voyage-level attack count changes

library(here)
library(tidyverse)
library(lubridate)

# --- Load data ---

voyage_data <- readRDS(here("data", "processed", "voyages.rds"))

cat("Loaded", nrow(voyage_data), "voyage records\n")

# --- Define routes ---

routes <- voyage_data %>%
  select(origin_country, to_country) %>%
  distinct() %>%
  mutate(
    route_id = paste(origin_country, to_country),
    route_id_reverse = paste(to_country, origin_country)
  ) %>%
  # Canonical route key: alphabetically ordered country pair
  mutate(
    route_key = paste(
      pmin(origin_country, to_country),
      pmax(origin_country, to_country)
    )
  )

cat("Found", nrow(routes), "unique routes\n")

# --- Prepare voyage data with route identifiers ---

voyage_data_with_routes <- voyage_data %>%
  mutate(
    route_key = paste(
      pmin(origin_country, to_country),
      pmax(origin_country, to_country)
    )
  ) %>%
  mutate(
    attacks_3mo = number_previous_attacks_3_months_5_degrees,
    attacks_6mo = number_previous_attacks_6_months_5_degrees,
    attacks_12mo = number_previous_attacks_12_months_5_degrees
  ) %>%
  select(
    trip_id, departure_date, route_key, origin_country, to_country,
    attacks_3mo, attacks_6mo, attacks_12mo,
    distance, time, speed,
    vessel_type, tonnage_decile,
    guinea, aden, asia
  ) %>%
  arrange(route_key, departure_date)

# --- Infer attacks from count changes ---

# Positive changes in attack counts between consecutive voyages on the same route signal an attack occurred
infer_attacks <- function(route_data, route_key) {
  if (nrow(route_data) < 2) return(tibble())

  route_data %>%
    arrange(departure_date) %>%
    mutate(
      delta_attacks_3mo = attacks_3mo - lag(attacks_3mo, default = 0),
      delta_attacks_6mo = attacks_6mo - lag(attacks_6mo, default = 0),
      delta_attacks_12mo = attacks_12mo - lag(attacks_12mo, default = 0),
      attack_indicator = ifelse(delta_attacks_3mo > 0, 1, 0),
      attack_indicator_any = ifelse(
        delta_attacks_3mo > 0 | delta_attacks_6mo > 0 | delta_attacks_12mo > 0,
        1, 0
      ),
      route_key = route_key
    ) %>%
    filter(attack_indicator == 1) %>%
    select(route_key, departure_date, attack_indicator, attack_indicator_any,
           delta_attacks_3mo, delta_attacks_6mo, delta_attacks_12mo)
}

inferred_attacks <- voyage_data_with_routes %>%
  group_by(route_key) %>%
  arrange(departure_date) %>%
  mutate(
    delta_attacks_3mo = attacks_3mo - lag(attacks_3mo, default = 0),
    delta_attacks_6mo = attacks_6mo - lag(attacks_6mo, default = 0),
    delta_attacks_12mo = attacks_12mo - lag(attacks_12mo, default = 0),
    attack_indicator = ifelse(delta_attacks_3mo > 0, 1, 0),
    attack_indicator_any = ifelse(
      delta_attacks_3mo > 0 | delta_attacks_6mo > 0 | delta_attacks_12mo > 0,
      1, 0
    )
  ) %>%
  filter(attack_indicator == 1) %>%
  select(route_key, departure_date, attack_indicator, attack_indicator_any,
         delta_attacks_3mo, delta_attacks_6mo, delta_attacks_12mo) %>%
  ungroup()

cat("Inferred", nrow(inferred_attacks), "attacks across", n_distinct(inferred_attacks$route_key), "routes\n")

# --- Create date grid ---

date_range <- voyage_data %>%
  summarise(
    start_date = min(departure_date),
    end_date = max(departure_date)
  )

cat("Study period:", as.character(date_range$start_date), "to", as.character(date_range$end_date), "\n")

all_dates <- tibble(
  date = seq(from = date_range$start_date, to = date_range$end_date, by = "day")
)

# --- Build route-day panel ---

route_day_panel <- routes %>%
  select(route_key, origin_country, to_country) %>%
  crossing(all_dates) %>%
  arrange(route_key, date)

cat("Created", nrow(route_day_panel), "route-day combinations\n")

# --- Add attack indicators ---

route_day_panel_with_attacks <- route_day_panel %>%
  left_join(
    inferred_attacks %>%
      select(route_key, departure_date, attack_indicator, attack_indicator_any,
             delta_attacks_3mo, delta_attacks_6mo, delta_attacks_12mo),
    by = c("route_key", "date" = "departure_date")
  ) %>%
  mutate(
    attack_indicator = ifelse(is.na(attack_indicator), 0, attack_indicator),
    attack_indicator_any = ifelse(is.na(attack_indicator_any), 0, attack_indicator_any),
    delta_attacks_3mo = ifelse(is.na(delta_attacks_3mo), 0, delta_attacks_3mo),
    delta_attacks_6mo = ifelse(is.na(delta_attacks_6mo), 0, delta_attacks_6mo),
    delta_attacks_12mo = ifelse(is.na(delta_attacks_12mo), 0, delta_attacks_12mo)
  )

# --- Add shipping activity metrics ---

daily_activity <- voyage_data_with_routes %>%
  group_by(route_key, departure_date) %>%
  summarise(
    n_voyages = n(),
    n_vessels = n_distinct(trip_id),
    total_distance = sum(distance, na.rm = TRUE),
    total_time = sum(time, na.rm = TRUE),
    avg_speed = mean(speed, na.rm = TRUE),
    total_tonnage = sum(tonnage_decile, na.rm = TRUE),
    hotspot_guinea = max(guinea, na.rm = TRUE),
    hotspot_aden = max(aden, na.rm = TRUE),
    hotspot_asia = max(asia, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = departure_date)

final_panel <- route_day_panel_with_attacks %>%
  left_join(daily_activity, by = c("route_key", "date")) %>%
  mutate(
    n_voyages = ifelse(is.na(n_voyages), 0, n_voyages),
    n_vessels = ifelse(is.na(n_vessels), 0, n_vessels),
    total_distance = ifelse(is.na(total_distance), 0, total_distance),
    total_time = ifelse(is.na(total_time), 0, total_time),
    avg_speed = ifelse(is.na(avg_speed), 0, avg_speed),
    total_tonnage = ifelse(is.na(total_tonnage), 0, total_tonnage),
    hotspot_guinea = ifelse(is.na(hotspot_guinea), 0, hotspot_guinea),
    hotspot_aden = ifelse(is.na(hotspot_aden), 0, hotspot_aden),
    hotspot_asia = ifelse(is.na(hotspot_asia), 0, hotspot_asia)
  )

# --- Add time and region variables ---

final_panel <- final_panel %>%
  mutate(
    year = year(date),
    month = month(date),
    week = week(date),
    day_of_week = wday(date),
    quarter = quarter(date)
  )

final_panel <- final_panel %>%
  mutate(
    hotspot_region = case_when(
      hotspot_guinea == 1 ~ "Gulf of Guinea",
      hotspot_aden == 1 ~ "Gulf of Aden",
      hotspot_asia == 1 ~ "Southeast Asia",
      TRUE ~ "Other"
    )
  )

# --- Export ---

output_file <- here("data", "processed", "route_day_panel.rds")
write_rds(final_panel, file = output_file)

cat("Route-day panel saved to:", output_file, "\n")
cat("Final panel contains", nrow(final_panel), "observations\n")
cat("Number of unique routes:", n_distinct(final_panel$route_key), "\n")
cat("Number of days with attacks:", sum(final_panel$attack_indicator), "\n")
cat("Number of routes with attacks:", n_distinct(final_panel$route_key[final_panel$attack_indicator == 1]), "\n")

# --- Summary ---

cat("\n=== PANEL SUMMARY ===\n")
cat("Date range:", min(final_panel$date), "to", max(final_panel$date), "\n")
cat("Total route-day combinations:", nrow(final_panel), "\n")
cat("Routes with activity:", sum(final_panel$n_voyages > 0), "\n")
cat("Days with attacks:", sum(final_panel$attack_indicator), "\n")
cat("Routes with attacks:", n_distinct(final_panel$route_key[final_panel$attack_indicator == 1]), "\n")

hotspot_summary <- final_panel %>%
  group_by(hotspot_region) %>%
  summarise(
    routes = n_distinct(route_key),
    days_with_attacks = sum(attack_indicator),
    total_voyages = sum(n_voyages),
    .groups = "drop"
  )

cat("\n=== BY HOTSPOT REGION ===\n")
print(hotspot_summary)

cat("\nRoute-day panel creation completed!\n")
