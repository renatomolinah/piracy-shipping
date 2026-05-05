# Process voyage data from Parquet files and prepare for analysis

# --- Setup ---
rm(list = ls(all.names = TRUE))

library(tidyverse)
library(lubridate)
library(here)
library(arrow)

# --- Load voyage data ---
voyage_data_dir <- here("data", "processed", "voyage_data_5_v_20260420")

if (!dir.exists(voyage_data_dir)) {
  stop("Voyage data directory not found: ", voyage_data_dir)
}

parquet_files <- list.files(
  path = voyage_data_dir,
  pattern = "\\.parquet$",
  full.names = TRUE
)

if (length(parquet_files) == 0) {
  stop("No Parquet files found in directory: ", voyage_data_dir)
}

cat("Found", length(parquet_files), "Parquet files to process\n")

voyage_data <- lapply(parquet_files, function(f) read_parquet(f, mmap = FALSE)) %>%
  bind_rows()

cat("Loaded", nrow(voyage_data), "voyage records\n")

# --- Filter and clean ---
voyage_data <- voyage_data %>%
  filter(
    from_country != "",
    to_country != "",
    design_speed >= 10
  ) %>%
  mutate(
    implied_speed_knots = distance_km / 1.852 / hours,
    speed_consistency_ratio = implied_speed_knots / design_speed
  )

cat("After filtering missing countries:", nrow(voyage_data), "records remaining\n")

# --- Create variables ---
voyage_data <- voyage_data %>%
  mutate(
    month = month(departure_date),
    year = year(departure_date),
    route_port_pair = paste(pmin(from_port, to_port), pmax(from_port, to_port)),
    route_country_pair = paste(pmin(from_country, to_country), pmax(from_country, to_country)),
    speed = distance_km / hours,
    tonnage_decile = ntile(tonnage, 10)
  )

# --- Identify most common port pair per country pair ---
most_common_routes <- voyage_data %>%
  group_by(route_country_pair) %>%
  count(route_port_pair, sort = TRUE) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  rename(most_common_port_pair = route_port_pair) %>%
  select(route_country_pair, most_common_port_pair)

voyage_data <- voyage_data %>%
  left_join(most_common_routes, by = 'route_country_pair') %>%
  mutate(
    top_route = if_else(route_port_pair == most_common_port_pair, 1, 0)
  )

# --- Rename for analysis ---
voyage_data <- voyage_data %>%
  rename(
    time = hours,
    distance = distance_km,
    aden = hotspot_gulf_of_aden,
    guinea = hotspot_gulf_of_guinea,
    asia = hotspot_southeast_asia,
    vessel_type = best_vessel_type,
    country_pair = route_country_pair,
    origin_country = from_country,
    wind_speed = wind_speed_ms,
    wave_height = surface_wave_height_m
  )

# --- Export ---
output_file <- here("data", "processed", "voyages.rds")
write_rds(voyage_data, file = output_file)

cat("Processed voyage data saved to:", output_file, "\n")
cat("Final dataset contains", nrow(voyage_data), "records and", ncol(voyage_data), "variables\n")

cat("\n=== DATASET SUMMARY ===\n")
cat("Date range:", min(voyage_data$departure_date), "to", max(voyage_data$departure_date), "\n")
cat("Number of unique country pairs:", n_distinct(voyage_data$country_pair), "\n")
cat("Number of unique vessel types:", n_distinct(voyage_data$vessel_type), "\n")
cat("Average voyage distance:", round(mean(voyage_data$distance, na.rm = TRUE)), "km\n")
cat("Average voyage duration:", round(mean(voyage_data$time, na.rm = TRUE)), "hours\n")
