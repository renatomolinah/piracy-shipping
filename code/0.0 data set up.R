# =============================================================================
# PIRACY SHIPPING DATA SETUP SCRIPT
# =============================================================================
# This script processes voyage data from Parquet files and prepares it for analysis
# of piracy impacts on shipping routes.
# =============================================================================

# Clear environment
rm(list = ls(all.names = TRUE))

# Load required libraries
library(tidyverse)    # Data manipulation and visualization (includes dplyr, ggplot2, etc.)
library(lubridate)    # Date/time handling (month, year functions)
library(here)         # Path management
library(arrow)        # Parquet file handling (read_parquet)

# =============================================================================
# 1. LOAD VOYAGE DATA FROM PARQUET FILES
# =============================================================================

# Define the directory containing the Parquet files
voyage_data_dir <- here("piracy-data", "processed", "voyage_data_5_v_20250521")

# Check if directory exists
if (!dir.exists(voyage_data_dir)) {
  stop("Voyage data directory not found: ", voyage_data_dir)
}

# List all Parquet files in the directory
parquet_files <- list.files(
  path = voyage_data_dir, 
  pattern = "\\.parquet$", 
  full.names = TRUE
)

# Check if any files were found
if (length(parquet_files) == 0) {
  stop("No Parquet files found in directory: ", voyage_data_dir)
}

cat("Found", length(parquet_files), "Parquet files to process\n")

# Read and combine all Parquet files
voyage_data <- lapply(parquet_files, read_parquet) %>%
  bind_rows()

cat("Loaded", nrow(voyage_data), "voyage records\n")

# =============================================================================
# 2. INITIAL DATA CLEANING AND FILTERING
# =============================================================================

# Filter out records with missing country information
voyage_data <- voyage_data %>%
  filter(
    from_country != "", 
    to_country != ""
  ) %>%
  # Calculate implied speed and speed consistency check
  mutate(
    implied_speed_knots = distance_km / 1.852 / hours,  # Convert km/h to knots
    speed_consistency_ratio = implied_speed_knots / design_speed  # Check if speeds are reasonable
  )

cat("After filtering missing countries:", nrow(voyage_data), "records remaining\n")

# =============================================================================
# 3. CREATE ADDITIONAL VARIABLES
# =============================================================================

voyage_data <- voyage_data %>%
  mutate(
    # Time variables
    month = month(departure_date),
    year = year(departure_date),
    
    # Route identifiers (sorted to ensure consistency)
    route_port_pair = paste(pmin(from_port, to_port), pmax(from_port, to_port)),
    route_country_pair = paste(pmin(from_country, to_country), pmax(from_country, to_country)),
    
    # Speed and size variables
    speed_kmh = distance_km / hours,
    tonnage_decile = ntile(tonnage, 10)  # Create 10 equal-sized groups by tonnage
  )

# =============================================================================
# 4. IDENTIFY MOST COMMON ROUTES BETWEEN COUNTRIES
# =============================================================================

# Find the most frequent port combination for each country pair
most_common_routes <- voyage_data %>%
  group_by(route_country_pair) %>%
  count(route_port_pair, sort = TRUE) %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  rename(most_common_port_pair = route_port_pair) %>%
  select(route_country_pair, most_common_port_pair)

# Merge back to main dataset and create indicator for most common routes
voyage_data <- voyage_data %>%
  left_join(most_common_routes, by = 'route_country_pair') %>%
  mutate(
    is_most_common_route = if_else(route_port_pair == most_common_port_pair, 1, 0)
  )

# =============================================================================
# 5. RENAME VARIABLES FOR ANALYSIS
# =============================================================================

voyage_data <- voyage_data %>%
  rename(
    # Time and distance
    time_hours = hours,
    distance_km = distance_km,
    
    # Piracy hotspot indicators
    gulf_of_guinea_hotspot = hotspot_gulf_of_guinea,
    southeast_asia_hotspot = hotspot_southeast_asia,
    gulf_of_aden_hotspot = hotspot_gulf_of_aden,
    
    # Vessel characteristics
    vessel_type = best_vessel_type,
    
    # Geographic identifiers
    country_pair = route_country_pair,
    origin_country = from_country,
    
    # Environmental conditions
    wind_speed_ms = wind_speed_ms
  )

# =============================================================================
# 6. SAVE PROCESSED DATA
# =============================================================================

output_file <- here("piracy-data", "processed", "voyages.rds")
write_rds(voyage_data, file = output_file)

cat("Processed voyage data saved to:", output_file, "\n")
cat("Final dataset contains", nrow(voyage_data), "records and", ncol(voyage_data), "variables\n")

# =============================================================================
# 7. SUMMARY STATISTICS
# =============================================================================

cat("\n=== DATASET SUMMARY ===\n")
cat("Date range:", min(voyage_data$departure_date), "to", max(voyage_data$departure_date), "\n")
cat("Number of unique country pairs:", n_distinct(voyage_data$country_pair), "\n")
cat("Number of unique vessel types:", n_distinct(voyage_data$vessel_type), "\n")
cat("Average voyage distance:", round(mean(voyage_data$distance_km, na.rm = TRUE)), "km\n")
cat("Average voyage duration:", round(mean(voyage_data$time_hours, na.rm = TRUE)), "hours\n")

