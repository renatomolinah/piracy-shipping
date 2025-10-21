# =============================================================================
# ADD 15-DAY ATTACK COUNTS TO VOYAGE DATA
# =============================================================================
# Fast, efficient approach using country-to-country routes
# =============================================================================

library(here)
library(tidyverse)
library(lubridate)
library(data.table)

# =============================================================================
# 1. LOAD VOYAGE DATA (TEST WITH 1M SAMPLE)
# =============================================================================

cat("Loading voyage data...\n")
full_voyage_data <- readRDS(here("data", "processed", "voyages.rds"))

# Test with 1 million records
set.seed(123)
voyage_data <- full_voyage_data %>%
  sample_n(1000000)

cat("Loaded", nrow(voyage_data), "voyage records (1M sample for testing)\n")
cat("Full dataset has", nrow(full_voyage_data), "records\n")

# =============================================================================
# 2. PREPARE DATA WITH COUNTRY-TO-COUNTRY ROUTES
# =============================================================================

cat("Preparing data with country-to-country routes...\n")

# Create route key (country-to-country, alphabetically ordered)
voyage_data <- voyage_data %>%
  mutate(
    route_key = paste(
      pmin(origin_country, to_country), 
      pmax(origin_country, to_country)
    )
  )

# Convert to data.table for speed
dt <- as.data.table(voyage_data)

# =============================================================================
# 3. INFER ATTACKS FROM EXISTING COUNTS
# =============================================================================

cat("Inferring attacks from existing attack counts...\n")

# Use existing attack count columns to infer when attacks occurred
inferred_attacks <- dt[
  , .(
    trip_id,
    departure_date,
    route_key,
    attacks_3mo = number_previous_attacks_3_months_5_degrees,
    attacks_6mo = number_previous_attacks_6_months_5_degrees,
    attacks_12mo = number_previous_attacks_12_months_5_degrees
  )
][
  order(route_key, departure_date)
][
  , `:=`(
    # Calculate changes from previous voyage on same route
    delta_3mo = attacks_3mo - shift(attacks_3mo, type = "lag", fill = 0),
    delta_6mo = attacks_6mo - shift(attacks_6mo, type = "lag", fill = 0),
    delta_12mo = attacks_12mo - shift(attacks_12mo, type = "lag", fill = 0)
  ), 
  by = route_key
][
  # Keep only voyages where attack count increased (indicating an attack occurred)
  delta_3mo > 0 | delta_6mo > 0 | delta_12mo > 0
][
  , .(route_key, attack_date = departure_date, delta_3mo, delta_6mo, delta_12mo)
]

cat("Inferred", nrow(inferred_attacks), "attacks across", n_distinct(inferred_attacks$route_key), "routes\n")

# =============================================================================
# 4. CREATE ATTACK LOOKUP SYSTEM
# =============================================================================

cat("Creating attack lookup system...\n")

# Create lookup: route -> list of attack dates
attack_lookup <- inferred_attacks[
  , .(attack_dates = list(attack_date)), 
  by = route_key
]

# Convert to named list for fast lookup
attack_lookup_list <- setNames(attack_lookup$attack_dates, attack_lookup$route_key)

cat("Created lookup for", length(attack_lookup_list), "routes\n")

# =============================================================================
# 5. CALCULATE 15-DAY ATTACK COUNTS
# =============================================================================

cat("Calculating 15-day attack counts for all voyages...\n")
start_time <- Sys.time()

# Function to count attacks in past N days for a route
count_attacks_past_days <- function(route, date, days, attack_lookup_list) {
  route_attacks <- attack_lookup_list[[route]]
  if (is.null(route_attacks)) return(0)
  
  window_start <- date - days(days)
  window_end <- date - days(1)
  
  return(sum(route_attacks >= window_start & route_attacks <= window_end))
}

# Process in chunks to manage memory
chunk_size <- 1000000  # 1M records per chunk
total_voyages <- nrow(voyage_data)
n_chunks <- ceiling(total_voyages / chunk_size)

cat("Processing", total_voyages, "voyages in", n_chunks, "chunks of", chunk_size, "records each\n")

# Initialize result vector
attacks_15_days <- integer(total_voyages)

# Process each chunk
for (chunk in 1:n_chunks) {
  start_idx <- (chunk - 1) * chunk_size + 1
  end_idx <- min(chunk * chunk_size, total_voyages)
  
  cat("Processing chunk", chunk, "of", n_chunks, "(records", start_idx, "to", end_idx, ")...\n")
  
  # Get chunk data
  chunk_data <- voyage_data[start_idx:end_idx, ]
  
  # Calculate 15-day attack counts for this chunk
  chunk_attacks <- mapply(
    count_attacks_past_days,
    chunk_data$route_key,
    chunk_data$departure_date,
    MoreArgs = list(days = 15, attack_lookup_list = attack_lookup_list)
  )
  
  # Store results
  attacks_15_days[start_idx:end_idx] <- chunk_attacks
  
  # Progress update
  if (chunk %% 10 == 0) {
    cat("Completed", chunk, "chunks. Progress:", round(100 * chunk / n_chunks, 1), "%\n")
  }
}

# =============================================================================
# 6. ADD 15-DAY ATTACK COUNTS TO VOYAGE DATA
# =============================================================================

cat("Adding 15-day attack counts to voyage data...\n")

# Add the new column to the original dataset
final_voyage_data <- voyage_data %>%
  mutate(
    number_previous_attacks_15_days_5_degrees = attacks_15_days
  )

# =============================================================================
# 7. SAVE UPDATED VOYAGE DATA
# =============================================================================

cat("Saving updated voyage data...\n")

# Save the updated voyage data (test version)
output_file <- here("data", "processed", "voyages_with_15day_attacks_test_1M.rds")
write_rds(final_voyage_data, file = output_file)

cat("Updated voyage data saved to:", output_file, "\n")

# =============================================================================
# 8. SUMMARY STATISTICS
# =============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total voyages:", nrow(final_voyage_data), "\n")
cat("Voyages with attacks in past 15 days:", sum(final_voyage_data$number_previous_attacks_15_days_5_degrees > 0), "\n")
cat("Total attacks in past 15 days:", sum(final_voyage_data$number_previous_attacks_15_days_5_degrees), "\n")

# Compare with 3-month counts
cat("\n=== COMPARISON WITH 3-MONTH COUNTS ===\n")
cat("Voyages with attacks in past 3 months:", sum(final_voyage_data$number_previous_attacks_3_months_5_degrees > 0), "\n")
cat("Total attacks in past 3 months:", sum(final_voyage_data$number_previous_attacks_3_months_5_degrees), "\n")

# Summary statistics
summary_stats <- final_voyage_data %>%
  summarise(
    mean_15day = mean(number_previous_attacks_15_days_5_degrees, na.rm = TRUE),
    mean_3month = mean(number_previous_attacks_3_months_5_degrees, na.rm = TRUE),
    max_15day = max(number_previous_attacks_15_days_5_degrees, na.rm = TRUE),
    max_3month = max(number_previous_attacks_3_months_5_degrees, na.rm = TRUE)
  )

cat("\nMean attacks (15 days):", round(summary_stats$mean_15day, 3), "\n")
cat("Mean attacks (3 months):", round(summary_stats$mean_3month, 3), "\n")
cat("Max attacks (15 days):", summary_stats$max_15day, "\n")
cat("Max attacks (3 months):", summary_stats$max_3month, "\n")

# =============================================================================
# 9. PERFORMANCE ESTIMATE FOR FULL DATASET
# =============================================================================

cat("\n=== PERFORMANCE ESTIMATE FOR FULL DATASET ===\n")
cat("Time for 1M records:", round(as.numeric(Sys.time() - start_time, units = "mins"), 1), "minutes\n")

# Estimate time for full dataset
full_time_estimate <- (as.numeric(Sys.time() - start_time, units = "mins") / 1000000) * nrow(full_voyage_data)
cat("Estimated time for full dataset (", nrow(full_voyage_data), " records):", round(full_time_estimate, 1), "minutes\n")

cat("\n15-day attack count addition completed!\n")
