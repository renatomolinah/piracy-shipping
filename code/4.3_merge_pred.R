# =============================================================================
# MERGE PREDICTIONS ANALYSIS
# =============================================================================
# This script merges prediction data from cost and emissions analyses
# to create a comprehensive dataset for further analysis.
# =============================================================================

library(here)
library(tidyverse)

# =============================================================================
# 1. LOAD PREDICTION DATA
# =============================================================================

# Load cost predictions
pred_cost <- readRDS(here("data", "processed", "cost_pred_global.rds"))

# Load emissions predictions
pred_emissions <- readRDS(here("data", "processed", "emissions_pred_global.rds"))

# =============================================================================
# 2. MERGE PREDICTIONS
# =============================================================================

# Merge cost and emissions predictions
total_pred <- left_join(pred_cost, pred_emissions, by = join_by(trip_id,
                                                                attacks_3mo_num,
                                                                wind_speed,
                                                                wind_vector,
                                                                wave_height,
                                                                country_pair, 
                                                                vessel_type,
                                                                tonnage_decile,
                                                                hotspot, 
                                                                top_route, 
                                                                month, 
                                                                year))

# =============================================================================
# 3. SAVE MERGED DATA
# =============================================================================

# Save merged predictions
write_rds(total_pred, here("data", "processed", "full_pred_global.rds"))

# =============================================================================
# 4. SUMMARY STATISTICS
# =============================================================================

cat("Predictions merge completed.\n")
cat("Number of observations in merged dataset:", nrow(total_pred), "\n")
cat("Number of variables in merged dataset:", ncol(total_pred), "\n")
cat("Merged data saved to:", here("data", "processed"), "\n")
cat("- full_pred_global.rds: RDS format\n")

