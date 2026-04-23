# Merge cost and emissions predictions into a single dataset

library(here)
library(tidyverse)

# --- Load prediction data ---

pred_cost <- readRDS(here("data", "processed", "cost_pred_global.rds"))
pred_emissions <- readRDS(here("data", "processed", "emissions_pred_global.rds"))

# --- Merge predictions ---

total_pred <- left_join(pred_cost, pred_emissions, by = join_by(trip_id,
                                                                attacks_7day_num,
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

# --- Export ---

write_rds(total_pred, here("data", "processed", "full_pred_global.rds"))
write_csv(total_pred, here("data", "processed", "full_pred_global.csv"))

# --- Summary ---

cat("Predictions merge completed.\n")
cat("Number of observations in merged dataset:", nrow(total_pred), "\n")
cat("Number of variables in merged dataset:", ncol(total_pred), "\n")
cat("Merged data saved to:", here("data", "processed"), "\n")
cat("- full_pred_global.rds: RDS format\n")
