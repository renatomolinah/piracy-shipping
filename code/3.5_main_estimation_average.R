# =============================================================================
# MAIN ESTIMATION ANALYSIS - AVERAGE ATTACKS
# =============================================================================
# This script performs the main regression analysis examining how average pirate 
# attacks affect shipping behavior using 7/15/30 day windows.
# =============================================================================

library(here)
library(tidyverse)
library(fixest)
library(viridis)
library(broom)

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

# Load the main dataset
wdb <- readRDS(here("data", "processed", "voyages.rds")) %>%
  mutate(
    drop = guinea + aden + asia
  ) %>%
  filter(drop <= 1, best_vessel_type_cargo) %>%
  mutate(
    hotspot = ifelse(guinea == 1, "G. of Guinea",
                     ifelse(aden == 1, "G. of Aden",
                            ifelse(asia == 1, "Southeast Asia", "None")))
  )

# Create attack variables for different time windows and spatial footprints
wdb <- wdb %>% mutate(
  # 3-degree spatial footprint
  attacks_7day_ave_3 = average_route_attacks_last_7_days_3_degrees,
  attacks_15day_ave_3 = average_route_attacks_last_15_days_3_degrees,
  attacks_30day_ave_3 = average_route_attacks_last_1_month_3_degrees,
  attacks_3mo_ave_3 = average_route_attacks_last_3_months_3_degrees,
  attacks_6mo_ave_3 = average_route_attacks_last_6_months_3_degrees,
  attacks_12mo_ave_3 = average_route_attacks_last_12_months_3_degrees,

  # 5-degree spatial footprint
  attacks_7day_ave_5 = average_route_attacks_last_7_days_5_degrees,
  attacks_15day_ave_5 = average_route_attacks_last_15_days_5_degrees,
  attacks_30day_ave_5 = average_route_attacks_last_1_month_5_degrees,
  attacks_3mo_ave_5 = average_route_attacks_last_3_months_5_degrees,
  attacks_6mo_ave_5 = average_route_attacks_last_6_months_5_degrees,
  attacks_12mo_ave_5 = average_route_attacks_last_12_months_5_degrees
)

# =============================================================================
# 2. SET UP FORMULAS AND CONTROLS
# =============================================================================

# Define weather controls (now including wave height)
setFixest_fml(..wctrl = ~ wind_speed + wind_vector + wave_height)

# =============================================================================
# 3. MAIN REGRESSIONS
# =============================================================================

# Run regressions for all specifications (7, 15, and 30 day windows only)
feature_coefficients <- feols(
  c(distance, time, speed) ~ sw(
    # 3-degree footprint
    attacks_7day_ave_3, attacks_15day_ave_3, attacks_30day_ave_3,
    # 5-degree footprint  
    attacks_7day_ave_5, attacks_15day_ave_5, attacks_30day_ave_5
  ) + ..wctrl | country_pair + vessel_type + tonnage_decile + hotspot + top_route + month^year,
  lean = TRUE,
  data = wdb,
  fsplit = ~hotspot,
  cluster = ~country_pair ^ year
)

# Save regression results
saveRDS(feature_coefficients, here("data", "output", "feature_coefficients_average.rds"))

# =============================================================================
# 4. PROCESS COEFFICIENTS FOR VISUALIZATION
# =============================================================================

# Extract and clean coefficient data
coef_data <- map_df(feature_coefficients, broom::tidy, .id = "model", conf.int = TRUE) %>%
  filter(str_detect(term, "^attacks_")) %>%
  mutate(
    sample = str_extract(model, "(?<=sample: ).+?(?=; lhs)"),
    sample = ifelse(sample == "Full sample", "Global", sample)
  ) %>%
  filter(sample != "None") %>%
  mutate(
    outcome = str_extract(model, "lhs: (\\w+)"),
    outcome = case_when(
      str_detect(outcome, "distance") ~ "Distance (km)",
      str_detect(outcome, "time") ~ "Time (hr)",
      str_detect(outcome, "speed") ~ "Speed (km/hr)",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    timing = case_when(
      str_detect(term, "7day") ~ "7 day",
      str_detect(term, "15day") ~ "15 day",
      str_detect(term, "30day") ~ "30 day",
      TRUE ~ "Other"
    ),
    degrees = str_extract(term, "\\d+$") %>% str_c(" degrees"),
    term = str_replace(term, "_ave_\\d+$", "")
  ) %>%
  mutate(
    timing = factor(timing, levels = c("7 day", "15 day", "30 day")),
    degrees = factor(degrees, levels = c("3 degrees", "5 degrees")),
    term = factor(term, levels = c("attacks_7day", "attacks_15day", "attacks_30day")),
    sample = factor(sample, levels = c("Global", "G. of Aden", "G. of Guinea", "Southeast Asia")),
    outcome = factor(outcome, levels = c("Distance (km)", "Time (hr)", "Speed (km/hr)"))
  ) %>%
  filter(timing != "Other") %>%
  arrange(sample, timing, degrees, term)

# Save processed coefficient data
saveRDS(coef_data, here("data", "processed", "feature_coefficients_average_clean.rds"))

# =============================================================================
# 5. CREATE VISUALIZATION
# =============================================================================

# Create comprehensive feature specification plot
feature_plot <- ggplot(coef_data, aes(x = timing, y = estimate, color = degrees, group = degrees)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.5),
    color = "black",
    linewidth = 0.2
  ) +
  geom_pointrange(
    aes(ymin = estimate - std.error, ymax = estimate + std.error),
    position = position_dodge(width = 0.5),
    size = 0.5,
    linewidth = 1
  ) +
  facet_wrap(
    outcome ~ sample,
    scales = "free_y",
    strip.position = "top",
    nrow = length(unique(coef_data$outcome))
  ) +
  scale_color_brewer(palette = "Set1", name = "Grid Footprint:") +
  labs(
    y = "Estimate ± (std.error & 95%CI)",
    x = "Time window before departure"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.background = element_rect(colour = "black", fill = "white"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 10, margin = margin(t = 0, b = 1)),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12, margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10))
  )

# Save plot
ggsave(
  filename = here("results", "figures_and_tables", "all_features_average.png"),
  plot = feature_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

# =============================================================================
# 6. SUMMARY STATISTICS
# =============================================================================

# Print summary of results
cat("Main estimation analysis (average attacks) completed.\n")
cat("Number of specifications:", length(feature_coefficients), "\n")
cat("Number of observations:", nrow(wdb), "\n")
cat("Results saved to:", here("data", "output", "feature_coefficients_average.rds"), "\n")
cat("Plot saved to:", here("results", "figures_and_tables", "all_features_average.png"), "\n")


