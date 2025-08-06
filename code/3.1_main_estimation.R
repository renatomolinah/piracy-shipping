# =============================================================================
# MAIN ESTIMATION ANALYSIS
# =============================================================================
# This script performs the main regression analysis examining how pirate attacks
# affect shipping behavior across different time windows and spatial footprints.
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
  attacks_3mo_num_3 = number_previous_attacks_3_months_3_degrees,
  attacks_6mo_num_3 = number_previous_attacks_6_months_3_degrees,
  attacks_12mo_num_3 = number_previous_attacks_12_months_3_degrees,
  
  # 5-degree spatial footprint
  attacks_3mo_num_5 = number_previous_attacks_3_months_5_degrees,
  attacks_6mo_num_5 = number_previous_attacks_6_months_5_degrees,
  attacks_12mo_num_5 = number_previous_attacks_12_months_5_degrees,
  
  # 7-degree spatial footprint
  attacks_3mo_num_7 = number_previous_attacks_3_months_7_degrees,
  attacks_6mo_num_7 = number_previous_attacks_6_months_7_degrees,
  attacks_12mo_num_7 = number_previous_attacks_12_months_7_degrees
)

# =============================================================================
# 2. SET UP FORMULAS AND CONTROLS
# =============================================================================

# Define weather controls
setFixest_fml(..wctrl = ~ wind_speed + wind_vector)

# =============================================================================
# 3. MAIN REGRESSIONS
# =============================================================================

# Run regressions for all specifications
feature_coefficients <- feols(
  c(distance, time, speed) ~ sw(
    attacks_3mo_num_3, attacks_6mo_num_3, attacks_12mo_num_3,
    attacks_3mo_num_5, attacks_6mo_num_5, attacks_12mo_num_5,
    attacks_3mo_num_7, attacks_6mo_num_7, attacks_12mo_num_7
  ) + ..wctrl | country + vtype + size + hotspot + top_route + month^year,
  lean = TRUE,
  data = wdb,
  fsplit = ~hotspot,
  cluster = ~country ^ year
) 

# Save regression results
saveRDS(feature_coefficients, here("data", "output", "feature_coefficients.rds"))

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
    timing = str_extract(term, "\\d+mo") %>% str_replace_all("mo", " mo"),
    degrees = str_extract(term, "\\d+$") %>% str_c(" degrees"),
    term = str_replace(term, "_num_\\d+$", "")
  ) %>%
  mutate(
    timing = factor(timing, levels = c("3 mo", "6 mo", "12 mo")),
    degrees = factor(degrees, levels = c("3 degrees", "5 degrees", "7 degrees")),
    term = factor(term, levels = c("attacks_3mo", "attacks_6mo", "attacks_12mo")),
    sample = factor(sample, levels = c("Global", "G. of Aden", "G. of Guinea", "Southeast Asia")),
    outcome = factor(outcome, levels = c("Distance (km)", "Time (hr)", "Speed (km/hr)"))
  ) %>%
  arrange(sample, timing, degrees, term)

# Save processed coefficient data
saveRDS(coef_data, here("data", "processed", "feature_coefficients_clean.rds"))

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
    title = "Effect of Pirate Attacks on Shipping Behavior",
    subtitle = "By time window, spatial footprint, and region",
    y = "Estimate ± (std.error & 95%CI)",
    x = "Time window before departure",
    caption = "Note: Each point shows the effect of pirate attacks on shipping behavior.\nError bars show standard errors and confidence intervals."
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
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 9, hjust = 0)
  )

# Save plot
ggsave(
  filename = here("figures", "all_features.pdf"),
  plot = feature_plot,
  width = 12, 
  height = 7, 
  units = "in",
  dpi = 300
)

# Also save as PNG for easier viewing
ggsave(
  filename = here("figures", "all_features.png"),
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
cat("Main estimation analysis completed.\n")
cat("Number of specifications:", length(feature_coefficients), "\n")
cat("Number of observations:", nrow(wdb), "\n")
cat("Results saved to:", here("data", "output", "feature_coefficients.rds"), "\n")
cat("Plot saved to:", here("figures", "all_features.pdf"), "\n")


