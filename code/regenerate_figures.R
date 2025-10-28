# =============================================================================
# REGENERATE FIGURES FROM PRE-COMPUTED RESULTS
# This script loads saved regression results and regenerates figures quickly
# WITHOUT running expensive regressions
# =============================================================================

library(here)
library(tidyverse)
library(fixest)
library(broom)
library(viridis)

# =============================================================================
# REGENERATE 3.3 ALWAYS CARGO
# =============================================================================

cat("Loading 3.3 always_cargo results...\n")
feature_coefficients <- readRDS(here("data", "output", "feature_coefficients_always_cargo.rds"))

cat("Processing coefficients...\n")
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
    term = str_replace(term, "_num_\\d+$", "")
  ) %>%
  mutate(
    timing = factor(timing, levels = c("7 day", "15 day", "30 day")),
    degrees = factor(degrees, levels = c("3 degrees", "5 degrees", "7 degrees")),
    term = factor(term, levels = c("attacks_7day", "attacks_15day", "attacks_30day")),
    sample = factor(sample, levels = c("Global", "G. of Aden", "G. of Guinea", "Southeast Asia")),
    outcome = factor(outcome, levels = c("Distance (km)", "Time (hr)", "Speed (km/hr)"))
  ) %>%
  filter(timing != "Other") %>%
  arrange(sample, timing, degrees, term)

cat("Creating plot...\n")
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
    axis.title.x = element_text(size = 20, margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(size = 20, margin = margin(r = 10))
  )

ggsave(
  filename = here("results", "figures_and_tables", "all_features_always_cargo.png"),
  plot = feature_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

cat("✓ Saved: all_features_always_cargo.png\n")

# =============================================================================
# REGENERATE 3.4 ANY CARGO
# =============================================================================

cat("\nLoading 3.4 any_cargo results...\n")
feature_coefficients <- readRDS(here("data", "output", "feature_coefficients_any_cargo.rds"))

cat("Processing coefficients...\n")
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
    term = str_replace(term, "_num_\\d+$", "")
  ) %>%
  mutate(
    timing = factor(timing, levels = c("7 day", "15 day", "30 day")),
    degrees = factor(degrees, levels = c("3 degrees", "5 degrees", "7 degrees")),
    term = factor(term, levels = c("attacks_7day", "attacks_15day", "attacks_30day")),
    sample = factor(sample, levels = c("Global", "G. of Aden", "G. of Guinea", "Southeast Asia")),
    outcome = factor(outcome, levels = c("Distance (km)", "Time (hr)", "Speed (km/hr)"))
  ) %>%
  filter(timing != "Other") %>%
  arrange(sample, timing, degrees, term)

cat("Creating plot...\n")
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
    axis.title.x = element_text(size = 20, margin = margin(t = 10, b = 10)),
    axis.title.y = element_text(size = 20, margin = margin(r = 10))
  )

ggsave(
  filename = here("results", "figures_and_tables", "all_features_any_cargo.png"),
  plot = feature_plot,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300
)

cat("✓ Saved: all_features_any_cargo.png\n")


