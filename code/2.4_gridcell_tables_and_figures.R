# =============================================================================
# 1. SET UP
# =============================================================================
# Packages
library(here)
library(conleyreg)
library(tidyverse)
library(patchwork)
library(fixest)
library(modelsummary)

# File paths and names
output_dir <- here("results", "figures_and_tables")
reg_table_name <- here(output_dir, "cell_post_regression.tex")
AIS_disab_table_name <- here(output_dir, "AIS_disabling_cell_post_regression.tex")
event_study_figure_name <- here(output_dir, "cell_level_event_study_2x3.png")
supplementary_event_study_figure_name <- here(output_dir, "cell_level_event_study_2x3_by_resolution.png")

# Load models and data
load(file = here("data", "output", "gridcell_models.RData"))
load(file = here("data", "output", "gridcell_spec_models.RData"))
load(file = here("data", "output", "gridcell_AIS_disabling.RData"))

# LOAD PANEL DATA
panel <- function(res) {
  readRDS(here("data", "processed", paste0("ev_panel_", res, ".rds"))) |>
    mutate(time_vessels = time_hours/n_vessels,
           dist_vessels = distance_km/n_vessels)
}


# =============================================================================
# 2. HELPER FUNCTIONS
# =============================================================================

# LaTeX table helper functions
add_adjust_box <- function(file,
                           line_before = "\\begin{adjustbox}{width = .9\\textwidth}",
                           line_after = "\\end{adjustbox}",
                           before = "\\begin{threeparttable}",
                           after = "\\end{threeparttable}") {
  lines <- readLines(file)
  after_line <- grep(after, lines, fixed = TRUE)
  before_line <- grep(before, lines, fixed = TRUE)
  lines <- c(lines[1:(before_line-1)], line_before, lines[(before_line):(after_line)], line_after, lines[(after_line+1):length(lines)])
  writeLines(lines, file)
}

adjust_notes_font_size <- function(file, font_size_command = "\\scriptsize") {
  lines <- readLines(file)
  item_line_index <- grep("\\item", lines, fixed = TRUE)
  if (length(item_line_index) > 0) {
    lines[item_line_index] <- gsub("\\item", paste0("\\item ", font_size_command), lines[item_line_index], fixed = TRUE)
  }
  writeLines(lines, file)
}

# Function to add rows with model observations after each sub-panel
add_rows_with_observations <- function(file, observations_df) {
  lines <- readLines(file)
  
  # Define panel order and their corresponding outcome variables
  panel_order <- c("Panel (A): Total Time (hours)" = "asinh(time_hours)",
                   "Panel (B): Total Distance (km)" = "asinh(distance_km)",
                   "Panel (C): Vessels (#)" = "asinh(n_vessels)",
                   "Panel (D): Voyages (#)" = "asinh(n_trips)",
                   "Panel (E): Time per Vessel (hours / vessel)" = "asinh(time_vessels)",
                   "Panel (F): Distance per Vessel (km / vessel)" = "asinh(dist_vessels)")
  
  # Find the end of each panel by looking for the next panel header or end of table
  panel_headers <- grep("Panel \\([A-F]\\):", lines)
  
  # Add observations row after each panel
  for (i in seq_along(panel_headers)) {
    outcome_var <- panel_order[i]

    # Find the end of this panel (start of next panel or before bottomrule)
    if (i < length(panel_headers)) {
      panel_end <- panel_headers[i + 1] - 1
    } else {
      # For the last panel, find the bottomrule line and insert before it
      bottomrule_line <- grep("\\\\bottomrule", lines)[1]
      panel_end <- bottomrule_line - 1
    }

    # Get the observations for this panel
    panel_obs <- observations_df[observations_df$outcome_var == outcome_var, ]

    # Create the observations row
    obs_values <- panel_obs[, -1] # Remove outcome_var
    # Format each numeric value with commas
    obs_values_formatted <- sapply(obs_values, function(x) format(x, big.mark = ",", scientific = FALSE))
    obs_row <- paste("\\hspace{1em}Observations", paste(obs_values_formatted, collapse = " & "), "\\\\", sep = " & ")

    # Insert the observations row before the end of this panel
    lines <- c(lines[1:panel_end], obs_row, lines[(panel_end + 1):length(lines)])

    # Update panel_headers indices since we added a line
    panel_headers <- grep("Panel \\([A-F]\\):", lines)
  }

  writeLines(lines, file)
}

# =============================================================================
# 4. BUILD LATEX TABLE
# =============================================================================
# Extract models
time <- models |> filter(outcome_var == "asinh(time_hours)", res == "0_5") |> pull(coefficients)
distance <- models |> filter(outcome_var == "asinh(distance_km)", res == "0_5") |> pull(coefficients)
n_vessels <- models |> filter(outcome_var == "asinh(n_vessels)", res == "0_5") |> pull(coefficients)
n_trips <- models |> filter(outcome_var == "asinh(n_trips)", res == "0_5") |> pull(coefficients)
time_per_vessel <- models |> filter(outcome_var == "asinh(time_vessels)", res == "0_5") |> pull(coefficients)
distance_per_vessel <- models |> filter(outcome_var == "asinh(dist_vessels)", res == "0_5") |> pull(coefficients)

# Create observations data frame for the function (keep outcome_var for matching)
observations_df <- models |>
  filter(res == "0_5") |> 
  select(outcome_var, hotspot, n) |>
  pivot_wider(names_from = hotspot, values_from = n)

# Create LaTeX table
msummary(models = list("Panel (A): Occupancy Time (hours)" = time,
                       "Panel (B): Distance Traveled (km)" = distance,
                       "Panel (C): Transit (# Vessels)" = n_vessels,
                       "Panel (D): Transit (# Voyages)" = n_trips,
                       "Panel (E): Occupancy per Vessel (hours / vessel)" = time_per_vessel,
                       "Panel (F): Distance Traveled per Vessel (km / vessel)" = distance_per_vessel),
         shape = "rbind",
         coef_rename = c("Post-Attack"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.3f",
         title = "Effect of Pirate Attacks on Grid Cell Shipping Activity. \\label{tab:cell-post-regression}",
         notes = list("The unit of observation is a grid cell-day. Estimates are from Eq. (1) in the main text. Each panel examines a different shipping activity measure and each column represents a different geographic region. Post-Attack is a binary indicator equal to 1 for days on or after a pirate attack in the grid cell. The analysis uses a 7-day window around attacks to identify pre- and post-attack periods. The sample spans from 2012 to 2023. All regressions include grid cell, year-by-month, and day-of-week fixed effects. Standard errors are Conley standard errors (50km cutoff)."),
         threeparttable = TRUE,
         escape = FALSE,
         output = reg_table_name)

# Apply formatting functions
adjust_notes_font_size(reg_table_name)
add_rows_with_observations(reg_table_name, observations_df)

# =============================================================================
# 5. BUILD EVENT-STUDY PLOTS
# =============================================================================

# Function to create event study plot for a given outcome variable
create_event_study_plot <- function(outcome_var, res, title, y_label = "Estimate ± (std.error & 95% CI)") {
  # Run regression for the specific outcome
  model <- conleyreg(
    # as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id + year^month + day_of_week + asam_subregion")),
    as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id + date + id^month")),
    unit = "id",
    time = "date",
    lat = "lat_bin",
    lon = "lon_bin",
    data = panel(res) |> 
      drop_na(outcome_var) |> 
      add_count(date, name = "n_int") |>
      filter(n_int > 1) |>
      select(-n_int) |>
      add_count(id, month, name = "n_int2") |>
      filter(n_int2 > 1) |>
      select(-n_int2),
    dist_cutoff = 50,
    lag_cutoff = Inf
  )

  # Extract coefficients
  coeff_data <- broom::tidy(model) %>%
    filter(str_detect(term, "relative_time::")) %>%
    mutate(
      relative_time = as.integer(str_extract(term, "-?\\d+")),
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error
    ) %>%
    arrange(relative_time)

  # Add the reference point (-1)
  ref_row <- tibble(
    term = "relative_time::-1",
    estimate = 0,
    std.error = 0,
    statistic = NA,
    p.value = NA,
    relative_time = -1,
    ci_low = 0,
    ci_high = 0
  )

  coeff_data <- bind_rows(coeff_data, ref_row) %>%
    arrange(relative_time)

  # Create plot
  ggplot(coeff_data, aes(x = relative_time, y = estimate)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_linerange(aes(ymin = ci_low,
                       ymax = ci_high),
                   linewidth = 0.5, color = "black") +
    geom_linerange(aes(ymin = estimate - std.error,
                       ymax = estimate + std.error),
                   linewidth = 1.5, color = "cadetblue") +
    geom_point(size = 3, color = "cadetblue") +
    labs(
      title = title,
      x = "Days Relative to Attack",
      y = y_label
    ) +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = seq(-7, 7, 2))
}

# Create all four event study plots
p1 <- create_event_study_plot("time_hours",
                              res = "0_5",
                              title = "A) Occupancy Time (hours)")
p2 <- create_event_study_plot("distance_km",
                              res = "0_5",
                              title = "B) Distance Traveled (km)")
p3 <- create_event_study_plot("n_vessels",
                              res = "0_5",
                              title = "C) Transit (# Vessels)")
p4 <- create_event_study_plot("n_trips",
                              res = "0_5",
                              title = "D) Transit (# Trips)")
p5 <- create_event_study_plot("time_vessels",
                              res = "0_5",
                              title = "E) Occupancy per Vessel (hours / vessel)")
p6 <- create_event_study_plot("dist_vessels",
                              res = "0_5",
                              title = "F) Distance Traveled per Vessel (km / vessel)")

# Combine plots into 2x3 grid
combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# Save the combined plot
ggsave(
  filename = event_study_figure_name,
  plot = combined_plot,
  width = 9,
  height = 9,
  dpi = 300
)

# =============================================================================
# 7. BUILD EVENT-STUDY PLOTS FOR DIFFERENT RESOLUTIONS
# =============================================================================

## Supplementary materials plot
res_plot <- models |> 
  select(1:3, coefficients) |> 
  mutate(coefficients = map(coefficients, broom::tidy, conf.int = T)) |> 
  unnest(coefficients) |> 
  mutate(outcome_var = case_when(outcome_var == "asinh(time_hours)" ~ "Occupancy Time (hours)",
                                 outcome_var == "asinh(distance_km)" ~ "Distance Traveled (km)",
                                 outcome_var == "asinh(n_vessels)" ~ "Transit (# vessels)",
                                 outcome_var == "asinh(n_trips)" ~ "Transit (# trips)",
                                 outcome_var == "asinh(time_vessels)" ~ "Occupancy per Vessel (hours / vessel)",
                                 outcome_var == "asinh(dist_vessels)" ~ "Distance Traveled per Vessel (km / vessel)"),
         outcome_var = fct_relevel(outcome_var,
                                   "Occupancy Time (hours)",
                                   "Distance Traveled (km)",
                                   "Transit (# vessels)",
                                   "Transit (# trips)",
                                   "Occupancy per Vessel (hours / vessel)",
                                   "Distance Traveled per Vessel (km / vessel)"),
         hotspot = ifelse(hotspot == "S.E. Asia", "Southeast Asia", hotspot),
         hotspot = fct_relevel(hotspot,
                               "Global",
                          "G. of Aden",
                          "G. of Guinea",
                          "Southeast Asia"),
    res = paste0(str_replace(res, "_", "."), "°")) |> 
  ggplot(aes(x = res, y = estimate)) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 color = "black", 
                 linewidth = 0.5) + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error),
                 color = "black",
                 linewidth = 1.5) + 
  geom_point(shape = 21, fill = "cadetblue", size = 4) + 
  facet_wrap(outcome_var ~ hotspot, scales = "free_y", ncol = 4) +
  theme_minimal(base_size = 10) +
  labs(x = "Resolution",
       y = "Estimate ± (std. error & 95% CI)")
ggsave(plot = res_plot,
       filename = here("results/figures_and_tables/cell_post_regression_by_resolution.png"),
       width = 11,
       height = 10)


## Supplementary materials event studies

create_multi_event_study_plot <- function(outcome_var, title, y_label = "Estimate ± (std.error & 95% CI)") {
  # Run regression for the specific outcome
  model_0_1 <- conleyreg(
    as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id + year^month + day_of_week + asam_subregion")),
    unit = "id",
    time = "date",
    lat = "lat_bin",
    lon = "lon_bin",
    data = panel("0_1"),
    dist_cutoff = 50,
    lag_cutoff = Inf
  )
  
  model_0_5 <- conleyreg(
    as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id + year^month + day_of_week + asam_subregion")),
    unit = "id",
    time = "date",
    lat = "lat_bin",
    lon = "lon_bin",
    data = panel("0_5"),
    dist_cutoff = 50,
    lag_cutoff = Inf
  )
  
  model_1 <- conleyreg(
    as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id + year^month + day_of_week + asam_subregion")),
    unit = "id",
    time = "date",
    lat = "lat_bin",
    lon = "lon_bin",
    data = panel(+1),
    dist_cutoff = 50,
    lag_cutoff = Inf
  )
  
  models <- list("0.1°" = model_0_1,
                 "0.5°" = model_0_5,
                 "1°" = model_1)
  
  # Extract coefficients
  coeff_data <- map_dfr(models, broom::tidy, .id = "res") |> 
    filter(str_detect(term, "relative_time::")) %>%
    mutate(
      relative_time = as.integer(str_extract(term, "-?\\d+")),
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error
    ) %>%
    arrange(relative_time)
  
  # Add the reference point (-1)
  ref_row <- tibble(
    term = "relative_time::-1",
    res = c("0.1°", "0.5°", "1°"),
    estimate = 0,
    std.error = 0,
    statistic = NA,
    p.value = NA,
    relative_time = -1,
    ci_low = 0,
    ci_high = 0
  )
  
  coeff_data <- bind_rows(coeff_data, ref_row) %>%
    arrange(relative_time)
  
  # Create plot
  ggplot(coeff_data, aes(x = relative_time, y = estimate, color = res, group = res)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_linerange(aes(ymin = ci_low,
                       ymax = ci_high),
                   linewidth = 0.3, color = "black",
                   position = position_dodge(width = 1)) +
    geom_linerange(aes(ymin = estimate - std.error,
                       ymax = estimate + std.error),
                   linewidth = 1,
                   position = position_dodge(width = 1)) +
    geom_point(size = 2, position = position_dodge(width = 1)) +
    scale_color_manual(values = c("steelblue","cadetblue", "lightblue")) +
    labs(
      title = title,
      x = "Days Relative to Attack",
      y = y_label,
      color = "Resolution"
    ) +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = seq(-7, 7, 2))
}


# Create all four event study plots
sp1 <- create_multi_event_study_plot("time_hours",
                              title = "A) Occupancy Time (hours)")
sp2 <- create_multi_event_study_plot("distance_km",
                              title = "B) Distance Traveled (km)")
sp3 <- create_multi_event_study_plot("n_vessels",
                              title = "C) Transit (# Vessels)")
sp4 <- create_multi_event_study_plot("n_trips",
                              title = "D) Transit (# Trips)")
sp5 <- create_multi_event_study_plot("time_vessels",
                              title = "E) Occupancy per Vessel (hours / vessel)")
sp6 <- create_multi_event_study_plot("dist_vessels",
                              title = "F) Distance Traveled per Vessel (km / vessel)")

# Combine plots into 2x3 grid
s_combined_plot <- (sp1 + sp2) / (sp3 + sp4) / (sp5 + sp6) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))


# Save the combined supplementary plot
ggsave(
  filename = supplementary_event_study_figure_name,
  plot = s_combined_plot,
  width = 10,
  height = 10,
  dpi = 300
)

# =============================================================================
# 8. BUILD TABLE FOR AIS DISABLING EVENT
# =============================================================================

AIS_disab <- create_event_study_plot("n_ais_disabling",
                                     res = "0_5",
                                     title = "# AIS disabling events")
AIS_observations_df <- AIS_disab_models |>
  filter(res == "0_5") |> 
  select(outcome_var, hotspot, n) |>
  pivot_wider(names_from = hotspot, values_from = n)

modelsummary(AIS_disab_models$coefficients,
             coef_rename = c("Post-Attack"),
             gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
             stars = c('*' = .1, '**' = .05, '***' = .01),
             fmt = "%.3f",
             title = "Effect of Pirate Encounters on AIS Disabling Events. \\label{tab:ais-disabling}",
             notes = list("The unit of observation is a grid cell-day. Estimates are from Eq. (1) in the main text. Each column represents a different geographic region. The Southeast Asia hotspot is excluded because there were no disabling events detected within attacked pixels. Post-Attack is a binary indicator equal to 1 for days on or after a pirate attack in the grid cell. The analysis uses a 7-day window around attacks to identify pre- and post-attack periods. The sample spans from 2012 to 2023. All regressions include grid cell, year-by-month, and day-of-week fixed effects. Standard errors are Conley standard errors (50km cutoff)."),
             threeparttable = TRUE,
             escape = FALSE,
             output = AIS_disab_table_name)

# Add other rows
adjust_notes_font_size(AIS_disab_table_name)
add_rows_with_observations(AIS_disab_table_name, AIS_observations_df)

