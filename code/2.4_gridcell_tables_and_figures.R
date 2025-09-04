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
event_study_figure_name <- here(output_dir, "cell_level_event_study_2x2.png")

# Load models and data
load(file = here("data", "output", "gridcell_models.RData"))

# LOAD PANEL DATA
panel <- function() {
  readRDS(here("data", "processed", "ev_panel.rds")) |> 
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

replace_table_headers <- function(file, new_headers) {
  lines <- readLines(file)
  header_line_idx <- which(grepl("& \\(", lines))
  if (length(header_line_idx) == 0) {
    stop("Header line not found")
  }
  header_line <- lines[header_line_idx]
  for (i in 1:length(new_headers)) {
    header_line <- sub(paste0("\\(", i, "\\)"), new_headers[i], header_line)
  }
  lines[header_line_idx] <- header_line
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

# =============================================================================
# 4. BUILD LATEX TABLE
# =============================================================================
# Set up dictionary for variable names
setFixest_dict(c(
  post = "Post-Attack",
  "time_vessels" = "Time per Vessel (hrs)",
  "dist_vessels" = "Distance per Vessel (km)",
  time_hours = "Total Time (hrs)",
  distance_km = "Total Distance (km)",
  n_vessels = "Number of Vessels"
))

# Create LaTeX table
msummary(list("Total Time" = m1_total_time,
              "Total Distance" = m2_total_distance,
              "Time per Vessel" = m3_time_per_vessel,
              "Distance per Vessel" = m4_distance_per_vessel),
         coef_rename = c("Post-Attack"),
         gof_omit = "N|R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.3f",
         add_rows = rows,
         title = "Effect of Pirate Attacks on Grid Cell Shipping Activity. \\label{tab:cell-post-regression}",
         notes = list("The unit of observation is a grid cell-day. Each column examines a different shipping activity measure:
                      time per vessel (hours), distance per vessel (kilometers), total time (hours), and total distance (kilometers).
                      Post-Attack is a binary indicator equal to 1 for days on or after a pirate attack in the grid cell.
                      The analysis uses a 7-day window around attacks to identify treatment periods.
                      All regressions include grid cell, year-month, day of week, and ASAM subregion fixed effects."),
         threeparttable = TRUE,
         escape = FALSE,
         output = reg_table_name)

# Apply formatting functions
add_adjust_box(reg_table_name)
replace_table_headers(reg_table_name,
                     c("Time per Vessel", "Distance per Vessel", "Total Time", "Total Distance"))
adjust_notes_font_size(reg_table_name)

# =============================================================================
# 4. BUILD EVENT-STUDY PLOTS
# =============================================================================

# Function to create event study plot for a given outcome variable
create_event_study_plot <- function(outcome_var, title, y_label) {
  # Run regression for the specific outcome
  model <- conleyreg(
    as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id + year^month + day_of_week + asam_subregion")),
    unit = "id",
    time = "date",
    lat = "lat_bin",
    lon = "lon_bin",
    data = panel(),
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
    geom_point(size = 2, color = "#000000") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, color = "#000000") +
    labs(
      title = title,
      x = "Days Relative to Attack",
      y = y_label
    ) +
    theme_minimal(base_size = 12) +
    scale_x_continuous(breaks = seq(-7, 7, 2))
}

# Create all four event study plots
p1 <- create_event_study_plot("time_hours", "Time", "asinh(hours)")
p2 <- create_event_study_plot("distance_km", "Distance", "asinh(kilometer)")
p3 <- create_event_study_plot("time_vessels", "Time / Vessel", "asinh(hours/vessel)")
p4 <- create_event_study_plot("dist_vessels", "Distance / Vessel", "asinh(kilometer/vessel)")

# Combine plots into 2x2 grid
combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# Save the combined plot
ggsave(
  filename = event_study_figure_name,
  plot = combined_plot,
  width = 12,
  height = 10,
  dpi = 300
)





