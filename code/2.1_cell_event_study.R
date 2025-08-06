# =============================================================================
# CELL-LEVEL EVENT STUDY ANALYSIS
# =============================================================================
# This script performs event study analysis at the grid cell level to examine
# how shipping activity responds to piracy attacks over time.
# =============================================================================

library(here)
library(conleyreg)
library(tidyverse)
library(patchwork)
library(fixest)
library(modelsummary)

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

panel <- readRDS(here("data", "processed", "attacks_and_activity_by_grid.rds")) %>%
  mutate(attack = ifelse(days_since_attack == 0, TRUE, FALSE))

panel <- panel %>%
  mutate(
    month = month(date),
    year = year(date)
  )

# =============================================================================
# 2. EVENT STUDY FUNCTIONS
# =============================================================================

identify_and_populate_attacks <- function(id, dates, attack, window) {
  attack_id <- rep(NA, length(attack))
  attack_counter <- 0
  group_anchor <- NA
  processed_indices <- rep(FALSE, length(attack))
  current_id <- NA
  
  for (i in seq_along(attack)) {
    if (!is.na(id[i]) && (is.na(current_id) || id[i] != current_id)) {
      current_id <- id[i]
      attack_counter <- 0
    }
    
    if (!is.na(attack[i]) && attack[i] && !processed_indices[i]) {
      attack_counter <- attack_counter + 1
      group_anchor <- dates[i]
      attack_id[i] <- paste0("attack_", id[i], "_", attack_counter)
      processed_indices[i] <- TRUE
      
      for (j in (i + 1):length(attack)) {
        if (!is.na(id[j]) && id[j] != id[i]) break
        
        if (j > length(attack) || is.na(attack[j]) || processed_indices[j]) next
        
        days_diff <- as.numeric(difftime(dates[j], group_anchor, units = "days"))
        
        if (days_diff <= window * 2 && attack[j]) {
          attack_id[j] <- attack_id[i]
          processed_indices[j] <- TRUE
          group_anchor <- dates[j]
        }
        
        if (days_diff > window * 2) break
      }
    }
  }
  
  for (i in 2:length(attack_id)) {
    if (is.na(attack_id[i]) && !is.na(attack_id[i - 1])) {
      for (j in i:length(attack_id)) {
        if (!is.na(attack_id[j]) && attack_id[j] == attack_id[i - 1]) {
          attack_id[i:j] <- attack_id[i - 1]
          break
        } else if (!is.na(attack_id[j]) && attack_id[j] != attack_id[i - 1]) {
          break
        }
      }
    }
  }
  
  return(attack_id)
}

calculate_relative_time_rle <- function(dates, id, attack_id, window) {
  relative_time <- rep(NA, length(attack_id))
  unique_ids <- unique(id)
  
  for (current_id in unique_ids) {
    id_indices <- which(id == current_id)
    current_attack_id <- attack_id[id_indices]
    current_dates <- dates[id_indices]
    
    events <- rle(!is.na(current_attack_id))
    current_index <- 1
    
    for (i in seq_along(events$values)) {
      run_length <- events$lengths[i]
      
      if (events$values[i]) {
        start_index <- current_index
        end_index <- current_index + run_length - 1
        
        relative_time[id_indices[start_index:end_index]] <- 0
        
        if (start_index > 1) {
          pre_event_length <- min(window, start_index - 1)
          relative_time[id_indices[(start_index - pre_event_length):(start_index - 1)]] <- 
            -pre_event_length:-1
        }
        
        if (end_index < length(current_attack_id)) {
          post_event_length <- min(window, length(current_attack_id) - end_index)
          relative_time[id_indices[(end_index + 1):(end_index + post_event_length)]] <- 
            1:post_event_length
        }
      }
      
      current_index <- current_index + run_length
    }
  }
  
  return(relative_time)
}

propagate_attack_id <- function(dates, id, attack_id, window) {
  if (length(dates) != length(attack_id) || length(dates) != length(id)) {
    stop("Dates, id, and attack_id must have the same length.")
  }
  
  propagated_attack_id <- attack_id
  unique_ids <- unique(id)
  
  for (current_id in unique_ids) {
    id_indices <- which(id == current_id)
    current_attack_id <- attack_id[id_indices]
    
    for (i in seq_along(current_attack_id)) {
      if (!is.na(current_attack_id[i])) {
        if (i > 1) {
          for (j in seq(i - 1, max(1, i - window), by = -1)) {
            if (is.na(propagated_attack_id[id_indices[j]])) {
              propagated_attack_id[id_indices[j]] <- current_attack_id[i]
            } else {
              break
            }
          }
        }
        
        if (i < length(current_attack_id)) {
          for (j in seq(i + 1, min(length(current_attack_id), i + window), by = 1)) {
            if (is.na(propagated_attack_id[id_indices[j]])) {
              propagated_attack_id[id_indices[j]] <- current_attack_id[i]
            } else {
              break
            }
          }
        }
      }
    }
  }
  
  if (length(propagated_attack_id) != length(attack_id)) {
    stop("Length mismatch: propagated_attack_id and attack_id must have the same length.")
  }
  
  return(propagated_attack_id)
}

# =============================================================================
# 3. ANALYSIS
# =============================================================================

window <- 7

panel <- panel %>% rename(id = grid_id)

panel <- panel %>%
  arrange(id, date) %>%
  group_by(id) %>%
  mutate(
    attack_id = identify_and_populate_attacks(id, date, attack, window = window),
    relative_time = calculate_relative_time_rle(date, id, attack_id, window = window),
    attack_id = propagate_attack_id(date, id, attack_id, window = window)
  ) %>%
  ungroup()

ev_panel <- panel %>%
  filter(!is.na(relative_time)) %>%
  group_by(id) %>%
  mutate(
    post  = ifelse(relative_time >= 0, 1, 0),
    id = cur_group_id(),
    day_of_week = weekdays(date),
    date = as.numeric(date)
  ) %>%
  ungroup()

saveRDS(ev_panel, here("data", "processed", "ev_panel.rds"))

# =============================================================================
# 4. HELPER FUNCTIONS
# =============================================================================

load_ev_panel <- function() {
  panel <- readRDS(here("data", "processed", "ev_panel.rds"))
  return(panel)
}

# LaTeX table helper functions
add_adjust_box <- function(file, line_before = "\\begin{adjustbox}{width = .9\\textwidth}", line_after = "\\end{adjustbox}", before = "\\begin{threeparttable}", after = "\\end{threeparttable}") {
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
# 5. POST-REGRESSION ANALYSIS
# =============================================================================

# Set up dictionary for variable names
setFixest_dict(c(
  post = "Post-Attack",
  "time_hours/n_vessels" = "Time per Vessel (hrs)",
  "distance_km/n_vessels" = "Distance per Vessel (km)",
  time_hours = "Total Time (hrs)",
  distance_km = "Total Distance (km)",
  n_vessels = "Number of Vessels"
))

# Run post-regressions for different outcome variables using conleyreg with asinh transformation
m1_total_time <- conleyreg(
  asinh(time_hours) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m2_total_distance <- conleyreg(
  asinh(distance_km) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m3_time_per_vessel <- conleyreg(
  asinh(time_hours/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

m4_distance_per_vessel <- conleyreg(
  asinh(distance_km/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  unit = "id",
  time = "date",
  lat = "lat_bin",
  lon = "lon_bin",
  data = load_ev_panel(),
  dist_cutoff = 50,
  lag_cutoff = Inf
)

obs_reg_1 <- feols(
  asinh(time_hours) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )

obs_reg_2 <- feols(
  asinh(distance_km) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )

obs_reg_3 <- feols(
  asinh(time_hours/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )

obs_reg_4 <- feols(
  asinh(distance_km/n_vessels) ~ post | id + year^month + day_of_week + asam_subregion,
  data = load_ev_panel()
  )


# Create rows for additional information
rows <- tribble(
  ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)",
  "", "", "", "", "",
  "Observations", as.character(nobs(obs_reg_1) %>% format(big.mark = ",")),
  as.character(nobs(obs_reg_2) %>% format(big.mark = ",")),
  as.character(nobs(obs_reg_3) %>% format(big.mark = ",")),
  as.character(nobs(obs_reg_4) %>% format(big.mark = ",")),
  "", "", "", "", "",
  "Grid Cell FE", "X", "X", "X", "X",
  "Year-Month FE", "X", "X", "X", "X",
  "Day of Week FE", "X", "X", "X", "X",
  "Subregion FE", "X", "X", "X", "X"
)

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
         output = here("data", "figures and tables", "cell_post_regression.tex"))

# Apply formatting functions
add_adjust_box(here("data", "figures and tables", "cell_post_regression.tex"))
replace_table_headers(here("data", "figures and tables", "cell_post_regression.tex"), 
                     c("Time per Vessel", "Distance per Vessel", "Total Time", "Total Distance"))
adjust_notes_font_size(here("data", "figures and tables", "cell_post_regression.tex"))

# =============================================================================
# 6. VISUALIZATION
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
    data = load_ev_panel(),
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
p3 <- create_event_study_plot("time_hours/n_vessels", "Time / Vessel", "asinh(hours/vessel)")
p4 <- create_event_study_plot("distance_km/n_vessels", "Distance / Vessel", "asinh(kilometer/vessel)")

# Combine plots into 2x2 grid
combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# Save the combined plot
ggsave(
  filename = here("data", "figures and tables", "cell_level_event_study_2x2.png"),
  plot = combined_plot,
  width = 12,
  height = 10,
  dpi = 300
)





