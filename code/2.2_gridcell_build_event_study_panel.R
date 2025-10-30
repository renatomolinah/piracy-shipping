################################################################################
#
# Builds the event horizon event study panel for gridcell analysis
#
################################################################################

# Load packages
library(here)
library(tidyverse)

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

panel <- function(res = "0_5"){
  print("Loading data")
  
  readRDS(here("data", "processed", paste0("attacks_and_activity_by_grid_", res, ".rds"))) %>%
    mutate(attack = ifelse(days_since_attack == 0, TRUE, FALSE)) |> 
    mutate(month = month(date),
           year = year(date)) |> 
    rename(id = grid_id)
}

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
# 3. BUILD PANEL
# =============================================================================

make_panel <- function(res = c("0_5", "0_1", "1"), window = 7){
  data <- panel(res) %>%
    arrange(id, date) %>%
    group_by(id) %>%
    mutate(
      attack_id = identify_and_populate_attacks(id, date, attack, window = window),
      relative_time = calculate_relative_time_rle(date, id, attack_id, window = window),
      attack_id = propagate_attack_id(date, id, attack_id, window = window)
    ) %>%
    ungroup() |> 
    filter(!is.na(relative_time)) %>%
    group_by(id) %>%
    mutate(
      post  = ifelse(relative_time >= 0, 1, 0),
      id = cur_group_id(),
      day_of_week = weekdays(date),
      date = as.numeric(date)
    ) %>%
    ungroup()
  
  # Export the resulting file
  saveRDS(data, here("data", "processed", paste0("ev_panel_", res, ".rds")))
  
}

# Now run the pipeline for each 
make_panel("0_1")
make_panel("0_5")
make_panel("1")

