################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Event study but using did_multiplegt_dyn
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
library(DIDmultiplegtDYN)
library(tidyverse)
library(furrr)
library(here)

theme_set(theme_minimal(base_size = 7))

# Load data --------------------------------------------------------------------
grid_level_panel <- readRDS(file = here("data",
                                        "processed",
                                        "attacks_and_activity_by_grid.rds")) %>%
  mutate(time_hours = asinh(time_hours),
         distance_km = asinh(distance_km),
         n_vessels = asinh(n_vessels),
         n_trips = asinh(n_trips))

## PROCESSING ##################################################################

effects <- 7
placebos <- 5
group <- "grid_id"
time <- "date"
treatment <- "number_previous_attacks_grid_1_month"

wrap <- function(outcome) {
  out_file <- here("data", "output", paste0("es_mod_", outcome, "_2025.rds"))

  if(!file.exists(out_file)) {
    print(paste("File", out_file, "not found, proceeding to estimate model"))
    reg <- did_multiplegt_dyn(df = grid_level_panel,
                              outcome = outcome,
                              effects = effects,
                              placebo = placebos,
                              group = group,
                              time = time,
                              treatment = treatment)

    saveRDS(reg, file = out_file)
  }
}

outcomes <- c("time_hours",
              "distance_km",
              "n_vessels",
              "n_trips")

plan("multisession", workers = 4)
future_walk(outcomes[1], wrap)
plan(sequential)


## VISUALIZE ###################################################################
build_coef_table <- function(model) {
  # browser()
  bind_rows(as_tibble(model$results$Placebos, rownames = NA),
            as_tibble(model$results$Effects, rownames = NA)) |>
    rownames_to_column(var = "term") |>
    mutate(term = str_squish(term),
           event = as.numeric(str_extract(term, "[:digit:]+")),
           event = ifelse(str_detect(term, "Placebo"), -1, 1) * event) |>
    janitor::clean_names() |>
    bind_rows(tibble(term = "Placebo 0",
                     event = 0,
                     estimate = 0,
                     se = 0,
                     lb_ci = 0,
                     ub_ci = 0)) |>
    mutate(outcome = model$args$outcome,
           title = case_when(outcome == "time_hours" ~ "Occupancy (hr)",
                             outcome == "time_vessel" ~ "Occupancy per vessel (hr/vessel)",
                             outcome == "time_trip" ~ "Occupancy per voyage (hr/voyage)",
                             outcome == "distance_km" ~ "Distance (km)",
                             outcome == "distance_vessel" ~ "Distance per vessel (km/vessel)",
                             outcome == "distance_trip" ~ "Distance per voyage (km/trip)",
                             outcome == "n_vessels" ~ "Transit (# vessels)",
                             outcome == "n_trips" ~ "Transit (# voyages)")) |>
    select(outcome, title, term, event, estimate, se, lb_ci, ub_ci, n, switchers, n_w, switchers_w) |>
    arrange(event)
}


build_event_study_plot <- function(coef_table) {
  ggplot(data = coef_table,
         mapping = aes(x = event, y = estimate)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_linerange(aes(ymin = lb_ci,
                       ymax = ub_ci),
                   linewidth = 0.2,
                   color = "black") +
    geom_line() +
    geom_pointrange(aes(ymin = estimate - se,
                        ymax = estimate + se),
                    size = 0.25,
                    linewidth = 1,
                    color = "cadetblue") +
    facet_wrap(~title,
               scales = "free_y") +
    labs(x = "Relative time to last period before treatment changes (t = 0)",
         y = "Estimate ± (std.error & 95% CI)")
}

files <- list.files("output_data", pattern = "es_mod", full.names = T)

mods <- map(files, readRDS)

es_plot_clement <- mods |>
  map_dfr(build_coef_table) |>
  build_event_study_plot()

ggsave(plot = es_plot_clement,
       filename = here("figures", "grid_level_event_study_multiple_gt_din.pdf"),
       width = 9,
       height = 6,
       units = "cm")
