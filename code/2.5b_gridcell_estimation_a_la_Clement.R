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
Sys.setenv(RGL_USE_NULL = TRUE) # If runnin on newer macs, this needs to be used per: https://github.com/chaisemartinPackages/did_multiplegt_dyn

# Load packages ----------------------------------------------------------------
library(DIDmultiplegtDYN)
library(tidyverse)
library(here)

theme_set(theme_minimal(base_size = 7))

# Load data --------------------------------------------------------------------
panel <- readRDS(here("data", "processed", "attacks_and_activity_by_grid.rds")) |>
  replace_na(replace = list(time_hours = 0,
                            distance_km = 0,
                            n_vessels = 0)) |>
  # Apply inverse hyperbolic sine transformation
  mutate(time_per_vessel = asinh(time_hours / n_vessels),
         dist_per_vessel = asinh(distance_km / n_vessels),
         time_hours = asinh(time_hours),
         distance_km = asinh(distance_km))

## PROCESSING ##################################################################

effects <- 7
placebos <- 7
group <- "grid_id"
time <- "date"
treatment <- "number_previous_attacks_grid_1_month"

wrap <- function(outcome, force = F) {

  out_file <- here("data", "output", "es_a_la_Clement", paste0("es_mod_", outcome, ".rds"))

  if(!(file.exists(out_file)) || force) {
    print(paste("File", out_file, "not found, proceeding to estimate model"))
    reg <- did_multiplegt_dyn(df = panel,
                              outcome = outcome,
                              effects = effects,
                              placebo = placebos,
                              group = group,
                              time = time,
                              treatment = treatment)

    saveRDS(reg, file = out_file)
  } else {
    print(paste("File", out_file, "already exist, skipping estimation"))
  }
}

outcomes <- c("time_hours",
              "distance_km",
              "time_per_vessel",
              "dist_per_vessel")

walk(outcomes, wrap, force = T)
beepr::beep(4)


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
           title = case_when(outcome == "time_hours" ~ "Time",
                             outcome == "time_per_vessel" ~ "Time / Vessel",
                             outcome == "distance_km" ~ "Distance",
                             outcome == "dist_per_vessel" ~ "Distance / Vessel")) |>
    select(outcome, title, term, event, estimate, se, lb_ci, ub_ci, n, switchers, n_w, switchers_w) |>
    arrange(event)
}


build_event_study_plot <- function(coef_table) {
  ggplot(data = coef_table,
         mapping = aes(x = event, y = estimate)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(size = 2, color = "#000000") +
    geom_errorbar(aes(ymin = lb_ci, ymax = ub_ci), width = 0.3, color = "#000000") +
    facet_wrap(~title,
               scales = "free_y") +
    labs(x = "Relative time to last period before treatment changes (t = 0)",
         y = "Estimate ± (std.error & 95% CI)") +
    theme_minimal(base_size = 12) +
    scale_x_continuous(breaks = seq(-7, 7, 2))
}

files <- list.files(here("data", "output", "es_a_la_Clement"), pattern = "es_mod", full.names = T)

mods <- map(files, readRDS)

es_plot_clement <- mods |>
  map_dfr(build_coef_table) |>
  build_event_study_plot()

ggsave(plot = es_plot_clement,
       filename = here("results", "figures_and_tables", "grid_level_event_study_multiple_gt_dyn.pdf"),
       width = 9,
       height = 6,
       units = "cm")
