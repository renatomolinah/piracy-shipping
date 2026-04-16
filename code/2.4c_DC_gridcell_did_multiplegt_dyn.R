# Robustness event study using did_multiplegt_dyn (de Chaisemartin and D'Haultfoeuille)

# --- Setup ---
library(DIDmultiplegtDYN)
library(polars)
library(tidyverse)
library(here)
library(modelsummary)
source(here("r", "table_format_helpers.R"))
source(here("code", "table_helpers.R"))

theme_set(theme_minimal(base_size = 10))

# --- Load daily panel ---

# Constructs transformed outcomes and a binary attack_day treatment indicator
load_dc_panel <- function(res = "0_5") {
  raw <- readRDS(here("data", "processed", paste0("attacks_and_activity_by_grid_", res, ".rds")))

  raw |>
    replace_na(replace = list(
      time_hours      = 0,
      distance_km     = 0,
      n_vessels       = 0,
      n_trips         = 0,
      n_ais_messages  = 0,
      n_ais_disabling = 0
    )) |>
    arrange(grid_id, date) |>
    group_by(grid_id) |>
    mutate(
      attack_day = if_else(!is.na(days_since_attack) & days_since_attack == 0, 1L, 0L)
    ) |>
    ungroup() |>
    mutate(
      time_per_vessel_raw = if_else(n_vessels > 0, time_hours / n_vessels, 0),
      dist_per_vessel_raw = if_else(n_vessels > 0, distance_km / n_vessels, 0)
    ) |>
    mutate(
      time_hours       = asinh(time_hours),
      distance_km      = asinh(distance_km),
      n_vessels        = asinh(n_vessels),
      n_trips          = asinh(n_trips),
      time_per_vessel  = asinh(time_per_vessel_raw),
      dist_per_vessel  = asinh(dist_per_vessel_raw)
    ) |>
    group_by(grid_id) |>
    mutate(tot = sum(attack_day)) |>
    # Restrict to single-switcher cells for clean identification
    filter(tot == 1) |>
    ungroup()
}

# --- Estimation settings ---
effects  <- 7
placebos <- 7
group    <- "grid_id"
time     <- "date"

treatment_main <- "attack_day"

outcomes <- c(
  "time_hours",
  "distance_km",
  "n_vessels",
  "n_trips",
  "time_per_vessel",
  "dist_per_vessel"
)

outcome_labels <- c(
  time_hours = "Occupancy Time (hours)",
  distance_km = "Distance Traveled (km)",
  n_vessels = "Transit (# Vessels)",
  n_trips = "Transit (# Trips)",
  time_per_vessel = "Occupancy per Vessel (hours / vessel)",
  dist_per_vessel = "Distance Traveled per Vessel (km / vessel)"
)

outcome_titles <- c(
  time_hours = "A) Occupancy Time (hours)",
  distance_km = "B) Distance Traveled (km)",
  n_vessels = "C) Transit (# Vessels)",
  n_trips = "D) Transit (# Trips)",
  time_per_vessel = "E) Occupancy per Vessel (hours / vessel)",
  dist_per_vessel = "F) Distance Traveled per Vessel (km / vessel)"
)

# --- Estimation wrapper ---
run_dc_estimation <- function(res = "0_5", treatment_var = treatment_main, force = FALSE) {
  message("Loading panel for resolution: ", res)
  panel <- load_dc_panel(res = res)

  out_dir <- here("data", "output", paste0("es_DC_", treatment_var, "_", res))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  wrap <- function(outcome) {
    out_file <- file.path(out_dir, paste0("es_DC_", outcome, ".rds"))

    if (!file.exists(out_file) || force) {
      message("Estimating DC model: outcome = ", outcome,
              ", treatment = ", treatment_var, ", res = ", res)

      reg <- did_multiplegt_dyn(
        df        = panel,
        outcome   = outcome,
        effects   = effects,
        placebo   = placebos,
        group     = group,
        time      = time,
        treatment = treatment_var,
        same_switchers = TRUE,
        same_switchers_pl = TRUE,
        more_granular_demeaning = TRUE
      )

      write_rds(reg, file = out_file)
    } else {
      message("File ", out_file, " exists and force = FALSE, skipping.")
    }
  }

  walk(outcomes, wrap)
}

# --- Visualization helpers ---
extract_dc_model_diagnostics <- function(model) {
  results <- model[["results"]]
  args <- model[["args"]]

  ate_tbl <- results[["ATE"]]

  outcome_name <- args[["outcome"]]
  if (is.null(outcome_name)) outcome_name <- NA_character_
  outcome_label <- if (outcome_name %in% names(outcome_labels)) outcome_labels[[outcome_name]] else outcome_name

  treatment_name <- args[["treatment"]]
  if (is.null(treatment_name)) treatment_name <- NA_character_

  first_or_na <- function(x, nm) {
    if (is.null(x)) return(NA_real_)
    if (!(nm %in% colnames(x))) return(NA_real_)
    val <- suppressWarnings(as.numeric(x[1, nm, drop = TRUE]))
    if (length(val) == 0) return(NA_real_)
    val
  }

  tibble(
    outcome = outcome_name,
    outcome_label = outcome_label,
    treatment = treatment_name,
    ate = first_or_na(ate_tbl, "Estimate"),
    ate_se = first_or_na(ate_tbl, "SE"),
    ate_ci_low = first_or_na(ate_tbl, "LB CI"),
    ate_ci_high = first_or_na(ate_tbl, "UB CI"),
    p_value_joint_effects = suppressWarnings(as.numeric(results[["p_jointeffects"]])),
    p_value_joint_placebos = suppressWarnings(as.numeric(results[["p_jointplacebo"]])),
    avg_treatment_accumulation_periods = suppressWarnings(as.numeric(results[["delta_D_avg_total"]])),
    n_ate = first_or_na(ate_tbl, "N"),
    switchers_ate = first_or_na(ate_tbl, "Switchers"),
    n_w_ate = first_or_na(ate_tbl, "N.w"),
    switchers_w_ate = first_or_na(ate_tbl, "Switchers.w")
  )
}

build_dc_summary_table <- function(mods) {
  map_dfr(mods, extract_dc_model_diagnostics) |>
    arrange(outcome_label) |>
    select(
      outcome, outcome_label,
      ate, ate_se,
      p_value_joint_effects, p_value_joint_placebos,
      avg_treatment_accumulation_periods,
      n_ate, switchers_ate, n_w_ate, switchers_w_ate
    )
}

# Tidy coefficient table with event-time indexing from Placebos/Effects output
build_dc_coef_table <- function(model) {
  bind_rows(
    as_tibble(model$results$Placebos, rownames = NA),
    as_tibble(model$results$Effects,   rownames = NA)
  ) |>
    rownames_to_column(var = "term") |>
    mutate(
      term  = str_squish(term),
      event = as.numeric(str_extract(term, "[:digit:]+")),
      event = if_else(str_detect(term, "Placebo"), -1, 1) * event
    ) |>
    janitor::clean_names() |>
    bind_rows(tibble(
      term     = "Placebo 0",
      event    = 0,
      estimate = 0,
      se       = 0,
      lb_ci    = 0,
      ub_ci    = 0
    )) |>
    mutate(
      outcome = model$args$outcome,
      title = dplyr::coalesce(unname(outcome_titles[outcome]), outcome)
    ) |>
    select(
      outcome, title, term, event,
      estimate, se, lb_ci, ub_ci,
      n, switchers, n_w, switchers_w
    ) |>
    arrange(event)
}

build_dc_event_study_plot <- function(coef_table) {
  coef_table |>
    mutate(
      title = factor(title, levels = unname(outcome_titles))
    ) |>
    ggplot(aes(x = event, y = estimate)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    geom_linerange(aes(ymin = lb_ci, ymax = ub_ci),
                   linewidth = 0.5, color = "black") +
    geom_linerange(aes(ymin = estimate - se, ymax = estimate + se),
                   linewidth = 1.5, color = "steelblue") +
    geom_point(size = 3, color = "steelblue") +
    facet_wrap(~ title, scales = "free_y", ncol = 2) +
    labs(
      x = "Event time (days relative to first attack switch)",
      y = "Estimate ± (std. error & 95% CI)"
    ) +
    theme_minimal(base_size = 12) +
    scale_x_continuous(breaks = seq(-7, 7, 2))
}

# --- Run main specification ---
res <- "0_5"

run_dc_estimation(res = res, treatment_var = treatment_main, force = TRUE)

out_dir_main <- here("data", "output", paste0("es_DC_", treatment_main, "_", res))

files <- list.files(
    out_dir_main,
    pattern = "^es_DC_.*\\.rds$",
    full.names = TRUE
)


mods <- map(files, readRDS)

es_plot_dc <- mods |>
  map_dfr(build_dc_coef_table) |>
  build_dc_event_study_plot()

event_study_file <- here("results", "figures_and_tables",
                         "grid_level_event_study_DC_did_multiplegt_dyn_0_5.png")

ggsave(plot     = es_plot_dc,
       filename = event_study_file,
       width    = 9,
       height   = 9)

dc_summary_tbl <- build_dc_summary_table(mods)

dc_summary_display <- dc_summary_tbl |>
  transmute(
    Outcome = outcome_label,
    ATE = ate,
    `SE` = ate_se,
    `p (effects)` = p_value_joint_effects,
    `p (placebos)` = p_value_joint_placebos
  )

dc_summary_file <- here("results", "figures_and_tables", "grid_level_did_multiplegt_dyn_summary.tex")

dc_note_text <- paste0(
  "Each row corresponds to one transformed shipping-activity outcome estimated from the daily grid-cell panel. ",
  "ATE is the average cumulative effect from DIDmultiplegtDYN and p-values correspond to the joint null tests for post-treatment effects and placebo leads. ",
  "All regressions use N = ", format(unique(dc_summary_tbl$n_ate), big.mark = ","),
  " of which N = ", format(unique(dc_summary_tbl$switchers_ate), big.mark = ","), " come from switchers."
)

datasummary_df(
  dc_summary_display,
  output = dc_summary_file,
  fmt = 3,
  title = "Effect of Pirate Encounters on Grid Cell Shipping Activity. \\label{tab:grid-level-did-multiplegt-dyn-summary}",
  escape = FALSE
)

# Post-process: wrap in threeparttable with standardized formatting
tex_lines <- readLines(dc_summary_file, warn = FALSE)
tex_lines <- tex_lines[!grepl("\\\\multicolumn.*\\\\rule", tex_lines)]
writeLines(tex_lines, dc_summary_file)

tex_lines <- readLines(dc_summary_file, warn = FALSE)
tabular_begin <- grep("\\\\begin\\{tabular\\}", tex_lines)[1]
tabular_end <- grep("\\\\end\\{tabular\\}", tex_lines)
tabular_end <- tabular_end[length(tabular_end)]
tex_lines <- c(
  tex_lines[1:(tabular_begin - 1)],
  "\\begin{adjustbox}{width = .9\\textwidth}",
  "\\begin{threeparttable}",
  tex_lines[tabular_begin:tabular_end],
  "\\begin{tablenotes}",
  paste0("\\item \\scriptsize ", dc_note_text),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{adjustbox}",
  tex_lines[(tabular_end + 1):length(tex_lines)]
)
writeLines(tex_lines, dc_summary_file)
