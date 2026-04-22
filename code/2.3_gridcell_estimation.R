# Estimate gridcell pre/post regressions with Conley standard errors via fixest.

# --- Setup ---
library(here)
library(fixest)
library(tidyverse)
library(furrr)
library(modelsummary)
library(kableExtra)
library(scales)

source(here("code", "table_helpers.R"))

processKBLoutput <- function(file_path) {

  lines <- readLines(file_path, warn = FALSE)

  hotspots_patterns <- c(
    "\\\\hspace\\{1em\\}G\\. of Aden",
    "\\\\hspace\\{1em\\}Southeast Asia",
    "\\\\hspace\\{1em\\}G\\. of Guinea",
    "\\\\hspace\\{1em\\}Rest of the World"
  )

  modified_lines <- lines
  for (i in seq_along(modified_lines)) {
    for (pattern in hotspots_patterns) {
      modified_lines[i] <- gsub(pattern, "\\\\hspace{1em}", modified_lines[i])
    }
  }

  writeLines(modified_lines, file_path)

  cat("The LaTeX file has been successfully processed and saved.\n")
}

# --- Load panel data ---
# Panels are loaded once at the top under stable names (panel_0_1, panel_0_5,
# panel_1) so that fits produced by feols() reference them via `fit$call$data`
# as bare symbols. This keeps the saved fits self-describing and allows
# downstream scripts (2.4) to resolve the data by loading the same names.
panel <- function(res) {
  readRDS(here("data", "processed", paste0("ev_panel_", res, ".rds"))) |>
    mutate(time_vessels = time_hours/n_vessels,
           dist_vessels = distance_km/n_vessels,
           attack_cluster = case_when(attack_cluster == "GoA" ~ "G. of Aden",
                                      attack_cluster == "GoG" ~ "G. of Guinea",
                                      attack_cluster == "SEA" ~ "S.E. Asia",
                                      T ~ attack_cluster))
}

panel_0_1 <- panel("0_1")
panel_0_5 <- panel("0_5")
panel_1   <- panel("1")

# --- Summary statistics table ---
P5 <- function(x) quantile(x, 0.05, na.rm = TRUE)
P95 <- function(x) quantile(x, 0.95, na.rm = TRUE)

by_cluster <- datasummary(attack_cluster * (Mean + SD + P5 + P95) ~ distance_km + time_hours + n_trips + n_vessels,
                          data = panel("0_5") %>%
                            mutate(attack_cluster = ifelse(attack_cluster == "None", "Rest of the world", attack_cluster),
                                   attack_cluster = fct_relevel(attack_cluster, "G. of Aden", "G. of Guinea", "S.E. Asia", "Rest of the world")),
                          output = "dataframe") %>%
  mutate(
    distance_km = as.numeric(distance_km),
    time_hours = as.numeric(time_hours),
    n_trips = as.numeric(n_trips),
    n_vessels = as.numeric(n_vessels)
  ) %>%
  mutate(across(c(distance_km, time_hours, n_trips, n_vessels), ~scales::comma(., accuracy = 0.1))) %>%
  select(-attack_cluster)


kbl(x = by_cluster,
    booktabs = TRUE,
    label = "grid_summary",
    caption = "Summary Statistics for Daily Ship Transit by Grid Cell.",
    col.names = c("", "Distance (km)", "Occupancy (hr)", "Voyages (\\#)", "Unique vessels (\\#)"),
    align = c("l", "r", "r", "r", "r"),
    linesep = "",
    format = "latex",
    escape = FALSE) %>%
  kable_styling() %>%
  pack_rows("Gulf of Aden", 1, 4) %>%
  pack_rows("Gulf of Guinea", 5, 8) %>%
  pack_rows("Southeast Asia", 9, 12) %>%
  pack_rows("Rest of the World", 13, 16) %>%
  footnote(general = "The unit of observation is a grid cell-day on a $0.5^{\\\\circ} \\\\times 0.5^{\\\\circ}$ grid. The sample includes 618 grid cells with at least one pirate encounter during the 2012--2023 period. Each column reports a different measure of daily shipping activity within a cell. P5 and P95 denote the 5th and 95th percentiles, respectively.",
           general_title = "",
           escape = FALSE,
           threeparttable = TRUE) %>%
  cat(file = here("results", "figures_and_tables", "grid_summary_stats.tex"))

processKBLoutput(here("results", "figures_and_tables", "grid_summary_stats.tex"))
adjust_notes_font_size(here("results", "figures_and_tables", "grid_summary_stats.tex"))


################################################################################
# --- Pre/post regression analysis ---
# Two estimation helpers:
# * run_estimation_multi() fits the model on the full sample AND each hotspot
#   in one feols() call via fsplit = ~ attack_cluster. Returns a fixest_multi.
# * run_estimation() fits a single model on the full sample (for the spec
#   build-up, which is Global-only). Returns a fixest.
# Both bake Conley spatial SEs (50 km cutoff) in at fit time. fixest drops NAs
# and singletons internally, so no manual handling is needed.
run_estimation_multi <- function(outcome_var,
                                 spec = "~ post | id^month + date",
                                 res = "0_5") {
  
  fml <- as.formula(paste(outcome_var, spec))
  
  cat("Estimating (multi) for", outcome_var, "at a resolution of", res, "\n")
  
  switch(res,
         "0_1" = feols(fml, data = panel_0_1, fsplit = ~ attack_cluster, vcov = conley(cutoff = 50)),
         "0_5" = feols(fml, data = panel_0_5, fsplit = ~ attack_cluster, vcov = conley(cutoff = 50)),
         "1"   = feols(fml, data = panel_1,   fsplit = ~ attack_cluster, vcov = conley(cutoff = 50))
  )
}

run_estimation <- function(outcome_var,
                           spec = "~ post | id^month + date",
                           res = "0_5") {
  
  fml <- as.formula(paste(outcome_var, spec))
  
  cat("Estimating for", outcome_var, "at a resolution of", res, "\n")
  
  switch(res,
         "0_1" = feols(fml, data = panel_0_1, vcov = conley(cutoff = 50)),
         "0_5" = feols(fml, data = panel_0_5, vcov = conley(cutoff = 50)),
         "1"   = feols(fml, data = panel_1,   vcov = conley(cutoff = 50))
  )
}

# Unpack a fixest_multi into a tibble with one row per hotspot (Global + three
# attack clusters). Drops the "None" (rest-of-world) split since we never
# report it. hotspot_multi is the fixest_multi; hotspot labels match what the
# downstream tables expect.
unpack_hotspots <- function(multi) {
  tibble(
    hotspot = c("Global", "G. of Aden", "G. of Guinea", "S.E. Asia"),
    model   = list(
      multi[sample = "Full sample",  drop = TRUE],
      multi[sample = "G. of Aden",   drop = TRUE],
      multi[sample = "G. of Guinea", drop = TRUE],
      multi[sample = "S.E. Asia",    drop = TRUE]
    )
  )
}

# Define all our outcome variables
outcome_vars <- c("asinh(time_hours)",
                  "asinh(distance_km)",
                  "asinh(n_vessels)",
                  "asinh(n_trips)",
                  "asinh(time_vessels)",
                  "asinh(dist_vessels)")

# Different resolutions
res <- c("0_1", "0_5", "1")

# Main text estimation. Each (outcome, res) pair runs ONE feols() call with
# fsplit = ~ attack_cluster, which fits the full sample AND each hotspot
# simultaneously. We then unpack the fixest_multi into four rows per pair.
# This replaces the previous 6 outcomes x 4 hotspots x 3 res = 72 calls with
# 6 x 3 = 18 calls -- and eliminates the hotspot subsetting logic entirely.
# The main-text FE spec absorbs grid-cell-by-month (local seasonality) and a
# full set of time FEs.
plan(multisession, workers = 18)
models <- expand_grid(outcome_var = outcome_vars,
                      spec = "~ post | id^month + date",
                      res = res) |>
  mutate(multi = future_pmap(.l = list(outcome_var = outcome_var, spec = spec, res = res),
                             run_estimation_multi,
                             .options = furrr_options(seed = NULL))) |>
  mutate(unpacked = map(multi, unpack_hotspots)) |>
  select(-multi) |>
  unnest(unpacked) |>
  mutate(n = map_dbl(model, nobs))
plan(sequential)

# Name the model list by hotspot so modelsummary uses them as column headers.
names(models$model) <- models$hotspot

################################################################################
# Supplementary models
# Part 1 - specifications building up with FEs. The main-text spec
# `~ post | id^month + date` is the final "column" but is already computed in
# the `models` tibble above, so it is NOT re-added here.
all_specs <- c("~ post | id",
               "~ post | id + date")

# Estimation for specification table, only done for 0.5° on the full sample.
plan(multisession, workers = 18)
spec_tables <- expand_grid(outcome_var = outcome_vars,
                           spec = all_specs) |>
  mutate(model = future_pmap(.l = list(outcome_var = outcome_var, spec = spec),
                      run_estimation,
                      res = "0_5",
                      .options = furrr_options(seed = NULL)),
         n = map_dbl(model, nobs))
plan(sequential)

# Part 2 - AIS disabling events
# SE Asia excluded from AIS disabling because outcome is constant at 0.
# One fsplit feols() call fits Global + each hotspot simultaneously; we then
# drop the S.E. Asia row.
AIS_disab_models <- tibble(outcome_var = "asinh(n_ais_disabling)",
                           spec = "~ post | id^month + date",
                           res = "0_5") |>
  mutate(multi = pmap(.l = list(outcome_var = outcome_var, spec = spec, res = res),
                      run_estimation_multi)) |>
  mutate(unpacked = map(multi, unpack_hotspots)) |>
  select(-multi) |>
  unnest(unpacked) |>
  filter(hotspot != "S.E. Asia") |>
  mutate(n = map_dbl(model, nobs))

names(AIS_disab_models$model) <- AIS_disab_models$hotspot

# --- Export all models ---
save(models,
     file = here("data", "output", "gridcell_models.RData"))
save(spec_tables,
     file = here("data", "output", "gridcell_spec_models.RData"))
save(AIS_disab_models,
     file = here("data", "output", "gridcell_AIS_disabling.RData"))
