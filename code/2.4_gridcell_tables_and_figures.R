# Build LaTeX tables and event study figures from gridcell regression output

# --- Setup ---
library(here)
library(tidyverse)
library(patchwork)
library(fixest)
library(modelsummary)

output_dir <- here("results", "figures_and_tables")
reg_table_name <- here(output_dir, "cell_post_regression.tex")
spec_buildup_table_name <- here(output_dir, "cell_post_regression_spec_buildup.tex")
AIS_disab_table_name <- here(output_dir, "AIS_disabling_cell_post_regression.tex")
event_study_figure_name <- here(output_dir, "cell_level_event_study_2x3.png")
supplementary_event_study_figure_name <- here(output_dir, "cell_level_event_study_2x3_by_resolution.png")
resolution_figure_name <- here(output_dir, "cell_post_regression_by_resolution.png")

load(file = here("data", "output", "gridcell_models.RData"))
load(file = here("data", "output", "gridcell_spec_models.RData"))
load(file = here("data", "output", "gridcell_AIS_disabling.RData"))

panel <- function(res) {
  readRDS(here("data", "processed", paste0("ev_panel_", res, ".rds"))) |>
    mutate(time_vessels = time_hours/n_vessels,
           dist_vessels = distance_km/n_vessels)
}

# Panels must be in scope because the event-study plot functions below fit
# fresh feols models, and because some post-load fixest helpers occasionally
# reach for the original data frame even when the vcov is pre-baked.
panel_0_1 <- panel("0_1")
panel_0_5 <- panel("0_5")
panel_1   <- panel("1")

# --- Helper functions ---
source(here("code", "table_helpers.R"))

# gof_map recipe reused by every modelsummary call below: include the N row,
# rename it to "Observations", and format values with thousand separators.
obs_gof_map <- list(list(raw = "nobs",
                         clean = "Observations",
                         fmt = function(x) format(x, big.mark = ",", scientific = FALSE)))

# --- Build LaTeX table ---
# msummary consumes fitted feols objects directly. Each `pull(model)` returns a
# named list (named by hotspot courtesy of names(models$model) <- models$hotspot
# in 2.3) so the resulting panels have one column per geographic region with
# proper headers. Conley SEs are baked into each fit and surface as "(0.019)".
time <- models |> filter(outcome_var == "asinh(time_hours)", res == "0_5") |> pull(model)
distance <- models |> filter(outcome_var == "asinh(distance_km)", res == "0_5") |> pull(model)
n_vessels <- models |> filter(outcome_var == "asinh(n_vessels)", res == "0_5") |> pull(model)
n_trips <- models |> filter(outcome_var == "asinh(n_trips)", res == "0_5") |> pull(model)
time_per_vessel <- models |> filter(outcome_var == "asinh(time_vessels)", res == "0_5") |> pull(model)
distance_per_vessel <- models |> filter(outcome_var == "asinh(dist_vessels)", res == "0_5") |> pull(model)

msummary(models = list("Panel (A): Occupancy Time (hours)" = time,
                       "Panel (B): Distance Traveled (km)" = distance,
                       "Panel (C): Transit (# Vessels)" = n_vessels,
                       "Panel (D): Transit (# Voyages)" = n_trips,
                       "Panel (E): Occupancy per Vessel (hours / vessel)" = time_per_vessel,
                       "Panel (F): Distance Traveled per Vessel (km / vessel)" = distance_per_vessel),
         shape = "rbind",
         coef_rename = c("Post-Attack"),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         gof_map = obs_gof_map,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.3f",
         title = "Effect of Pirate Encounters on Grid Cell Shipping Activity. \\label{tab:cell-post-regression}",
         notes = list("The unit of observation is a grid cell-day. Estimates are from Eq. (1). Each panel examines a different shipping activity measure. Each column represents a different geographic region. Post-Attack is a binary indicator equal to 1 for days within the 7-day window following a pirate attack in the grid cell. The sample spans from 2012 to 2023. All regressions include grid-cell-by-month and date fixed effects. Standard errors are Conley standard errors (50 km cutoff) and reported in parentheses."),
         threeparttable = TRUE,
         escape = FALSE,
         output = reg_table_name)

adjust_notes_font_size(reg_table_name)

# --- Build specification build-up table ---
# Shows how the Post-Attack coefficient evolves as we add more FEs. Column (1)
# uses cell FEs only; column (2) adds date FEs (full set of time FEs); column
# (3) is the main-text spec (cell-by-month + date FEs), pulled from `models`.
# Only the Global, 0.5 deg specification is shown.
build_panel <- function(outcome) {
  list(
    "(1)" = spec_tables |>
      filter(outcome_var == outcome, spec == "~ post | id") |>
      pull(model) |> pluck(1),
    "(2)" = spec_tables |>
      filter(outcome_var == outcome, spec == "~ post | id + date") |>
      pull(model) |> pluck(1),
    "(3)" = models |>
      filter(outcome_var == outcome, hotspot == "Global", res == "0_5") |>
      pull(model) |> pluck(1)
  )
}

msummary(models = list("Panel (A): Occupancy Time (hours)"                   = build_panel("asinh(time_hours)"),
                       "Panel (B): Distance Traveled (km)"                   = build_panel("asinh(distance_km)"),
                       "Panel (C): Transit (# Vessels)"                      = build_panel("asinh(n_vessels)"),
                       "Panel (D): Transit (# Voyages)"                      = build_panel("asinh(n_trips)"),
                       "Panel (E): Occupancy per Vessel (hours / vessel)"    = build_panel("asinh(time_vessels)"),
                       "Panel (F): Distance Traveled per Vessel (km / vessel)" = build_panel("asinh(dist_vessels)")),
         shape = "rbind",
         coef_rename = c("Post-Attack"),
         gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
         gof_map = obs_gof_map,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         fmt = "%.3f",
         title = "Specification build-up: effect of pirate encounters on grid cell shipping activity at 0.5 deg, Global sample. \\label{tab:cell-post-regression-spec-buildup}",
         notes = list("The unit of observation is a grid cell-day at 0.5 deg resolution over 2012--2023 in the Global sample. Each panel examines a different shipping activity outcome. Column (1) includes grid-cell fixed effects; column (2) adds date fixed effects (a full set of time fixed effects); column (3) is the main-text specification with grid-cell-by-month and date fixed effects. Post-Attack is a binary indicator equal to 1 for days within the 7-day window following a pirate attack in the grid cell. Standard errors are Conley standard errors (50 km cutoff) and reported in parentheses."),
         threeparttable = TRUE,
         escape = FALSE,
         output = spec_buildup_table_name)

adjust_notes_font_size(spec_buildup_table_name)

# --- Build event study plots ---
create_event_study_plot <- function(outcome_var, res, title, y_label = "Estimate ± (std.error & 95% CI)") {
  data <- switch(res, "0_1" = panel_0_1, "0_5" = panel_0_5, "1" = panel_1)
  model <- feols(
    as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id^month + date")),
    data = data,
    vcov = conley(cutoff = 50)
  )

  n_label <- paste0("N = ", format(nobs(model), big.mark = ",", scientific = FALSE))

  coeff_data <- broom::tidy(model) %>%
    filter(str_detect(term, "relative_time::")) %>%
    mutate(
      relative_time = as.integer(str_extract(term, "-?\\d+")),
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error
    ) %>%
    arrange(relative_time)

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
    annotate("text",
             x = -Inf, y = -Inf, label = n_label,
             hjust = -0.1, vjust = -0.7,
             size = 2.8, color = "gray30") +
    labs(
      title = title,
      x = "Days Relative to Pirate Event",
      y = y_label
    ) +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = seq(-7, 7, 2))
}

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

combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(
  filename = event_study_figure_name,
  plot = combined_plot,
  width = 9,
  height = 9,
  dpi = 300
)

# --- Event study plots by resolution (supplementary) ---
# Tidy each fit on the fly and keep just the `post` coefficient row. Because
# each fit carries a pre-baked Conley vcov, broom::tidy surfaces the correct SE.
res_plot <- models |>
  select(outcome_var, hotspot, spec, res, model) |>
  mutate(tidied = map(model, broom::tidy, conf.int = TRUE)) |>
  select(-model) |>
  unnest(tidied) |>
  filter(term == "post") |>
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
       filename = resolution_figure_name,
       width = 11,
       height = 10)

# --- Event study plots across resolutions (supplementary) ---
create_multi_event_study_plot <- function(outcome_var, title, y_label = "Estimate ± (std.error & 95% CI)") {
  fml <- as.formula(paste("asinh(", outcome_var, ") ~ i(relative_time, ref = -1) | id^month + date"))
  model_0_1 <- feols(fml, data = panel_0_1, vcov = conley(cutoff = 50))
  model_0_5 <- feols(fml, data = panel_0_5, vcov = conley(cutoff = 50))
  model_1   <- feols(fml, data = panel_1,   vcov = conley(cutoff = 50))

  models <- list("0.1°" = model_0_1,
                 "0.5°" = model_0_5,
                 "1°" = model_1)

  n_label <- paste(
    sprintf("%s: N = %s",
            names(models),
            format(vapply(models, nobs, numeric(1)), big.mark = ",", scientific = FALSE)),
    collapse = "\n"
  )

  coeff_data <- map_dfr(models, broom::tidy, .id = "res") |>
    filter(str_detect(term, "relative_time::")) %>%
    mutate(
      relative_time = as.integer(str_extract(term, "-?\\d+")),
      ci_low = estimate - 1.96 * std.error,
      ci_high = estimate + 1.96 * std.error
    ) %>%
    arrange(relative_time)

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
    annotate("text",
             x = -Inf, y = -Inf, label = n_label,
             hjust = -0.1, vjust = -0.15,
             size = 2.5, color = "gray30",
             lineheight = 0.9) +
    scale_color_manual(values = c("steelblue","cadetblue", "lightblue")) +
    labs(
      title = title,
      x = "Days Relative to Pirat Event",
      y = y_label,
      color = "Resolution"
    ) +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = seq(-7, 7, 2))
}

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

s_combined_plot <- (sp1 + sp2) / (sp3 + sp4) / (sp5 + sp6) +
  plot_layout(guides = "collect") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(
  filename = supplementary_event_study_figure_name,
  plot = s_combined_plot,
  width = 10,
  height = 10,
  dpi = 300
)

# --- AIS disabling table ---
AIS_disab <- create_event_study_plot("n_ais_disabling",
                                     res = "0_5",
                                     title = "# AIS disabling events")
modelsummary(AIS_disab_models$model,
             coef_rename = c("Post"),
             gof_omit = "R2|AIC|BIC|Log.|RMSE|FE|Std.Errors",
             gof_map = obs_gof_map,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             fmt = "%.3f",
             title = "Effect of Pirate Encounters on AIS Disabling Events. \\label{tab:ais-disabling}",
             notes = list("This table tests whether vessels disable their AIS transponders following a piracy report. The unit of observation is a grid cell-day. Each column represents a different geographic region. The Southeast Asia hotspot is excluded because there were no disabling events detected within pixels with pirate activity. Post is a binary indicator equal to 1 for days on or after a pirate event in the grid cell. The analysis uses a 7-day window around events to identify pre- and post-encounter periods. The sample spans from 2012 to 2023. All regressions include grid-cell-by-month and date fixed effects. Standard errors are Conley standard errors (50 km cutoff) and reported in parentheses."),
             threeparttable = TRUE,
             escape = FALSE,
             output = AIS_disab_table_name)

adjust_notes_font_size(AIS_disab_table_name)
