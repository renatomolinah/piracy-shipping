# Summary statistics tables for voyage data by piracy hotspot region

# --- Setup ---
library(here)
library(tidyverse)
library(ggpubr)
library(fixest)
library(modelsummary)
library(kableExtra)
library(scales)

source(here("code", "table_helpers.R"))

# --- Load and prepare data ---
voyage_data <- readRDS(here("data", "processed", "voyages.rds")) %>%
  mutate(
    in_piracy_hotspot = guinea + aden + asia
  ) %>%
  filter(
    in_piracy_hotspot <= 1, best_vessel_type_cargo
  ) %>%
  mutate(
    hotspot_region = case_when(
      guinea == 1 ~ "Gulf of Guinea",
      aden == 1 ~ "Gulf of Aden",
      asia == 1 ~ "Southeast Asia",
      TRUE ~ "Rest of the World"
    ),
    hotspot_region = fct_relevel(
      hotspot_region,
      "Gulf of Aden",
      "Gulf of Guinea",
      "Southeast Asia",
      "Rest of the World"
    )
  )  %>%
  mutate(
    attacks_7d  = number_previous_attacks_7_days_5_degrees,
    attacks_15d = number_previous_attacks_15_days_5_degrees,
    attacks_30d = number_previous_attacks_1_month_5_degrees
  )

cat("Loaded", nrow(voyage_data), "voyage records for summary statistics\n")

# --- Summary statistics by region ---
P5 <- function(x) quantile(x, 0.05, na.rm = TRUE)
P95 <- function(x) quantile(x, 0.95, na.rm = TRUE)

summary_by_region <-
  datasummary(
    hotspot_region * (Mean + SD + P5 + P95) ~ distance + time + speed + attacks_7d + attacks_15d + attacks_30d,
    data = voyage_data,
    output = "dataframe") |>
  mutate(
    across(c(distance, time, speed, attacks_7d, attacks_15d, attacks_30d), as.numeric)
  ) |>
  mutate(
    across(c(distance, time, speed, attacks_7d, attacks_15d, attacks_30d),
           ~scales::comma(., accuracy = 0.1))
  )

# --- Export summary table ---
latex_table <- kbl(
  x = summary_by_region,
  booktabs = TRUE,
  label = "summary",
  caption = "Summary Statistics for Individual Voyage Features.",
  col.names = c("", "", "Distance (km)", "Time (hr)", "Speed (km/hr)", "7 days", "15 days", "30 days"),
  align = c("l", "l", "r", "r", "r", "r", "r", "r"),
  format = "latex"
) %>%
  kable_styling() %>%
  add_header_above(c(" " = 2, "Voyage Features" = 3, "Encounters (\\\\#)" = 3), escape = FALSE) %>%
  pack_rows("Gulf of Aden", 1, 4) %>%
  pack_rows("Gulf of Guinea", 5, 8) %>%
  pack_rows("Southeast Asia", 9, 12) %>%
  pack_rows("Rest of the World", 13, 16) %>%
  footnote(general = "The unit of observation is a voyage. The sample includes all cargo vessel voyages from 2012 to 2023 that pass through at most one piracy hotspot. Voyage features report total distance (km), total time (hr), and average speed (km/hr). Encounters report the count of pirate encounters recorded in the projected path of the vessel using a 5-degree spatial footprint over the preceding 7, 15, and 30 days, respectively. P5 and P95 denote the 5th and 95th percentiles, respectively.",
           general_title = "",
           escape = FALSE,
           threeparttable = TRUE)

output_file <- here("results", "figures_and_tables", "summary.tex")
cat(latex_table, file = output_file)
adjust_notes_font_size(output_file)
add_adjust_box(output_file)

cat("LaTeX table saved to:", output_file, "\n")

# --- Post-process LaTeX ---
clean_latex_output <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE)

  cleanup_patterns <- c(
    "\\\\hspace\\{1em\\}Gulf of Aden" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}Gulf of Guinea" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}Southeast Asia" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}Rest of the World" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}G\\. of Aden" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}G\\. of Guinea" = "\\\\hspace{1em}"
  )

  modified_lines <- lines
  for (i in seq_along(modified_lines)) {
    for (pattern in names(cleanup_patterns)) {
      modified_lines[i] <- gsub(pattern, cleanup_patterns[pattern], modified_lines[i])
    }
  }

  writeLines(modified_lines, file_path)
  cat("LaTeX file has been cleaned and saved.\n")
}

clean_latex_output(output_file)

if(require(beepr)){beep(4)}

output_dir <- here("results", "figures_and_tables")

# --- ASAM reporting lags ---
# How long between an attack and its ASAM database entry?
# Informs the 7-day window: captains can only respond to public info.
asam_sf <- sf::read_sf(here("data", "processed", "clean_asam_data.gpkg")) %>%
  rename(geometry = geom) %>%
  mutate(
    dateofocc = lubridate::with_tz(dateofocc, tzone = "UTC"),
    date = format(dateofocc, "%m/%d/%Y") %>% lubridate::mdy(),
    entry_date = as.Date(entrydate),
    lag_days = as.numeric(entry_date - date),
    lon = sf::st_coordinates(.)[, 1],
    lat = sf::st_coordinates(.)[, 2]
  )

asam <- asam_sf %>%
  sf::st_drop_geometry() %>%
  filter(
    date >= lubridate::ymd("2012-01-01"),
    date <= lubridate::ymd("2023-12-31"),
    lag_days >= 0
  )

# Persistence is about the temporal recurrence of attacks, so keep all
# in-range attacks regardless of whether the reporting lag is negative.
asam_persistence_base <- asam_sf %>%
  sf::st_drop_geometry() %>%
  filter(
    date >= lubridate::ymd("2012-01-01"),
    date <= lubridate::ymd("2023-12-31")
  )

# DBSCAN-derived bounding boxes (eps=10, MinPts=150), snapped to nearest 5 degrees
hotspot_boxes <- tibble(
  cluster = c("Gulf of Aden", "Gulf of Guinea", "Southeast Asia"),
  lon_min = c(35, -15, 95),
  lat_min = c(-5, -10, -15),
  lon_max = c(65, 15, 135),
  lat_max = c(25, 10, 25)
)

asam <- asam %>%
  mutate(
    hotspot_asam = case_when(
      lat >= hotspot_boxes$lat_min[hotspot_boxes$cluster == "Gulf of Aden"] &
        lat <= hotspot_boxes$lat_max[hotspot_boxes$cluster == "Gulf of Aden"] &
        lon >= hotspot_boxes$lon_min[hotspot_boxes$cluster == "Gulf of Aden"] &
        lon <= hotspot_boxes$lon_max[hotspot_boxes$cluster == "Gulf of Aden"] ~ "Gulf of Aden (box)",
      lat >= hotspot_boxes$lat_min[hotspot_boxes$cluster == "Gulf of Guinea"] &
        lat <= hotspot_boxes$lat_max[hotspot_boxes$cluster == "Gulf of Guinea"] &
        lon >= hotspot_boxes$lon_min[hotspot_boxes$cluster == "Gulf of Guinea"] &
        lon <= hotspot_boxes$lon_max[hotspot_boxes$cluster == "Gulf of Guinea"] ~ "Gulf of Guinea (box)",
      lat >= hotspot_boxes$lat_min[hotspot_boxes$cluster == "Southeast Asia"] &
        lat <= hotspot_boxes$lat_max[hotspot_boxes$cluster == "Southeast Asia"] &
        lon >= hotspot_boxes$lon_min[hotspot_boxes$cluster == "Southeast Asia"] &
        lon <= hotspot_boxes$lon_max[hotspot_boxes$cluster == "Southeast Asia"] ~ "Southeast Asia (box)",
      TRUE ~ "Other"
    )
  )

lag_summary <- bind_rows(
  asam %>%
    summarise(
      n = n(), mean_lag = mean(lag_days), median_lag = median(lag_days),
      p75 = quantile(lag_days, 0.75), p90 = quantile(lag_days, 0.90),
      p95 = quantile(lag_days, 0.95),
      share_le_7 = mean(lag_days <= 7), share_le_14 = mean(lag_days <= 14),
      share_le_30 = mean(lag_days <= 30)
    ) %>% mutate(sample = "Global"),
  asam %>%
    group_by(sample = hotspot_asam) %>%
    summarise(
      n = n(), mean_lag = mean(lag_days), median_lag = median(lag_days),
      p75 = quantile(lag_days, 0.75), p90 = quantile(lag_days, 0.90),
      p95 = quantile(lag_days, 0.95),
      share_le_7 = mean(lag_days <= 7), share_le_14 = mean(lag_days <= 14),
      share_le_30 = mean(lag_days <= 30),
      .groups = "drop"
    )
)

write_csv(lag_summary, here(output_dir, "asam_reporting_lags_summary.csv"))
print(lag_summary %>% select(sample, n, median_lag, share_le_7, share_le_14, share_le_30))

lag_for_tex <- lag_summary %>%
  filter(sample != "Other") %>%
  mutate(
    sample = case_when(
      sample == "Gulf of Aden (box)" ~ "Gulf of Aden",
      sample == "Gulf of Guinea (box)" ~ "Gulf of Guinea",
      sample == "Southeast Asia (box)" ~ "Southeast Asia",
      TRUE ~ sample
    )
  ) %>%
  arrange(factor(sample, levels = c("Global", "Gulf of Aden", "Gulf of Guinea", "Southeast Asia")))

lag_tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{ASAM Reporting Lags: Days Between Occurrence and Database Entry (2012--2023).}",
  "\\label{tab:asam-reporting-lags}",
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  " & & \\multicolumn{2}{c}{Lag (days)} & \\multicolumn{3}{c}{Share entered within} \\\\",
  "\\cmidrule(lr){3-4} \\cmidrule(lr){5-7}",
  "Sample & N & Mean & Median & 7 days & 14 days & 30 days \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(lag_for_tex))) {
  r <- lag_for_tex[i, ]
  lag_tex_lines <- c(lag_tex_lines, sprintf(
    "%s & %s & %.1f & %d & %.1f\\%% & %.1f\\%% & %.1f\\%% \\\\",
    r$sample, format(r$n, big.mark = ","), r$mean_lag, as.integer(r$median_lag),
    r$share_le_7 * 100, r$share_le_14 * 100, r$share_le_30 * 100
  ))
}

lag_tex_lines <- c(lag_tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\item \\scriptsize Each row reports the distribution of reporting lags (in days) between the recorded date of occurrence and the ASAM database entry date. The share columns indicate the fraction of encounters publicly available within the given window. Sample restricted to 2012--2023. Hotspot assignment uses the bounding boxes described in the main text.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(lag_tex_lines, here(output_dir, "asam_reporting_lags.tex"))

# --- Attack persistence ---
# P(another attack in same 0.5x0.5 degree cell within k days)
asam_persistence <- asam_persistence_base %>%
  mutate(
    cell_lon = floor(lon * 2) / 2,
    cell_lat = floor(lat * 2) / 2
  )

compute_persistence <- function(data, k_values = c(1, 7, 15, 30), sample_label = "Global") {
  results <- tibble()
  for (k in k_values) {
    has_reattack <- data %>%
      group_by(cell_lon, cell_lat) %>%
      arrange(date) %>%
      mutate(
        days_to_next = as.numeric(lead(date) - date),
        # The final attack in a cell is a valid non-reattack observation,
        # not missing data, for the persistence probability.
        reattack = if_else(is.na(days_to_next), FALSE, days_to_next <= k)
      ) %>%
      ungroup() %>%
      summarise(
        n = n(),
        p_reattack = mean(reattack)
      )

    results <- bind_rows(results, tibble(
      n = has_reattack$n,
      p_reattack = round(has_reattack$p_reattack, 3),
      k_days = k,
      sample = sample_label
    ))
  }
  results
}

persistence_summary <- bind_rows(
  compute_persistence(asam_persistence, sample_label = "Global"),
  compute_persistence(asam_persistence %>% filter(hotspot_asam == "Gulf of Aden (box)"), sample_label = "Gulf of Aden"),
  compute_persistence(asam_persistence %>% filter(hotspot_asam == "Gulf of Guinea (box)"), sample_label = "Gulf of Guinea"),
  compute_persistence(asam_persistence %>% filter(hotspot_asam == "Other"), sample_label = "Other"),
  compute_persistence(asam_persistence %>% filter(hotspot_asam == "Southeast Asia (box)"), sample_label = "Southeast Asia")
)

persistence_wide <- persistence_summary %>%
  select(sample, k_days, p_reattack) %>%
  pivot_wider(names_from = sample, values_from = p_reattack)
print(persistence_wide)

persist_for_tex <- persistence_summary %>%
  filter(sample != "Other") %>%
  mutate(
    sample = factor(sample, levels = c("Global", "Gulf of Aden", "Gulf of Guinea", "Southeast Asia")),
    window_label = paste0("Within ", k_days, ifelse(k_days == 1, " day", " days"))
  ) %>%
  select(window_label, sample, p_reattack, k_days) %>%
  pivot_wider(names_from = sample, values_from = p_reattack) %>%
  arrange(k_days)

persist_tex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Spatial Persistence of Pirate Encounters.}",
  "\\label{tab:attack-persistence}",
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "Window & Global & Gulf of Aden & Gulf of Guinea & Southeast Asia \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(persist_for_tex))) {
  r <- persist_for_tex[i, ]
  persist_tex_lines <- c(persist_tex_lines, sprintf(
    "%s & %.1f\\%% & %.1f\\%% & %.1f\\%% & %.1f\\%% \\\\",
    r$window_label,
    r$Global * 100, r$`Gulf of Aden` * 100,
    r$`Gulf of Guinea` * 100, r$`Southeast Asia` * 100
  ))
}

persist_tex_lines <- c(persist_tex_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  sprintf("\\item \\\\scriptsize Each cell reports the probability that, conditional on an attack occurring in a $0.5^\\\\circ \\\\times 0.5^\\\\circ$ grid cell, at least one additional attack occurs in the same cell within the specified time window. Sample: %s encounters from 2012--2023. Hotspot assignment uses the bounding boxes described in the main text.", scales::comma(nrow(asam_persistence))),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(persist_tex_lines, here(output_dir, "attack_persistence.tex"))
