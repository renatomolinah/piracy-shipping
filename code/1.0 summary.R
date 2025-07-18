# =============================================================================
# VOYAGE DATA SUMMARY STATISTICS SCRIPT
# =============================================================================
# This script creates summary statistics tables for voyage data by piracy hotspot regions
# and exports them in LaTeX format for academic papers.
# =============================================================================

library(tidyverse)
library(ggpubr)
library(fixest)
library(modelsummary)
library(kableExtra)
library(scales)

# =============================================================================
# 1. LOAD AND PREPARE VOYAGE DATA
# =============================================================================

voyage_data_path <- here("piracy-data", "processed", "voyages.rds")

if (!file.exists(voyage_data_path)) {
  stop("Voyage data file not found: ", voyage_data_path)
}

voyage_data <- readRDS(voyage_data_path) %>%
  mutate(
    in_piracy_hotspot = gulf_of_guinea_hotspot + gulf_of_aden_hotspot + southeast_asia_hotspot
  ) %>%
  filter(
    in_piracy_hotspot <= 1, best_vessel_type_cargo
  ) %>%
  mutate(
    hotspot_region = case_when(
      gulf_of_guinea_hotspot == 1 ~ "Gulf of Guinea",
      gulf_of_aden_hotspot == 1 ~ "Gulf of Aden", 
      southeast_asia_hotspot == 1 ~ "Southeast Asia",
      TRUE ~ "Rest of the World"
    ),
    hotspot_region = fct_relevel(
      hotspot_region, 
      "Gulf of Aden", 
      "Gulf of Guinea", 
      "Southeast Asia", 
      "Rest of the World"
    )
  )

voyage_data <- voyage_data %>%
  mutate(
    recent_attacks = number_previous_attacks_3_months_5_degrees
  )

cat("Loaded", nrow(voyage_data), "voyage records for summary statistics\n")

# =============================================================================
# 2. CREATE SUMMARY STATISTICS BY REGION
# =============================================================================

summary_by_region <- datasummary(
  hotspot_region * (Mean + SD + Min + Max) ~ distance_km + time_hours + speed_kmh + recent_attacks,
  data = voyage_data,
  output = "dataframe"
)

summary_by_region <- summary_by_region %>%
  mutate(
    across(c(distance_km, time_hours, speed_kmh, recent_attacks), as.numeric)
  )

summary_by_region <- summary_by_region %>%
  mutate(
    across(c(distance_km, time_hours, speed_kmh, recent_attacks), 
           ~scales::comma(., accuracy = 0.1))
  )

# =============================================================================
# 3. EXPORT TO LATEX FORMAT
# =============================================================================

output_dir <- here("tables")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

latex_table <- kbl(
  x = summary_by_region,
  booktabs = TRUE,
  label = "summary_statistics",
  caption = "Summary Statistics for Voyages by Piracy Hotspot Region",
  col.names = c("", "", "Distance (km)", "Time (hr)", "Speed (km/hr)", "Recent Attacks (#/3 mo)"),
  align = c("l", "l", "r", "r", "r", "r"), 
  format = "latex"
) %>%
  kable_styling() %>%
  pack_rows("Gulf of Aden", 1, 4) %>% 
  pack_rows("Gulf of Guinea", 5, 8) %>% 
  pack_rows("Southeast Asia", 9, 12) %>% 
  pack_rows("Rest of the World", 13, 16)

output_file <- here("piracy-data", "figures and tables", "summary_statistics.tex")
cat(latex_table, file = output_file)

cat("LaTeX table saved to:", output_file, "\n")

# =============================================================================
# 4. POST-PROCESS LATEX OUTPUT
# =============================================================================

clean_latex_output <- function(file_path) {
  
  lines <- readLines(file_path, warn = FALSE)
  
  cleanup_patterns <- c(
    "\\\\hspace\\{1em\\}G\\. of Aden" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}G\\. of Guinea" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}Southeast Asia" = "\\\\hspace{1em}",
    "\\\\hspace\\{1em\\}Rest of the World" = "\\\\hspace{1em}"
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

# =============================================================================
# 5. PRINT SUMMARY INFORMATION
# =============================================================================

cat("\n=== SUMMARY STATISTICS COMPLETED ===\n")
cat("Total voyages analyzed:", nrow(voyage_data), "\n")
cat("Voyages by region:\n")
print(table(voyage_data$hotspot_region))
cat("\nOutput file:", output_file, "\n")
