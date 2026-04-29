# Copy canonical tables and produce _rev variants with swapped LaTeX labels for the revision manuscript

library(here)

output_dir <- here("results", "figures_and_tables")

# --- Helper ---

make_rev <- function(source_file, rev_file, old_label, new_label) {
  lines <- readLines(here(output_dir, source_file), warn = FALSE)
  lines <- gsub(old_label, new_label, lines, fixed = TRUE)
  writeLines(lines, here(output_dir, rev_file))
  cat("  ", rev_file, "\n")
}

# --- Generate revision tables ---

cat("Generating revision table variants...\n")

make_rev("spec_distance.tex", "spec_distance_rev.tex", "tab:spec-dist-table",   "tab:spec-dist-table-rev")
make_rev("spec_time.tex",     "spec_time_rev.tex",     "tab:spec-time-table",   "tab:spec-time-table-rev")
make_rev("spec_speed.tex",    "spec_speed_rev.tex",    "tab:spec-speed-table",  "tab:spec-speed-table-rev")
make_rev("spec_fuel.tex",     "spec_fuel_rev.tex",     "tab:spec-fuel-table",   "tab:spec-fuel-table-rev")
make_rev("spec_labor.tex",    "spec_labor_rev.tex",    "tab:spec-labor-table",  "tab:spec-labor-table-rev")
make_rev("spec_total.tex",    "spec_total_rev.tex",    "tab:spec-total-table",  "tab:spec-total-table-rev")
make_rev("spec_co2.tex",      "spec_co2_rev.tex",      "tab:spec-co2-table",    "tab:spec-co2-table-rev")
make_rev("spec_nox.tex",      "spec_nox_rev.tex",      "tab:spec-nox-table",    "tab:spec-nox-table-rev")
make_rev("spec_sox.tex",      "spec_sox_rev.tex",      "tab:spec-sox-table",    "tab:spec-sox-table-rev")

make_rev("attack_persistence.tex",                  "attack_persistence_rev.tex",                  "tab:attack-persistence",    "tab:attack-persistence-rev")
make_rev("trip_count.tex",                          "trip_count_rev1.tex",                          "tab:trip-count",            "tab:trip-count-rev1")
make_rev("trip_count.tex",                          "trip_count_rev2.tex",                          "tab:trip-count",            "tab:trip-count-rev2")
make_rev("suez_cape_route_choice_trip_level.tex",   "suez_cape_route_choice_trip_level_rev1.tex",   "tab:suez-cape-trip-level",  "tab:suez-cape-trip-level-rev1")
make_rev("suez_cape_route_choice_trip_level.tex",   "suez_cape_route_choice_trip_level_rev2.tex",   "tab:suez-cape-trip-level",  "tab:suez-cape-trip-level-rev2")
make_rev("grid_level_did_multiplegt_dyn_summary.tex", "grid_level_did_multiplegt_dyn_summary_rev.tex", "tab:grid-level-did-multiplegt-dyn-summary", "tab:grid-level-did-multiplegt-dyn-summary-rev")

cat("Done. Generated", 14, "revision tables.\n")
