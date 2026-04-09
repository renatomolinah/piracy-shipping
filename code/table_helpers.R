# =============================================================================
# Centralized LaTeX Table Helper Functions
# =============================================================================
# Source this file from any script that generates tables:
#   source(here("code", "table_helpers.R"))
# =============================================================================

# Wrap threeparttable inside adjustbox for width control
add_adjust_box <- function(file,
                           line_before = "\\begin{adjustbox}{width = .9\\textwidth}",
                           line_after = "\\end{adjustbox}",
                           before = "\\begin{threeparttable}",
                           after = "\\end{threeparttable}") {
  lines <- readLines(file)
  after_line <- grep(after, lines, fixed = TRUE)
  before_line <- grep(before, lines, fixed = TRUE)
  lines <- c(lines[1:(before_line-1)], line_before, lines[(before_line):(after_line)], line_after, lines[(after_line+1):length(lines)])
  writeLines(lines, file)
}

# Add \scriptsize to tablenotes \item lines
adjust_notes_font_size <- function(file, font_size_command = "\\scriptsize") {
  lines <- readLines(file)
  item_line_index <- grep("\\item", lines, fixed = TRUE)
  if (length(item_line_index) > 0) {
    lines[item_line_index] <- gsub("\\item", paste0("\\item ", font_size_command), lines[item_line_index], fixed = TRUE)
  }
  writeLines(lines, file)
}

# Replace numbered column headers (1), (2), ... with custom names
replace_table_headers <- function(file, new_headers) {
  lines <- readLines(file)
  header_line_idx <- which(grepl("& \\(", lines))
  if (length(header_line_idx) == 0) {
    stop("Header line not found")
  }
  header_line <- lines[header_line_idx]
  for (i in seq_along(new_headers)) {
    header_line <- sub(paste0("\\(", i, "\\)"), new_headers[i], header_line)
  }
  lines[header_line_idx] <- header_line
  writeLines(lines, file)
}

# Remove \usepackage and \newcolumntype declarations that modelsummary inserts
strip_pkg_declarations <- function(file) {
  lines <- readLines(file)
  lines <- lines[!grepl("^\\\\usepackage|^\\\\newcolumntype", lines)]
  writeLines(lines, file)
}

# Add observation rows after each sub-panel in grid-cell tables
add_rows_with_observations <- function(file, observations_df) {
  lines <- readLines(file)

  panel_order <- c("Panel (A): Total Time (hours)" = "asinh(time_hours)",
                   "Panel (B): Total Distance (km)" = "asinh(distance_km)",
                   "Panel (C): Vessels (#)" = "asinh(n_vessels)",
                   "Panel (D): Voyages (#)" = "asinh(n_trips)",
                   "Panel (E): Time per Vessel (hours / vessel)" = "asinh(time_vessels)",
                   "Panel (F): Distance per Vessel (km / vessel)" = "asinh(dist_vessels)")

  panel_headers <- grep("Panel \\([A-F]\\):", lines)

  for (i in seq_along(panel_headers)) {
    outcome_var <- panel_order[i]

    if (i < length(panel_headers)) {
      panel_end <- panel_headers[i + 1] - 1
    } else {
      bottomrule_line <- grep("\\\\bottomrule", lines)[1]
      panel_end <- bottomrule_line - 1
    }

    panel_obs <- observations_df[observations_df$outcome_var == outcome_var, ]
    obs_values <- panel_obs[, -1]
    obs_values_formatted <- sapply(obs_values, function(x) format(x, big.mark = ",", scientific = FALSE))
    obs_row <- paste("\\hspace{1em}Observations", paste(obs_values_formatted, collapse = " & "), "\\\\", sep = " & ")

    lines <- c(lines[1:panel_end], obs_row, lines[(panel_end + 1):length(lines)])
    panel_headers <- grep("Panel \\([A-F]\\):", lines)
  }

  writeLines(lines, file)
}

# Add threeparttable wrapper + note to a table that already has adjustbox
add_threeparttable_note <- function(file, note_text) {
  lines <- readLines(file, warn = FALSE)
  adj_open <- grep("\\\\begin\\{adjustbox\\}", lines)
  adj_close <- grep("\\\\end\\{adjustbox\\}", lines)
  lines <- c(lines[1:adj_open], "\\begin{threeparttable}", lines[(adj_open+1):length(lines)])
  adj_close <- grep("\\\\end\\{adjustbox\\}", lines)
  note_lines <- c("\\begin{tablenotes}",
                   paste0("\\item \\scriptsize ", note_text),
                   "\\end{tablenotes}",
                   "\\end{threeparttable}")
  lines <- c(lines[1:(adj_close-1)], note_lines, lines[adj_close:length(lines)])
  writeLines(lines, file)
}
