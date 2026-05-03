# Shared LaTeX table post-processing helpers for modelsummary output

# Wrap threeparttable (or talltblr) inside adjustbox for width control
add_adjust_box <- function(file,
                           line_before = "\\begin{adjustbox}{width = .9\\textwidth}",
                           line_after = "\\end{adjustbox}",
                           before = "\\begin{threeparttable}",
                           after = "\\end{threeparttable}") {
  lines <- readLines(file)
  before_line <- grep(before, lines, fixed = TRUE)
  after_line <- grep(after, lines, fixed = TRUE)

  # Fall back to talltblr if threeparttable not found (modelsummary v2+)
  if (length(before_line) == 0 || length(after_line) == 0) {
    before_line <- grep("\\begin{talltblr}", lines, fixed = TRUE)
    after_line  <- grep("\\end{talltblr}", lines, fixed = TRUE)
  }

  if (length(before_line) == 0 || length(after_line) == 0) {
    warning(paste("add_adjust_box: no wrappable block found in", file, "— skipping."))
    return(invisible(NULL))
  }

  before_line <- before_line[1]
  after_line  <- after_line[length(after_line)]

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

# Remove the visual break that modelsummary inserts before goodness-of-fit rows
# in compact tables.
remove_midrule_before_observations <- function(file) {
  lines <- readLines(file)
  obs_line_index <- grep("^Observations\\s*&", lines)
  if (length(obs_line_index) > 0) {
    midrule_index <- obs_line_index[1] - 1
    if (midrule_index > 0 && identical(lines[midrule_index], "\\midrule")) {
      lines <- lines[-midrule_index]
    }
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

# Strip \textit{} and \textbf{} wrappers from \multicolumn panel headers
# (Nature Comms forbids bold/italic data formatting unless declared in notes)
unstyle_panel_headers <- function(file) {
  lines <- readLines(file)
  lines <- gsub("\\\\multicolumn(\\{[0-9]+\\})\\{([lcr])\\}\\{\\\\textit\\{(.*?)\\}\\}",
                "\\\\multicolumn\\1{\\2}{\\3}", lines)
  lines <- gsub("\\\\multicolumn(\\{[0-9]+\\})\\{([lcr])\\}\\{\\\\textbf\\{(.*?)\\}\\}",
                "\\\\multicolumn\\1{\\2}{\\3}", lines)
  writeLines(lines, file)
}

# Append the fixed-effect legend to the table's \item \scriptsize notes line.
# has_bullet = TRUE adds the bullet clause; FALSE adds only the X clause.
append_fe_legend <- function(file, has_bullet = TRUE) {
  lines <- readLines(file)
  legend <- if (has_bullet) {
    "X indicates the fixed effect is included; $\\bullet$ indicates the fixed effect is absorbed by the sample restriction."
  } else {
    "X indicates the fixed effect is included."
  }
  item_idx <- grep("^\\\\item.*\\\\scriptsize", lines)
  if (length(item_idx) == 0) return(invisible(NULL))
  for (i in item_idx) {
    if (!grepl("X indicates the fixed effect is included", lines[i], fixed = TRUE)) {
      lines[i] <- paste0(lines[i], " ", legend)
    }
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
