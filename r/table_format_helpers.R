# Table helpers
# Add adjust box around regressiont ables
add_adjust_box <- function(file, line_before = "\\begin{adjustbox}{width = .9\\textwidth}", line_after = "\\end{adjustbox}", before = "\\begin{threeparttable}", after = "\\end{threeparttable}") {
  lines <- readLines(file)
  after_line <- grep(after, lines, fixed = TRUE)
  before_line <- grep(before, lines, fixed = TRUE)
  lines <- c(lines[1:(before_line-1)], line_before, lines[(before_line):(after_line)], line_after, lines[(after_line+1):length(lines)])
  writeLines(lines, file)
}

# Format the summary table
processKBLoutput <- function(file_path) {
  
  # Read the content of the LaTeX file
  lines <- readLines(file_path, warn = FALSE)
  
  # Define the patterns for hotspots as they appear in the document, prepared for regex matching
  hotspots_patterns <- c(
    "\\\\hspace\\{1em\\}G\\. of Aden",
    "\\\\hspace\\{1em\\}Southeast Asia",
    "\\\\hspace\\{1em\\}G\\. of Guinea",
    "\\\\hspace\\{1em\\}Rest of the World"
  )
  
  # Process each line to remove the specified hotspot text
  modified_lines <- lines
  for (i in seq_along(modified_lines)) {
    for (pattern in hotspots_patterns) {
      # This ensures we're matching the exact string with regex, and replace it
      modified_lines[i] <- gsub(pattern, "\\\\hspace{1em}", modified_lines[i])
    }
  }
  
  # Write the modified lines back to the same file or a new file
  writeLines(modified_lines, file_path)
  
  cat("The LaTeX file has been successfully processed and saved.\n")
}
