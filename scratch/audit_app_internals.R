suppressPackageStartupMessages({
  library(dplyr)
})

lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
cat("Total lines in Actualizacion Semanal/app.R:", length(lines), "\n")

# 1. Check for unhandled NAs or NaN or division by zero in calculations
div_zero <- grep("/\\s*(0|pmax\\(0,|Ha_|Área|Area|kg|val)", lines, value = TRUE)
cat("Division patterns count:", length(div_zero), "\n")

# 2. Check for missing error handling in network/file read operations
cat("\nFile read operations without tryCatch:\n")
read_lines_idx <- grep("readRDS|read_excel|load\\(", lines)
for (i in read_lines_idx) {
  # check if within 5 lines before there is a tryCatch
  has_try <- any(grepl("tryCatch", lines[max(1, i-6):i]))
  if (!has_try) {
    cat(sprintf("  Line %d: %s\n", i, trimws(lines[i])))
  }
}

# 3. Check for reactive loops or observers without ignoreInit or debounce
obs_events <- grep("observeEvent\\(", lines)
cat("\nTotal observeEvent calls:", length(obs_events), "\n")
obs_no_ignore <- grep("observeEvent\\([^,]+,\\s*\\{", lines)
cat("observeEvent without ignoreInit explicitly set (default is TRUE or FALSE depending on context):", length(obs_no_ignore), "\n")
