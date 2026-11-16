# Print all formulas in column AG
out_dir <- "scratch/xlsx_unzip"
sh3_file <- file.path(out_dir, "xl", "worksheets", "sheet3.xml")
sh3_str <- paste(readLines(sh3_file, warn = FALSE), collapse = "")

for (r in 7:40) {
  pat <- sprintf('<c r="AG%d"[^>]*>(.*?)</c>', r)
  m <- regmatches(sh3_str, regexec(pat, sh3_str))[[1]]
  if (length(m) >= 2) {
    cat(sprintf("Row %02d: %s\n", r, m[2]))
  }
}
