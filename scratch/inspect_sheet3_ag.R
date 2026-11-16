# Check column AG (33) in sheet3.xml
out_dir <- "scratch/xlsx_unzip"
sh3_file <- file.path(out_dir, "xl", "worksheets", "sheet3.xml")
sh3_str <- paste(readLines(sh3_file, warn = FALSE), collapse = "")

for (r in c(5, 6, 7, 9, 10, 11, 13, 14, 15, 18, 20, 21, 22, 23, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 76)) {
  pat <- sprintf('<c r="AG%d"[^>]*>(.*?)</c>', r)
  m <- regmatches(sh3_str, regexec(pat, sh3_str))[[1]]
  if (length(m) >= 2) {
    cat(sprintf("Cell AG%02d: %s\n", r, m[2]))
  }
}
