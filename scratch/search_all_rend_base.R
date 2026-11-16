files <- list.files("D:/ACP", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
matches <- list()
for (f in files) {
  if (grepl("scratch|bak", f)) next
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  hit <- grep("Rend_Base_Con_Floral", lines)
  if (length(hit) > 0) {
    cat(sprintf("%s (lines: %s)\n", f, paste(hit, collapse = ", ")))
  }
}
