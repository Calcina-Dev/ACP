lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

# Search for how turns with no harvest in current week are projected/adjusted
# For example: pct_avance, ha_real, rend_s1, castigo floral, etc.
idx <- grep("Sin avance previo|Sin avance|rend_sem_ant|rend_s1|castigo floral|evaluacion_avance", lines, ignore.case = TRUE)
cat("Found", length(idx), "matching lines:\n")
for (i in head(idx, 30)) {
  cat(sprintf("%d: %s\n", i, trimws(lines[i])))
}
