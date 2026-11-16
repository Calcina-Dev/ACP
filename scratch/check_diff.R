lines_new <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
lines_old <- readLines("D:/ACP/Actualizacion Semanal/app.R.bak_desp3_20261111", warn = FALSE, encoding = "UTF-8")

diff_idx <- which(lines_new != lines_old)
cat("Lines that differ (first 10):", head(diff_idx, 10), "\n")
cat("Total lines differing:", length(diff_idx), "\n")
for (i in head(diff_idx, 10)) {
  cat("OLD [", i, "]: ", lines_old[i], "\n")
  cat("NEW [", i, "]: ", lines_new[i], "\n")
}
