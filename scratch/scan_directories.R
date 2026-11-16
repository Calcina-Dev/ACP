dirs <- list.dirs("D:/ACP", recursive = FALSE)
for (d in dirs) {
  cat("\n======================================================\n")
  cat("DIRECTORY:", basename(d), "\n")
  cat("======================================================\n")
  files <- list.files(d, full.names = FALSE)
  info <- file.info(file.path(d, files))
  df <- data.frame(
    File = files,
    Size_MB = round(info$size / (1024 * 1024), 2),
    IsDir = info$isdir,
    Modified = info$mtime,
    stringsAsFactors = FALSE
  )
  print(head(df[order(!df$IsDir, df$File), ], 25), row.names = FALSE)
}
