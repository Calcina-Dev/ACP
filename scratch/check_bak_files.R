# Check all backup files (.bak, .bak_..., _backup)
all_files <- list.files("D:/ACP", recursive = TRUE, full.names = TRUE)
bak_files <- all_files[grepl("bak|backup|copia|~$", all_files, ignore.case = TRUE)]
cat("Total backup/temporary files found:", length(bak_files), "\n")
cat("Sample backup files:\n")
for (b in head(bak_files, 20)) {
  cat(sprintf("  %s (%.2f MB)\n", sub("D:/ACP/", "", b), file.size(b)/(1024*1024)))
}
