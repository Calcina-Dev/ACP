suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
dirs_form <- file.path(base_dir, "Proyección Semanal Formalizada")
files_form <- list.files(dirs_form, pattern = "\\.xlsx$", full.names = TRUE)

cat("Files in Proyección Semanal Formalizada:\n")
for (f in files_form) {
  cat("  ", basename(f), "\n")
}

dirs_det <- file.path(base_dir, "Proyección Semanal Detallada")
files_det <- list.files(dirs_det, pattern = "\\.xlsx$", full.names = TRUE)
files_det <- files_det[!grepl("^~", basename(files_det))]

cat("\nFiles in Proyección Semanal Detallada:\n")
for (f in files_det) {
  cat("  ", basename(f), "\n")
}
