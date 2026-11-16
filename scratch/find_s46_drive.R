suppressMessages(library(dplyr))
ruta_drive_raiz <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano"

# List files in Campaña 26-27 or subdirectories
dirs <- list.dirs(ruta_drive_raiz, recursive = TRUE, full.names = TRUE)
proy_dirs <- dirs[grep("Proyecci", dirs, ignore.case = TRUE)]
cat("Proyeccion dirs found:\n")
print(proy_dirs)

# List all files for Semana 46 in those dirs
for (d in proy_dirs) {
  files <- list.files(d, pattern = "46", full.names = TRUE, ignore.case = TRUE)
  if (length(files) > 0) {
    cat("\nIn dir:", d, "\n")
    for (f in files) {
      cat("  File:", basename(f), "Size:", file.size(f), "Mtime:", format(file.mtime(f)), "\n")
    }
  }
}
