paths <- c(
  "Actualizacion Semanal/datos_maestros_cosecha_2026.RData",
  "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData"
)

for (p in paths) {
  cat("Path:", p, "\n")
  cat("  Exists:", file.exists(p), "\n")
  if (file.exists(p)) {
    cat("  Size:", file.size(p), "Mtime:", format(file.mtime(p)), "\n")
  }
}
