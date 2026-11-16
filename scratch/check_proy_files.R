# Check manifest and Excel files
suppressMessages(library(dplyr))
rdata_path <- "Actualizacion Semanal/datos_maestros_cosecha_2026.RData"
e <- new.env()
load(rdata_path, envir = e)

if ("df_manifest_proy" %in% ls(e)) {
  cat("--- df_manifest_proy names & head ---\n")
  print(names(e$df_manifest_proy))
  print(head(e$df_manifest_proy))
}

if ("metadata_actualizacion" %in% ls(e)) {
  cat("\n--- metadata_actualizacion ---\n")
  print(e$metadata_actualizacion)
}

ruta_raiz <- "G:/.shortcut-targets-by-id/16xQsmkQ1cbL8vd_Xf_o619r0T7zpPcv-/02_OPERACIONES/08_PROGRAMA SEMANAL COSECHA/ARÁNDANO"
cat("\nChecking G: drive path:", dir.exists(ruta_raiz), "\n")
if (dir.exists(ruta_raiz)) {
  camp_dirs <- list.dirs(ruta_raiz, recursive = FALSE)
  print(camp_dirs)
}
