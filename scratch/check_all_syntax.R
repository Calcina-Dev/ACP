r_files <- c(
  "D:/ACP/Actualizacion Semanal/app.R",
  "D:/ACP/Actualizacion Semanal RNA/app.R",
  "D:/ACP/Rutas de Cosecha/app_mapa.R",
  "D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R",
  "D:/ACP/Seguimiento de Proyectos/app.R"
)

cat("==================================================\n")
cat("SYNTAX INTEGRITY CHECK ON CORE PRODUCTION APPS\n")
cat("==================================================\n")
for (f in r_files) {
  if (file.exists(f)) {
    res <- tryCatch({
      p <- parse(f)
      sprintf("PASS (Valid syntax, %d top-level expressions)", length(p))
    }, error = function(e) {
      sprintf("FAIL (Syntax error: %s)", e$message)
    })
    cat(sprintf("%-55s : %s\n", sub("D:/ACP/", "", f), res))
  } else {
    cat(sprintf("%-55s : FILE NOT FOUND\n", sub("D:/ACP/", "", f)))
  }
}
