ruta_rc <- "D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds"
if (file.exists(ruta_rc)) {
  rc <- readRDS(ruta_rc)
  cat("rc class:", class(rc), "rows:", nrow(rc), "\n")
  cat("rc names:\n")
  print(names(rc))
  rc_s45 <- rc[rc$Semana == 45 & grepl("13", rc$Módulo) & rc$Turno == "2", ]
  print(rc_s45[, intersect(c("Num_Pasada", "codigo", "Módulo", "Turno", "Variedad", "Ha_Lunes", "Ha_Jueves", "Kg_Lunes", "Kg_Jueves"), names(rc_s45))])
}
