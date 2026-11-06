rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
for (d in c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")) {
  h <- rc_map[[paste0("Ha_", d)]]
  k <- rc_map[[paste0("Kg_", d)]]
  cat(d, "rows with Ha > 0 but Kg == 0:", sum(h > 0 & k == 0, na.rm = TRUE), "\n")
  cat(d, "rows with Ha > 0 and Kg > 0:", sum(h > 0 & k > 0, na.rm = TRUE), "\n")
}
