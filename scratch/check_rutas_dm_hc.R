suppressPackageStartupMessages(library(dplyr))

cat("=== RUTAS COSECHA MAPA ===\n")
if (file.exists("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")) {
  rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
  m12_rc <- rc %>% filter(as.character(Módulo) == "12")
  cat("M12 in rutas_cosecha_mapa:", nrow(m12_rc), "\n")
  if (nrow(m12_rc) > 0) {
    print(table(m12_rc$Campaña, m12_rc$Sector))
  }
}

cat("\n=== DIAS MADURACION CALCULADOS ===\n")
if (file.exists("D:/ACP/Actualizacion Semanal/dias_maduracion_calculados.rds")) {
  dm <- readRDS("D:/ACP/Actualizacion Semanal/dias_maduracion_calculados.rds")
  m12_dm <- dm %>% filter(as.character(Módulo) == "12")
  cat("M12 in dias_maduracion:", nrow(m12_dm), "\n")
  if (nrow(m12_dm) > 0) {
    print(table(m12_dm$Sector))
  }
}

cat("\n=== HISTORICO CONTEO ===\n")
if (file.exists("D:/ACP/Actualizacion Semanal/historico_conteo.rds")) {
  hc <- readRDS("D:/ACP/Actualizacion Semanal/historico_conteo.rds")
  m12_hc <- hc %>% filter(as.character(Módulo) == "12")
  cat("M12 in historico_conteo:", nrow(m12_hc), "\n")
  if (nrow(m12_hc) > 0) {
    print(table(m12_hc$Sector))
  }
}
