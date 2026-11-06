library(dplyr)

cat("=== EXAMINING EDICIONES MANUALES ===\n")
for (p in c("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds",
            "D:/ACP/Actualizacion data maestra/ediciones_manuales_cosecha.rds",
            "D:/ACP/ediciones_manuales_cosecha.rds")) {
  if (file.exists(p)) {
    ed <- readRDS(p)
    cat("File:", p, "| Rows:", nrow(ed), "\n")
    m12_s8 <- ed %>% filter(as.character(Módulo) == "12", as.character(Sector) == "8")
    cat("  M12 with Sector 8:", nrow(m12_s8), "rows\n")
    if (nrow(m12_s8) > 0) {
      print(m12_s8 %>% select(Campaña, Semana, Sector, Módulo, Turno, Variedad, Campo, Valor, Tipo_Edicion))
    }
  }
}

cat("\n=== EXAMINING AJUSTE VALVULAS SEMANA ===\n")
for (p in c("D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds",
            "D:/ACP/ajuste_valvulas_semana.rds")) {
  if (file.exists(p)) {
    av <- readRDS(p)
    cat("File:", p, "| Rows:", nrow(av), "\n")
    m12_s8 <- av %>% filter(as.character(Módulo) == "12", as.character(Sector) == "8")
    cat("  M12 with Sector 8 in ajuste_valvulas:", nrow(m12_s8), "rows\n")
  }
}
