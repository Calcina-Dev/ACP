suppressPackageStartupMessages({
  library(dplyr)
})

cat("=== 1. BUSCAR EN valvulas_normalizadas.rds ===\n")
if (file.exists("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")) {
  v <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
  print(v %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12") %>% select(Sector, Módulo, Turno, Variedad, Área) %>% distinct())
}

cat("\n=== 2. BUSCAR EN datos_maestros_cosecha_2026.RData ===\n")
if (file.exists("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")) {
  load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
  if (exists("df_valvulas")) {
    cat("df_valvulas:\n")
    print(df_valvulas %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12") %>% select(Sector, Módulo, Turno, Variedad, Área) %>% distinct())
  }
  if (exists("df_cosecha")) {
    cat("df_cosecha:\n")
    print(df_cosecha %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12") %>% select(Campaña, Sector, Módulo, Turno, Variedad) %>% distinct())
  }
}

cat("\n=== 3. BUSCAR EN ediciones_manuales_cosecha.rds ===\n")
ed_files <- c(
  "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds",
  "D:/ACP/Actualizacion data maestra/ediciones_manuales_cosecha.rds",
  "D:/ACP/ediciones_manuales_cosecha.rds"
)
for (ef in ed_files) {
  if (file.exists(ef)) {
    ed <- readRDS(ef)
    m12 <- ed %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12")
    cat(ef, "has", nrow(m12), "rows for M12 T12:\n")
    if (nrow(m12) > 0) {
      print(m12 %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Tipo_Edicion", "Dia_Edicion", "Kg_Manual", "Ha_Manual"))))
    }
  }
}

cat("\n=== 4. BUSCAR EN ajuste_valvulas_semana.rds ===\n")
aj_files <- c(
  "D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds",
  "D:/ACP/ajuste_valvulas_semana.rds"
)
for (af in aj_files) {
  if (file.exists(af)) {
    aj <- readRDS(af)
    m12 <- aj %>% filter(as.character(Módulo) == "12", as.character(Turno) == "12")
    cat(af, "has", nrow(m12), "rows for M12 T12:\n")
    if (nrow(m12) > 0) {
      print(m12 %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Tipo_Ajuste", "Kilos_Ajustados"))))
    }
  }
}
