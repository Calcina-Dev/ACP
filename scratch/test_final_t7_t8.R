suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")

limpiar_variedad <- function(x) {
  v <- toupper(trimws(as.character(x)))
  gsub("\\s+", " ", v)
}
norm_codigo_valvula <- function(x) {
  x <- toupper(trimws(as.character(x)))
  gsub("\\s+", "", x)
}

sem_sel <- 44
sem_ant <- 43
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dias_ajustables_nom <- c("Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_nom <- "Martes"
dia_inicio_idx <- 2

desplazar_dia_frecuencia <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom) {
  idx_base <- match(dia_base_vec, dias_nombres)
  idx_target <- pmin(6L, pmax(1L, idx_base + as.integer(frec_vec) - 7L))
  dia_target <- ifelse(is.na(idx_target), dia_inicio_nom, dias_nombres[idx_target])
  ifelse(is.na(idx_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
}

# Ejecutar lógica idéntica a app.R
source("d:/ACP/scratch/test_full_engine_integration.R")

cat("\n=======================================================\n")
cat("RESULTADO FINAL PARA TURNO 7 Y TURNO 8 EN SEMANA 44:\n")
cat("=======================================================\n")

# Módulo 4 Turno 7
t7_ed <- auto_ediciones %>% filter(Módulo == "4", Turno == "7")
cat("\nTURNO 7 (MAGICA, 6.47 ha total):\n")
cat("  - Lunes (Real SAP): 2,059.16 kg (Cierre Remanente S43)\n")
for(i in seq_len(nrow(t7_ed))) {
  cat(sprintf("  - %s (Auto): %s kg\n", t7_ed$Campo[i], format(round(t7_ed$Valor[i], 2), big.mark = ",")))
}
cat(sprintf("  Total Semana T7 = %s kg (Proyección Oficial = 7,582.94 kg)\n", 
            format(round(2059.16 + sum(t7_ed$Valor), 2), big.mark = ",")))

# Módulo 4 Turno 8
t8_ed <- auto_ediciones %>% filter(Módulo == "4", Turno == "8")
cat("\nTURNO 8 (MAGICA, 5.52 ha total):\n")
cat("  - Lunes (Real SAP): 1,673.23 kg (Cierre Remanente S43)\n")
for(i in seq_len(nrow(t8_ed))) {
  cat(sprintf("  - %s (Auto): %s kg\n", t8_ed$Campo[i], format(round(t8_ed$Valor[i], 2), big.mark = ",")))
}
cat(sprintf("  Total Semana T8 = %s kg (Proyección Oficial = 5,898.87 kg)\n", 
            format(round(1673.23 + sum(t8_ed$Valor), 2), big.mark = ",")))
