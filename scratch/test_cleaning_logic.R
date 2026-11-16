suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc <- df_esc %>% filter(Nombre == "Escenario Martes a Viernes (09-Nov. 14:37)")
ed_filtradas <- esc$Ediciones_Data[[1]]

# Mock universo_base for Modulo 13
dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

# Test cleaning logic
d_desde_sel <- "Martes"
d_hasta_sel <- "Viernes"
dias_con_ed_sem <- unique(as.character(ed_filtradas$Campo[coalesce(ed_filtradas$Valor, 0) > 0 & ed_filtradas$Campo %in% dias_validos]))

idx_ini <- match(d_desde_sel, dias_validos)
idx_fin <- match(d_hasta_sel, dias_validos)
if (length(dias_con_ed_sem) > 0) {
  idx_ini <- min(c(idx_ini, match(dias_con_ed_sem, dias_validos)), na.rm = TRUE)
  idx_fin <- max(c(idx_fin, match(dias_con_ed_sem, dias_validos)), na.rm = TRUE)
}
dias_escenario_limpiar <- dias_validos[idx_ini:6]

cat("Días a limpiar para modo ajustado:", paste(dias_escenario_limpiar, collapse = ", "), "\n")
cat("Días previos que conservan Real SAP:", paste(dias_validos[1:(idx_ini - 1)], collapse = ", "), "\n")
