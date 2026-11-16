suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc <- df_esc %>% filter(Nombre == "Escenario Martes a Viernes (09-Nov. 14:37)")
ed_esc <- esc$Ediciones_Data[[1]]

dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
d_desde <- "Martes"
idx_desde <- match(d_desde, dias_semana_nombres)
dias_previos <- dias_semana_nombres[1:(idx_desde - 1)] # Lunes
dias_escenario <- dias_semana_nombres[idx_desde:length(dias_semana_nombres)] # Martes a Sábado

cat("Días previos (Real):", paste(dias_previos, collapse = ", "), "\n")
cat("Días escenario (Ajuste):", paste(dias_escenario, collapse = ", "), "\n")

# Let's inspect Turnos 1, 2, 6 of Modulo 13 in ed_esc
ed_m13 <- ed_esc %>% filter(Módulo == "13", Campo %in% dias_escenario)
print(ed_m13 %>% select(Turno, Variedad, Campo, Valor, Tipo_Edicion))
