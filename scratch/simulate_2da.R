load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

# Load existing functions from app.R or recreate the relevant parts
limpiar_variedad <- function(x) {
  toupper(trimws(as.character(x)))
}

desplazar_dia_2da_pasada <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(idx_base == 1L, ifelse(!is.na(frec_int) & frec_int <= 3L, 3L, 4L), 3L)
  idx_target <- pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_base + desp))
  dia_target <- dias_nombres[idx_target]
  dia_res <- ifelse(is.na(dia_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
  idx_res <- match(dia_res, dias_nombres)
  ifelse(!is.na(idx_res) & idx_res > dia_fin_idx, dia_fin_nom, dia_res)
}

# Load Escenario 4
esc <- readRDS("Actualizacion Semanal/escenarios_ajuste_semanal.rds")
e4 <- esc[4, ]
vdata <- e4$Valvulas_Data[[1]]

cat("Testing current Escenario 4 for Turno 1:\n")
print(vdata %>% filter(as.character(Módulo) == "12", as.character(Turno) == "1") %>% select(Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada))

cat("\nTesting current Escenario 4 for Turno 5:\n")
print(vdata %>% filter(as.character(Módulo) == "12", as.character(Turno) == "5") %>% select(Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada))
