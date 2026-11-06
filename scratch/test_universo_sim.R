suppressMessages({
  library(dplyr)
  library(tidyr)
})

# Load data
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

# Helper function
norm_codigo_valvula <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
}

sem_sel <- 45
dia_inicio_nom <- "Viernes"
dia_fin_nom <- "Viernes"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dia_fin_idx <- match(dia_fin_nom, dias_nombres)

dias_previos_nom <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)
cols_ha_prev <- paste0("Ha_", dias_previos_nom)
cols_kg_prev <- paste0("Kg_", dias_previos_nom)

rc_sem <- rc_map %>% filter(as.numeric(Semana) == sem_sel)

# Current logic
cols_ha_rc_exist <- intersect(cols_ha_prev, names(rc_sem))
t3_rc <- rc_sem %>% filter(Módulo == "2", Turno == "3")
cat("t3 in rc_sem:\n")
print(t3_rc %>% select(codigo, all_of(cols_ha_rc_exist)))

t3_sap <- hist_wide %>% filter(as.numeric(Semana) == sem_sel, Codigo_Valv %in% t3_rc$codigo)
cat("\nt3 in hist_wide:\n")
print(t3_sap %>% select(Codigo_Valv, any_of(c(cols_ha_prev, cols_kg_prev))))
