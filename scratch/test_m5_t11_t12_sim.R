suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

# Load app.R environment or run the calculation for M5 T11 and T12
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

norm_codigo_valvula <- function(x) toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
limpiar_variedad <- function(x) toupper(trimws(as.character(x)))

sem_sel <- 45
sem_ant <- 44
dia_inicio_nom <- "Jueves"
dia_fin_nom <- "Sábado"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dia_fin_idx <- match(dia_fin_nom, dias_nombres)
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
dias_previos_nom <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)
cols_ha_prev <- paste0("Ha_", dias_previos_nom)
cols_kg_prev <- paste0("Kg_", dias_previos_nom)

rc_sem <- rc_map %>% filter(as.numeric(Semana) == sem_sel)

# Check SAP for M5 T11 & T12
m5_sap <- hist_wide %>% filter(as.numeric(Semana) == sem_sel, Módulo_Norm == "5", Turno_Norm %in% c("11", "12"))
cat("=== M5 SAP in Sem 45 ===\n")
print(m5_sap %>% select(Turno_Norm, Codigo_Valv, Ha_Total_Sem, Kg_Total_Sem, any_of(c("Ha_Lunes", "Kg_Lunes", "Ha_Martes", "Kg_Martes"))))

# Let's inspect what happens in app.R for M5 T11 and T12
cat("\nLet's check Proy and S-1 for M5 T11 & T12:\n")
m5_s1 <- hist_wide %>% filter(as.numeric(Semana) == sem_ant, Módulo_Norm == "5", Turno_Norm %in% c("11", "12"))
print(m5_s1 %>% select(Turno_Norm, Codigo_Valv, Ha_Total_Sem, Kg_Total_Sem))
