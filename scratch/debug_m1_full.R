library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
sem_ant  <- 44
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg   <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

norm_codigo_valvula <- function(x) trimws(toupper(as.character(x)))
limpiar_variedad    <- function(x) trimws(toupper(as.character(x)))

# Let's inspect df_valvulas for M1
valvulas_calc <- df_valvulas %>%
  filter(as.character(Módulo) == "1") %>%
  mutate(
    codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
    Turno_num   = suppressWarnings(as.numeric(Turno)),
    Sector      = "6",
    Módulo      = "1",
    Turno       = trimws(as.character(Turno)),
    Variedad    = limpiar_variedad(Variedad),
    Área        = suppressWarnings(as.numeric(Área))
  )

# Let's see: what did sap_m1 have?
cs_m1 <- df_cosecha %>% filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1")
cat("cs_m1 rows:", nrow(cs_m1), "\n")
print(cs_m1 %>% select(Fecha, Turno, `Kg Total`))

# In app.R:
# sap_valv_avance:
dias_nombres_wday <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
sap_valv_avance <- cs_m1 %>%
  mutate(
    Fecha_Date = as.Date(Fecha),
    Dia_Texto  = dias_nombres_wday[as.integer(format(Fecha_Date, "%u"))],
    codigo_norm = norm_codigo_valvula(Válvula)
  )
cat("sap_valv_avance codigo_norm:\n")
print(head(sap_valv_avance$codigo_norm))
