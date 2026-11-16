suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(plotly)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc <- df_esc %>% filter(Nombre == "Escenario Martes a Viernes (09-Nov. 14:37)")
if (nrow(esc) == 0) esc <- df_esc[3, ]

dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
d_desde <- esc$Dia_Desde
d_hasta <- esc$Dia_Hasta
idx_d <- match(d_desde, dias_semana_nombres)
idx_h <- match(d_hasta, dias_semana_nombres)
dias_escenario <- dias_semana_nombres[idx_d:idx_h]
dias_previos   <- if (idx_d > 1) dias_semana_nombres[1:(idx_d - 1)] else character(0)

# Normalizador de módulos
norm_mod <- function(m) {
  case_when(
    as.character(m) %in% c("13.1", "13.2") ~ "13",
    as.character(m) %in% c("14.1", "14.2") ~ "14",
    TRUE ~ trimws(as.character(m))
  )
}

# Real SAP
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
cs_45 <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", as.numeric(Semana) == 45, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Módulo   = norm_mod(Módulo),
    Sector   = trimws(as.character(Sector)),
    Turno    = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad))),
    Fecha_d  = as.Date(Fecha),
    Dia_Nom  = dias_nombres[wday(Fecha_d, week_start = 1)]
  )

# Ediciones del escenario
ed_esc <- esc$Ediciones_Data[[1]] %>%
  mutate(
    Módulo   = norm_mod(Módulo),
    Sector   = trimws(as.character(Sector)),
    Turno    = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

# 1. Perspective Rango Escenario: Mar-Vie
ed_rango <- ed_esc %>%
  filter(Campo %in% dias_escenario, coalesce(Valor, 0) > 0) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Ajustada = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    Motivo_Ajuste = first(na.omit(as.character(Motivo_Ajuste))),
    .groups = "drop"
  )

sap_rango <- cs_45 %>%
  filter(Dia_Nom %in% dias_escenario) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Kg_Real_SAP = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1), .groups = "drop")

comp_rango <- full_join(ed_rango, sap_rango, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    Ha_Ajustada   = coalesce(Ha_Ajustada, 0),
    Kg_Ajustado   = coalesce(Kg_Ajustado, 0),
    Kg_Real_SAP   = coalesce(Kg_Real_SAP, 0),
    Diferencia_Kg = round(Kg_Ajustado - Kg_Real_SAP, 1),
    Pct_Desvio    = ifelse(Kg_Real_SAP > 0, round(((Kg_Ajustado - Kg_Real_SAP) / Kg_Real_SAP) * 100, 1), NA_real_),
    Turno_Label   = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  )

# Total en Mar-Vie para turnos ajustados
turnos_aj_keys <- paste(ed_rango$Módulo, ed_rango$Turno, ed_rango$Variedad)
comp_rango_aj <- comp_rango %>% filter(paste(Módulo, Turno, Variedad) %in% turnos_aj_keys)
cat("Totales de turnos ajustados en Mar-Vie:\n")
cat("Ajustado:", sum(comp_rango_aj$Kg_Ajustado), "kg\n")
cat("Real SAP Mar-Vie para esos turnos:", sum(comp_rango_aj$Kg_Real_SAP), "kg\n")
cat("Diferencia neta:", sum(comp_rango_aj$Diferencia_Kg), "kg\n")
cat("Precisión global:", round((1 - (sum(abs(comp_rango_aj$Diferencia_Kg)) / sum(comp_rango_aj$Kg_Real_SAP))) * 100, 1), "%\n")
