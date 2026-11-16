suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(plotly)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc <- df_esc %>% filter(Nombre == "Escenario Martes a Viernes (09-Nov. 14:37)")
if (nrow(esc) == 0) esc <- df_esc[3, ]

norm_mod <- function(m) {
  case_when(
    as.character(m) %in% c("13.1", "13.2") ~ "13",
    as.character(m) %in% c("14.1", "14.2") ~ "14",
    TRUE ~ trimws(as.character(m))
  )
}

# Test 1: Rango Escenario (Mar-Vie) with Modulo 13 filter
dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
idx_d <- match(esc$Dia_Desde, dias_semana_nombres)
idx_h <- match(esc$Dia_Hasta, dias_semana_nombres)
dias_escenario <- dias_semana_nombres[idx_d:idx_h]

ed_esc <- esc$Ediciones_Data[[1]] %>%
  mutate(Módulo = norm_mod(Módulo), Sector = trimws(as.character(Sector)), Turno = trimws(as.character(Turno)), Variedad = toupper(trimws(as.character(Variedad))))

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

# Modulo 13 test
f_mod <- "13"
ed_rango <- ed_esc %>%
  filter(Campo %in% dias_escenario, coalesce(Valor, 0) > 0, Módulo == f_mod) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Ajustada   = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado   = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    Motivo_Ajuste = first(na.omit(as.character(Motivo_Ajuste))),
    .groups       = "drop"
  )

sap_rango <- cs_45 %>%
  filter(Dia_Nom %in% dias_escenario, Módulo == f_mod) %>%
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

cat("Test 1 (Rango Mar-Vie Mod 13) successful! Rows:", nrow(comp_rango), "\n")
print(head(comp_rango, 3))
