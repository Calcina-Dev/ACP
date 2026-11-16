suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc <- df_esc %>% filter(Nombre == "Escenario Martes a Viernes (09-Nov. 14:37)")
if (nrow(esc) == 0) esc <- df_esc[3, ]

dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
idx_d <- match(esc$Dia_Desde, dias_semana_nombres)
idx_h <- match(esc$Dia_Hasta, dias_semana_nombres)
dias_escenario <- dias_semana_nombres[idx_d:idx_h]
dias_previos   <- if (idx_d > 1) dias_semana_nombres[1:(idx_d - 1)] else character(0)

# 1. Proyección oficial de semana 45
df_proy_sem <- df_proyecciones %>%
  filter(as.character(Campaña) == "2026", as.numeric(Semana) == 45) %>%
  mutate(
    Módulo_norm = case_when(
      as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
      as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
      TRUE ~ trimws(as.character(Módulo))
    ),
    Sector = trimws(as.character(Sector)),
    Turno  = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad)))
  ) %>%
  group_by(Sector, Módulo = Módulo_norm, Turno, Variedad) %>%
  summarise(Kg_Proyeccion_Oficial = round(sum(coalesce(Proyección, 0), na.rm = TRUE), 1), .groups = "drop")

# 2. Ediciones del escenario
ed_sem <- esc$Ediciones_Data[[1]] %>%
  mutate(
    Módulo_norm = case_when(
      as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
      as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
      TRUE ~ trimws(as.character(Módulo))
    ),
    Sector = trimws(as.character(Sector)),
    Turno  = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

ed_rango <- ed_sem %>%
  filter(Campo %in% dias_escenario, coalesce(Valor, 0) > 0) %>%
  group_by(Sector, Módulo = Módulo_norm, Turno, Variedad) %>%
  summarise(
    Ha_Ajustada_Rango = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado_Rango = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    .groups = "drop"
  )

# 3. Real SAP con fechas normalizadas
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
cs_45 <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", as.numeric(Semana) == 45, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Módulo_norm = case_when(
      as.character(Módulo) %in% c("13.1", "13.2") ~ "13",
      as.character(Módulo) %in% c("14.1", "14.2") ~ "14",
      TRUE ~ trimws(as.character(Módulo))
    ),
    Fecha_d = as.Date(Fecha),
    Dia_Nom = dias_nombres[wday(Fecha_d, week_start = 1)],
    Sector = trimws(as.character(Sector)),
    Turno  = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

sap_rango <- cs_45 %>%
  filter(Dia_Nom %in% dias_escenario) %>%
  group_by(Sector, Módulo = Módulo_norm, Turno, Variedad) %>%
  summarise(Kg_Real_Rango = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1), .groups = "drop")

sap_prev <- cs_45 %>%
  filter(Dia_Nom %in% dias_previos) %>%
  group_by(Sector, Módulo = Módulo_norm, Turno, Variedad) %>%
  summarise(Kg_Real_Prev = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1), .groups = "drop")

sap_tot_sem <- cs_45 %>%
  group_by(Sector, Módulo = Módulo_norm, Turno, Variedad) %>%
  summarise(Kg_Real_Semana = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1), .groups = "drop")

# Cruce Semana Completa: Proyección vs Ajustado vs Real
sem_comp <- full_join(df_proy_sem, ed_rango, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(sap_prev, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(sap_tot_sem, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Proyeccion_Oficial = coalesce(Kg_Proyeccion_Oficial, 0),
    Kg_Ajustado_Rango     = coalesce(Kg_Ajustado_Rango, 0),
    Kg_Real_Prev          = coalesce(Kg_Real_Prev, 0),
    Kg_Ajustado_Semana    = round(Kg_Real_Prev + Kg_Ajustado_Rango, 1),
    Kg_Real_Semana        = coalesce(Kg_Real_Semana, 0),
    Dif_Aj_vs_Proy        = round(Kg_Ajustado_Semana - Kg_Proyeccion_Oficial, 1),
    Pct_Aj_vs_Proy        = ifelse(Kg_Proyeccion_Oficial > 0, round((Dif_Aj_vs_Proy / Kg_Proyeccion_Oficial) * 100, 1), NA_real_),
    Dif_Aj_vs_Real        = round(Kg_Ajustado_Semana - Kg_Real_Semana, 1),
    Pct_Aj_vs_Real        = ifelse(Kg_Real_Semana > 0, round((Dif_Aj_vs_Real / Kg_Real_Semana) * 100, 1), NA_real_),
    Turno_Label           = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  )

cat("--- Filtrando Módulo 13 ---\n")
m13_comp <- sem_comp %>% filter(Módulo == "13")
print(m13_comp %>% select(Turno_Label, Kg_Proyeccion_Oficial, Kg_Ajustado_Semana, Kg_Real_Semana, Dif_Aj_vs_Proy, Dif_Aj_vs_Real))
cat("\nTotales Módulo 13:\n")
cat("Proyección Oficial:", sum(m13_comp$Kg_Proyeccion_Oficial), "kg\n")
cat("Cosecha Ajustada:", sum(m13_comp$Kg_Ajustado_Semana), "kg\n")
cat("Cosecha Real Semanal:", sum(m13_comp$Kg_Real_Semana), "kg\n")
cat("Dif Ajuste vs Proy:", sum(m13_comp$Kg_Ajustado_Semana) - sum(m13_comp$Kg_Proyeccion_Oficial), "kg\n")
cat("Dif Ajuste vs Real:", sum(m13_comp$Kg_Ajustado_Semana) - sum(m13_comp$Kg_Real_Semana), "kg\n")
