library(dplyr)
library(tidyr)

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones_manuales <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_data    <- readRDS("D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds")

camp_sel <- "2026"
sem_sel  <- 45
df_esc   <- escenarios_data

norm_mod <- function(m) {
  case_when(
    as.character(m) %in% c("13.1", "13.2") ~ "13",
    as.character(m) %in% c("14.1", "14.2") ~ "14",
    TRUE ~ trimws(as.character(m))
  )
}

dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_nombres_wday   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")

# Let's test with the active scenario:
id_a <- "ESC-2026-S45-20261109143748" # Martes a Viernes
esc_info <- df_esc %>% filter(Id_Escenario == id_a)
cat("Escenario Info:", esc_info$Nombre, "| Dia_Desde:", esc_info$Dia_Desde, "| Dia_Hasta:", esc_info$Dia_Hasta, "\n")

d_desde <- as.character(esc_info$Dia_Desde[1])
d_hasta <- as.character(esc_info$Dia_Hasta[1])
idx_d <- match(d_desde, dias_semana_nombres)
idx_h <- match(d_hasta, dias_semana_nombres)
dias_escenario <- dias_semana_nombres[idx_d:idx_h]
dias_previos   <- if (idx_d > 1) dias_semana_nombres[1:(idx_d - 1)] else character(0)

ed_eval <- esc_info$Ediciones_Data[[1]] %>%
  mutate(
    Módulo   = norm_mod(Módulo),
    Sector   = trimws(as.character(Sector)),
    Turno    = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

cs_sem <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Módulo   = norm_mod(Módulo),
    Sector   = trimws(as.character(Sector)),
    Turno    = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad))),
    Fecha_d  = as.Date(Fecha),
    Dia_Nom  = dias_nombres_wday[lubridate::wday(Fecha_d, week_start = 1)]
  )

cat("--- PERSPECTIVA: RANGO ESCENARIO (MARTES A VIERNES) PARA MODULO 13 ---\n")
dia_sel <- "Todos"
dias_filtrar <- dias_escenario

ed_turnos <- ed_eval %>%
  filter(Campo %in% dias_filtrar, coalesce(Valor, 0) > 0) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Ajustada   = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado   = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    Motivo_Ajuste = first(na.omit(as.character(Motivo_Ajuste))),
    .groups       = "drop"
  )

sap_turnos <- cs_sem %>%
  filter(Dia_Nom %in% dias_filtrar) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Kg_Real_SAP = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1), .groups = "drop")

comp_df <- full_join(ed_turnos, sap_turnos, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    Ha_Ajustada   = coalesce(Ha_Ajustada, 0),
    Kg_Ajustado   = coalesce(Kg_Ajustado, 0),
    Kg_Real_SAP   = coalesce(Kg_Real_SAP, 0),
    Diferencia_Kg = round(Kg_Ajustado - Kg_Real_SAP, 1),
    Pct_Desvio    = ifelse(Kg_Real_SAP > 0, round(((Kg_Ajustado - Kg_Real_SAP) / Kg_Real_SAP) * 100, 1), NA_real_),
    Turno_Label   = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  )

# Filter by Module 13
comp_m13 <- comp_df %>% filter(Módulo == "13") %>% arrange(as.numeric(Turno))
print(comp_m13 %>% select(Turno_Label, Kg_Ajustado, Kg_Real_SAP, Diferencia_Kg, Pct_Desvio))
cat("Totales Módulo 13 en Rango (Martes a Viernes):\n")
cat("Kilos Ajustados:", sum(comp_m13$Kg_Ajustado), "kg | Real SAP (Mar-Vie):", sum(comp_m13$Kg_Real_SAP), "kg | Dif:", sum(comp_m13$Diferencia_Kg), "kg\n\n")

cat("--- PERSPECTIVA: SEMANA COMPLETA PARA MODULO 13 ---\n")
df_proy_sub <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(
    Módulo   = norm_mod(Módulo),
    Sector   = trimws(as.character(Sector)),
    Turno    = trimws(as.character(Turno)),
    Variedad = toupper(trimws(as.character(Variedad)))
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Kg_Proy_Oficial = round(sum(coalesce(Proyección, 0), na.rm = TRUE), 1), .groups = "drop")

ed_rango_sem <- ed_eval %>%
  filter(Campo %in% dias_escenario, coalesce(Valor, 0) > 0) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Ajustada_Rango = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado_Rango = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    .groups = "drop"
  )

sap_prev_sem <- cs_sem %>%
  filter(Dia_Nom %in% dias_previos) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Kg_Real_Prev = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1), .groups = "drop")

sap_tot_sem <- cs_sem %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Kg_Real_Semana = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1), .groups = "drop")

comp_sem <- full_join(df_proy_sub, ed_rango_sem, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(sap_prev_sem, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(sap_tot_sem, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Proy_Oficial    = coalesce(Kg_Proy_Oficial, 0),
    Kg_Ajustado_Rango  = coalesce(Kg_Ajustado_Rango, 0),
    Kg_Real_Prev       = coalesce(Kg_Real_Prev, 0),
    Kg_Ajustado_Semana = round(Kg_Real_Prev + Kg_Ajustado_Rango, 1),
    Kg_Real_Semana     = coalesce(Kg_Real_Semana, 0),
    Dif_Aj_vs_Proy     = round(Kg_Ajustado_Semana - Kg_Proy_Oficial, 1),
    Pct_Aj_vs_Proy     = ifelse(Kg_Proy_Oficial > 0, round((Dif_Aj_vs_Proy / Kg_Proy_Oficial) * 100, 1), NA_real_),
    Dif_Aj_vs_Real     = round(Kg_Ajustado_Semana - Kg_Real_Semana, 1),
    Pct_Aj_vs_Real     = ifelse(Kg_Real_Semana > 0, round((Dif_Aj_vs_Real / Kg_Real_Semana) * 100, 1), NA_real_),
    Turno_Label        = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  )

comp_sem_m13 <- comp_sem %>% filter(Módulo == "13") %>% arrange(as.numeric(Turno))
print(comp_sem_m13 %>% select(Turno_Label, Kg_Proy_Oficial, Kg_Ajustado_Semana, Dif_Aj_vs_Proy, Kg_Real_Semana, Dif_Aj_vs_Real))
cat("Totales Semana Completa Módulo 13:\n")
cat("Proyección Oficial:", sum(comp_sem_m13$Kg_Proy_Oficial), "kg\n")
cat("Cosecha Ajustada Semana (Lunes Real + Mar-Vie Aj):", sum(comp_sem_m13$Kg_Ajustado_Semana), "kg | Dif vs Proy:", sum(comp_sem_m13$Dif_Aj_vs_Proy), "kg\n")
cat("Cosecha Real Semanal:", sum(comp_sem_m13$Kg_Real_Semana), "kg | Dif vs Real:", sum(comp_sem_m13$Dif_Aj_vs_Real), "kg\n")
