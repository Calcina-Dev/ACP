library(dplyr)
library(tidyr)
library(readr)

# Load master data
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones_manuales <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
escenarios_data    <- readRDS("D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds")

camp_sel <- "2026"
sem_sel  <- 45

cat("=== TESTING VISTA AJUSTADO VS VISTA REAL ===\n")
# Active scenario: Escenario Martes a Viernes
esc_row <- escenarios_data %>% filter(Semana == 45) %>% tail(1)
cat("Escenario activo:", esc_row$Nombre_Escenario, "| Rango:", esc_row$Dia_Desde, "a", esc_row$Dia_Hasta, "\n")
ed_esc <- esc_row$Ediciones_Data[[1]]

# Turno 1 de Módulo 13
ed_t1 <- ed_esc %>% filter(as.character(Módulo) == "13", as.character(Turno) == "1")
cat("Ajustes en Escenario para M13-T1:\n")
print(ed_t1 %>% select(Módulo, Turno, Día, Tipo_Ajuste, Valor, Ha_Ajustada))

# In df_cosecha:
df_cos_t1 <- df_cosecha %>% 
  mutate(Módulo_Norm = case_when(Módulo %in% c("13.1", "13.2") ~ "13", TRUE ~ Módulo)) %>%
  filter(Semana == 45, Módulo_Norm == "13", Turno == "1")
cat("Cosecha Real SAP en df_cosecha para M13-T1:\n")
print(df_cos_t1 %>% select(Módulo, Turno, Fecha, Kilos_Reales))

cat("\n=== TESTING COMPARATIVO LOGIC WITH MODULO FILTER ===\n")
norm_mod <- function(m) {
  case_when(
    as.character(m) %in% c("13.1", "13.2") ~ "13",
    as.character(m) %in% c("14.1", "14.2") ~ "14",
    TRUE ~ trimws(as.character(m))
  )
}

# Test Módulo 13
# Perspective 1: Rango Escenario (Martes a Viernes)
dias_rango <- c("Martes", "Miércoles", "Jueves", "Viernes")
ed_mod13 <- ed_esc %>% 
  mutate(Módulo_Norm = norm_mod(Módulo)) %>%
  filter(Módulo_Norm == "13", coalesce(Valor, 0) > 0)

ed_mod13_grp <- ed_mod13 %>%
  group_by(Sector, Módulo = Módulo_Norm, Turno, Variedad) %>%
  summarise(
    Kg_Ajustado = sum(Valor, na.rm = TRUE),
    Ha_Ajustada = sum(Ha_Ajustada, na.rm = TRUE),
    .groups = "drop"
  )

# Real SAP in range
dias_nombres_wday <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
real_range_m13 <- df_cosecha %>%
  filter(as.character(`Campaña`) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(
    Módulo_Norm = norm_mod(Módulo),
    Turno_chr   = trimws(as.character(Turno)),
    Fecha_Date  = as.Date(Fecha),
    Dia_Texto   = dias_nombres_wday[as.integer(format(Fecha_Date, "%u"))]
  ) %>%
  filter(Módulo_Norm == "13", Dia_Texto %in% dias_rango) %>%
  group_by(Módulo = Módulo_Norm, Turno = Turno_chr) %>%
  summarise(Kg_Real_SAP = sum(Kilos_Reales, na.rm = TRUE), .groups = "drop")

comp_m13 <- full_join(ed_mod13_grp, real_range_m13, by = c("Módulo", "Turno")) %>%
  mutate(
    Kg_Ajustado = coalesce(Kg_Ajustado, 0),
    Kg_Real_SAP = coalesce(Kg_Real_SAP, 0),
    Diferencia_Kg = Kg_Ajustado - Kg_Real_SAP,
    Pct_Desvio = ifelse(Kg_Real_SAP > 0, round((Diferencia_Kg / Kg_Real_SAP) * 100, 1), NA_real_)
  ) %>%
  arrange(as.numeric(Turno))

cat("Comparativo Rango Escenario (Martes - Viernes) para Módulo 13:\n")
print(comp_m13 %>% select(Módulo, Turno, Variedad, Kg_Ajustado, Kg_Real_SAP, Diferencia_Kg, Pct_Desvio))

cat("\nTotales M13 Rango Escenario:\n")
cat("Total Ajustado:", sum(comp_m13$Kg_Ajustado), "kg | Total Real SAP:", sum(comp_m13$Kg_Real_SAP), "kg | Dif:", sum(comp_m13$Diferencia_Kg), "kg\n")

# Perspective 2: Semana Completa (Proyección vs Ajustado vs Real SAP)
proy_mod13 <- df_proyecciones %>%
  filter(as.character(`Campaña`) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(Módulo_Norm = norm_mod(Módulo), Turno_chr = trimws(as.character(Turno))) %>%
  filter(Módulo_Norm == "13") %>%
  group_by(Sector, Módulo = Módulo_Norm, Turno = Turno_chr, Variedad) %>%
  summarise(Proy_Oficial = sum(as.numeric(`Proyección`), na.rm = TRUE), .groups = "drop")

# Cosecha Real Semanal Completa (Lunes a Sábado)
real_sem_m13 <- df_cosecha %>%
  filter(as.character(`Campaña`) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(Módulo_Norm = norm_mod(Módulo), Turno_chr = trimws(as.character(Turno))) %>%
  filter(Módulo_Norm == "13") %>%
  group_by(Módulo = Módulo_Norm, Turno = Turno_chr) %>%
  summarise(Kg_Real_Semanal = sum(Kilos_Reales, na.rm = TRUE), .groups = "drop")

# Cosecha Real previa (Lunes)
real_prev_m13 <- df_cosecha %>%
  filter(as.character(`Campaña`) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(
    Módulo_Norm = norm_mod(Módulo),
    Turno_chr   = trimws(as.character(Turno)),
    Fecha_Date  = as.Date(Fecha),
    Dia_Texto   = dias_nombres_wday[as.integer(format(Fecha_Date, "%u"))]
  ) %>%
  filter(Módulo_Norm == "13", Dia_Texto == "Lunes") %>%
  group_by(Módulo = Módulo_Norm, Turno = Turno_chr) %>%
  summarise(Kg_Real_Previo = sum(Kilos_Reales, na.rm = TRUE), .groups = "drop")

semana_comp_m13 <- proy_mod13 %>%
  left_join(real_prev_m13, by = c("Módulo", "Turno")) %>%
  left_join(ed_mod13_grp %>% select(Módulo, Turno, Kg_Ajustado), by = c("Módulo", "Turno")) %>%
  left_join(real_sem_m13, by = c("Módulo", "Turno")) %>%
  mutate(
    Proy_Oficial     = coalesce(Proy_Oficial, 0),
    Kg_Real_Previo   = coalesce(Kg_Real_Previo, 0),
    Kg_Ajustado      = coalesce(Kg_Ajustado, 0),
    Kg_Total_Ajustado = Kg_Real_Previo + Kg_Ajustado,
    Kg_Real_Semanal  = coalesce(Kg_Real_Semanal, 0),
    Dif_vs_Proy      = Kg_Total_Ajustado - Proy_Oficial,
    Pct_vs_Proy      = ifelse(Proy_Oficial > 0, round((Dif_vs_Proy / Proy_Oficial) * 100, 1), NA_real_),
    Dif_vs_Real      = Kg_Total_Ajustado - Kg_Real_Semanal,
    Pct_vs_Real      = ifelse(Kg_Real_Semanal > 0, round((Dif_vs_Real / Kg_Real_Semanal) * 100, 1), NA_real_)
  )

cat("\nComparativo Semana Completa para Módulo 13:\n")
print(semana_comp_m13 %>% select(Turno, Proy_Oficial, Kg_Total_Ajustado, Dif_vs_Proy, Kg_Real_Semanal, Dif_vs_Real))
cat("\nTotales M13 Semana Completa:\n")
cat("Proyección Oficial:", sum(semana_comp_m13$Proy_Oficial), "kg\n")
cat("Cosecha Ajustada (Lunes Real + Mar-Vie Aj):", sum(semana_comp_m13$Kg_Total_Ajustado), "kg | Dif vs Proy:", sum(semana_comp_m13$Dif_vs_Proy), "kg\n")
cat("Cosecha Real SAP Semanal:", sum(semana_comp_m13$Kg_Real_Semanal), "kg | Dif vs Real:", sum(semana_comp_m13$Dif_vs_Real), "kg\n")
