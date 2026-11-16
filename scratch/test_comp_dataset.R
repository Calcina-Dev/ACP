suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
rutas <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

camp_sel <- "2026"
sem_sel  <- 45
dia_sel  <- "Viernes" # or "Todos"

ed_sem <- ediciones %>%
  filter(as.character(`Campaña`) == as.character(camp_sel), as.numeric(Semana) == as.numeric(sem_sel))

if (dia_sel != "Todos") {
  ed_sem <- ed_sem %>% filter(Campo == dia_sel)
}

ed_turnos <- ed_sem %>%
  filter(coalesce(Valor, 0) > 0, coalesce(Tipo_Edicion, "") != "ruta_sin_kg") %>%
  group_by(Sector = as.character(Sector), Módulo = as.character(Módulo), Turno = as.character(Turno), Variedad = toupper(trimws(as.character(Variedad)))) %>%
  summarise(
    Ha_Ajustada  = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado  = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    Motivos      = paste(unique(na.omit(Motivo_Ajuste[nzchar(Motivo_Ajuste)])), collapse = " | "),
    .groups      = "drop"
  )

# SAP cosecha real
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
cs_sem <- df_cosecha %>%
  filter(as.character(Campaña) == as.character(camp_sel), as.numeric(Semana) == as.numeric(sem_sel), !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Fecha_d  = as.Date(Fecha),
    Dia_Nom  = dias_nombres[wday(Fecha_d, week_start = 1)],
    Sector   = as.character(Sector),
    Módulo   = as.character(Módulo),
    Turno    = as.character(Turno),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

if (dia_sel != "Todos") {
  cs_sem <- cs_sem %>% filter(Dia_Nom == dia_sel)
}

sap_turnos <- cs_sem %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Real_SAP = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1),
    .groups     = "drop"
  )

# Rutas de cosecha campo ha
r_sem <- rutas %>%
  filter(as.character(Campaña) == as.character(camp_sel), as.numeric(Semana) == as.numeric(sem_sel)) %>%
  mutate(
    Sector   = as.character(Sector),
    Módulo   = as.character(Módulo),
    Turno    = as.character(Turno),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

ha_col <- if (dia_sel == "Todos") "Ha_Total_Prog" else paste0("Ha_", dia_sel)
if (ha_col %in% names(r_sem)) {
  rutas_turnos <- r_sem %>%
    group_by(Sector, Módulo, Turno, Variedad) %>%
    summarise(
      Ha_Campo = round(sum(coalesce(.data[[ha_col]], 0), na.rm = TRUE), 2),
      .groups  = "drop"
    )
} else {
  rutas_turnos <- tibble(Sector = character(), Módulo = character(), Turno = character(), Variedad = character(), Ha_Campo = numeric())
}

# Full join
comp_df <- full_join(ed_turnos, sap_turnos, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(rutas_turnos, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    Ha_Ajustada   = coalesce(Ha_Ajustada, 0),
    Kg_Ajustado   = coalesce(Kg_Ajustado, 0),
    Kg_Real_SAP   = coalesce(Kg_Real_SAP, 0),
    Ha_Campo      = coalesce(Ha_Campo, 0),
    Diferencia_Kg = round(Kg_Ajustado - Kg_Real_SAP, 1),
    Pct_Desvio    = ifelse(Kg_Real_SAP > 0, round(((Kg_Ajustado - Kg_Real_SAP) / Kg_Real_SAP) * 100, 1), NA_real_),
    Rend_Ajustado = ifelse(Ha_Ajustada > 0 & Kg_Ajustado > 0, round(Kg_Ajustado / Ha_Ajustada, 1), NA_real_),
    Rend_Real     = ifelse(Ha_Campo > 0 & Kg_Real_SAP > 0, round(Kg_Real_SAP / Ha_Campo, 1), NA_real_),
    Delta_Rend    = ifelse(!is.na(Rend_Ajustado) & !is.na(Rend_Real), round(Rend_Ajustado - Rend_Real, 1), NA_real_),
    Turno_Label   = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  ) %>%
  arrange(desc(abs(Diferencia_Kg)))

cat("Comparativo Turnos:", nrow(comp_df), "\n")
print(head(comp_df %>% select(Turno_Label, Ha_Ajustada, Kg_Ajustado, Kg_Real_SAP, Diferencia_Kg, Pct_Desvio), 8))
cat("Total Kg Ajustado:", sum(comp_df$Kg_Ajustado), "\n")
cat("Total Kg Real SAP:", sum(comp_df$Kg_Real_SAP), "\n")
cat("Diferencia Neta:", sum(comp_df$Kg_Ajustado) - sum(comp_df$Kg_Real_SAP), "\n")
