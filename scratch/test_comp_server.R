suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(plotly)
  library(DT)
  library(writexl)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
rutas <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

# Mock inputs
input <- list(
  filtro_campana = "2026",
  filtro_semana = 45,
  filtro_dia_comp = "Viernes",
  filtro_metrica_comp = "kilos",
  filtro_modulo = "Todos",
  filtro_variedad = character(0)
)

# Test reactive logic
ed_todas <- ediciones
camp_sel <- as.character(input$filtro_campana)
sem_sel  <- as.numeric(input$filtro_semana)

ed_sem <- ed_todas %>%
  filter(
    as.character(`Campaña`) == camp_sel,
    as.numeric(Semana) == sem_sel,
    coalesce(Valor, 0) > 0,
    coalesce(Tipo_Edicion, "") != "ruta_sin_kg"
  )

cat("Ediciones encontradas:", nrow(ed_sem), "\n")
dias_con_ajuste <- unique(as.character(ed_sem$Campo))
cat("Días con ajuste:", paste(dias_con_ajuste, collapse = ", "), "\n")

dia_sel <- input$filtro_dia_comp
if (dia_sel != "Todos" && dia_sel %in% dias_con_ajuste) {
  ed_sem <- ed_sem %>% filter(Campo == dia_sel)
}

ed_turnos <- ed_sem %>%
  group_by(
    Sector = as.character(Sector),
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = toupper(trimws(as.character(Variedad)))
  ) %>%
  summarise(
    Ha_Ajustada = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    Motivo_Ajuste = {
      m <- unique(na.omit(as.character(Motivo_Ajuste[nzchar(as.character(Motivo_Ajuste))])))
      if (length(m) > 0) paste(m, collapse = " | ") else "Ajuste manual"
    },
    .groups = "drop"
  )

# SAP Cosecha Real
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
cs_sem <- df_cosecha %>%
  filter(
    as.character(Campaña) == camp_sel,
    as.numeric(Semana) == sem_sel,
    !coalesce(Es_Testblock, FALSE)
  ) %>%
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
} else if (length(dias_con_ajuste) > 0) {
  cs_sem <- cs_sem %>% filter(Dia_Nom %in% dias_con_ajuste)
}

sap_turnos <- cs_sem %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Real_SAP = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1),
    .groups     = "drop"
  )

# Rutas Campo Ha
r_sem <- rutas %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(
    Sector   = as.character(Sector),
    Módulo   = as.character(Módulo),
    Turno    = as.character(Turno),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

ha_col <- if (dia_sel == "Todos") "Ha_Total_Prog" else paste0("Ha_", dia_sel)
rutas_turnos <- if (ha_col %in% names(r_sem)) {
  r_sem %>%
    group_by(Sector, Módulo, Turno, Variedad) %>%
    summarise(
      Ha_Campo = round(sum(coalesce(.data[[ha_col]], 0), na.rm = TRUE), 2),
      .groups  = "drop"
    )
} else {
  tibble(Sector = character(), Módulo = character(), Turno = character(), Variedad = character(), Ha_Campo = numeric())
}

# Join comparativo
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
    Turno_Label   = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad),
    Motivo_Ajuste = coalesce(Motivo_Ajuste, "Sin ajuste programado")
  ) %>%
  arrange(desc(abs(Diferencia_Kg)))

cat("Total turnos evaluados:", nrow(comp_df), "\n")
cat("Suma Kg Ajustados:", sum(comp_df$Kg_Ajustado), "\n")
cat("Suma Kg Real SAP:", sum(comp_df$Kg_Real_SAP), "\n")
cat("Desvío Neto:", sum(comp_df$Kg_Ajustado) - sum(comp_df$Kg_Real_SAP), "\n")
