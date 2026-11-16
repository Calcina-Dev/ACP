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

cat("Escenario seleccionado:", esc$Nombre, "\n")
cat("Dia_Desde:", esc$Dia_Desde, "Dia_Hasta:", esc$Dia_Hasta, "\n")

dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
idx_d <- match(esc$Dia_Desde, dias_semana_nombres)
idx_h <- match(esc$Dia_Hasta, dias_semana_nombres)
dias_escenario <- dias_semana_nombres[idx_d:idx_h]
dias_previos   <- if (idx_d > 1) dias_semana_nombres[1:(idx_d - 1)] else character(0)

cat("Días previos (Real):", paste(dias_previos, collapse = ", "), "\n")
cat("Días escenario (Ajuste):", paste(dias_escenario, collapse = ", "), "\n")

# Ediciones del escenario
ed_esc <- esc$Ediciones_Data[[1]]
cat("Ediciones rows:", nrow(ed_esc), "\n")

# Turnos ajustados en el escenario (Martes a Viernes)
ed_turnos <- ed_esc %>%
  filter(Campo %in% dias_escenario, coalesce(Valor, 0) > 0) %>%
  group_by(Sector = as.character(Sector), Módulo = as.character(Módulo), Turno = as.character(Turno), Variedad = toupper(trimws(as.character(Variedad)))) %>%
  summarise(
    Ha_Ajustada = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Ajustado = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    .groups     = "drop"
  )

cat("Turnos con ajuste en Mar-Vie:", nrow(ed_turnos), "Total Kg Ajustado:", sum(ed_turnos$Kg_Ajustado), "\n")

# Real SAP para EXACTAMENTE ESOS MISMOS DÍAS (Martes a Viernes)
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
cs_esc <- df_cosecha %>%
  filter(
    as.character(Campaña) == "2026",
    as.numeric(Semana) == 45,
    !coalesce(Es_Testblock, FALSE)
  ) %>%
  mutate(
    Fecha_d = as.Date(Fecha),
    Dia_Nom = dias_nombres[wday(Fecha_d, week_start = 1)],
    Sector  = as.character(Sector),
    Módulo  = as.character(Módulo),
    Turno   = as.character(Turno),
    Variedad = toupper(trimws(as.character(Variedad)))
  ) %>%
  filter(Dia_Nom %in% dias_escenario) # STRICTLY MARTES A VIERNES!

sap_turnos <- cs_esc %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Real_SAP = round(sum(coalesce(`Kg Total`, 0), na.rm = TRUE), 1),
    .groups     = "drop"
  )

cat("Turnos con Real SAP en Mar-Vie:", nrow(sap_turnos), "Total Kg Real SAP Mar-Vie:", sum(sap_turnos$Kg_Real_SAP), "\n")

# Now let's compare for Módulo 13 (or Módulo 1)
mod_test <- "13"
comp_mod <- full_join(
  ed_turnos %>% filter(Módulo == mod_test),
  sap_turnos %>% filter(Módulo == mod_test),
  by = c("Sector", "Módulo", "Turno", "Variedad")
) %>%
  mutate(
    Kg_Ajustado = coalesce(Kg_Ajustado, 0),
    Kg_Real_SAP = coalesce(Kg_Real_SAP, 0),
    Dif_Kg      = round(Kg_Ajustado - Kg_Real_SAP, 1),
    Pct_Desvio  = ifelse(Kg_Real_SAP > 0, round(((Kg_Ajustado - Kg_Real_SAP) / Kg_Real_SAP) * 100, 1), NA_real_),
    Turno_Label = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  )

cat("Comparativo Módulo", mod_test, "en Mar-Vie:\n")
print(comp_mod %>% select(Turno_Label, Kg_Ajustado, Kg_Real_SAP, Dif_Kg, Pct_Desvio))
cat("Total Ajustado Mod", mod_test, ":", sum(comp_mod$Kg_Ajustado), "\n")
cat("Total Real SAP Mod", mod_test, ":", sum(comp_mod$Kg_Real_SAP), "\n")
cat("Diferencia:", sum(comp_mod$Kg_Ajustado) - sum(comp_mod$Kg_Real_SAP), "\n")
