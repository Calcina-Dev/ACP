library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
camp_sel <- "2026"
dia_inicio_idx <- 2 # Martes

df_cosecha_sem <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, Semana == sem_sel, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo    = trimws(as.character(Módulo)),
    Turno     = trimws(as.character(Turno)),
    Variedad  = trimws(toupper(as.character(Variedad))),
    wday_num  = lubridate::wday(Fecha, week_start = 1)
  )

kg_real_turno <- df_cosecha_sem %>%
  filter(Módulo == "1") %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Real_Sem             = sum(`Kg Total`[wday_num < dia_inicio_idx], na.rm = TRUE),
    Kg_Total_Completo_Sem   = sum(`Kg Total`, na.rm = TRUE),
    Kg_Lunes_Real           = sum(`Kg Total`[wday_num == 1], na.rm = TRUE),
    Kg_Martes_Real          = sum(`Kg Total`[wday_num == 2], na.rm = TRUE),
    Sap_Cosecho_Previo      = any(wday_num < dia_inicio_idx & `Kg Total` > 0),
    Sap_Cosecho_Lunes       = any(wday_num == 1 & `Kg Total` > 0),
    Sap_Cosecho_Martes      = any(wday_num == 2 & `Kg Total` > 0),
    .groups                 = "drop"
  )

print(kg_real_turno)
