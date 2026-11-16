library(dplyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
camp_sel <- "2026"
dia_inicio_idx <- 2 # Martes
umbral_avance_cfg <- 15
prop_umbral_cfg <- 0.15

# Let's inspect rend_turno logic from app.R lines 12220-12380
# For M1:
proy_m1 <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel, as.character(Módulo) == "1") %>%
  mutate(Módulo = "1", Turno = as.character(Turno), Variedad = trimws(toupper(as.character(Variedad)))) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Proy_Kg = sum(Proyección), .groups="drop")

valvs_m1 <- df_valvulas %>%
  filter(as.character(Módulo) == "1") %>%
  mutate(Módulo = "1", Turno = as.character(Turno), Variedad = trimws(toupper(as.character(Variedad))), Área = as.numeric(Área)) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Area_Total_Turno = sum(Área), Valvulas_Total = n(), .groups="drop")

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

rend_m1 <- valvs_m1 %>%
  left_join(proy_m1, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Rend_Proy = Proy_Kg / Area_Total_Turno,
    Esta_Proyectado = Proy_Kg > 0,
    Cosecho_Lunes   = Sap_Cosecho_Lunes,
    Cosecho_Martes  = Sap_Cosecho_Martes,
    Tiene_Cosecha_Act = Sap_Cosecho_Previo | Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0,
    # In app.R:
    # Ha_Prog_Turno = 0 (since rc_sem had 0 rows for M1)
    Ha_Prog_Turno = 0,
    Ha_Recorrida_Turno = 0,
    Semana_Ant_No_Finalizo = FALSE,
    Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act & (Area_Total_Turno - Ha_Recorrida_Turno <= 0.05),
    Es_Ciclo_Corto = FALSE
  )

print(rend_m1 %>% select(Turno, Area_Total_Turno, Proy_Kg, Rend_Proy, Tiene_Cosecha_Act, Turno_Completo_Lun_Mar))
