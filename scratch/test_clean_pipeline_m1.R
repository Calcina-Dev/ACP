library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
sem_ant  <- 44
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_cols_kg   <- c("Kg_Lunes", "Kg_Martes", "Kg_Miércoles", "Kg_Jueves", "Kg_Viernes", "Kg_Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
es_campana_act <- TRUE
modo_frec <- "auto"
prop_umbral_cfg <- 0.15
umbral_avance_cfg <- 15

norm_codigo_valvula <- function(x) trimws(toupper(as.character(x)))
limpiar_variedad    <- function(x) trimws(toupper(as.character(x)))

# 1. Valvulas M1
valvs_m1 <- df_valvulas %>%
  filter(as.character(Módulo) == "1") %>%
  mutate(
    codigo_norm = norm_codigo_valvula(coalesce(codigo_norm, codigo)),
    Turno_num   = suppressWarnings(as.numeric(Turno)),
    Sector      = "6",
    Módulo      = "1",
    Turno       = trimws(as.character(Turno)),
    Variedad    = limpiar_variedad(Variedad),
    Área        = suppressWarnings(as.numeric(Área))
  )

cat("Valvulas M1 count:", nrow(valvs_m1), "\n")

# 2. Rutas M1 in week 45
rc_sem <- if (exists("df_rutas_cosecha")) df_rutas_cosecha %>% filter(Semana == sem_sel) else tibble()
rc_valv_avance <- tibble(codigo_norm = character(), Ha_Prog_Rc = numeric(), Kg_Prog_Rc = numeric(), Ha_Lunes_Rc = numeric(), Ha_Martes_Rc = numeric(), Dia_Ruta_Sem = character())

# 3. SAP valv avance
df_cosecha_sem <- df_cosecha %>%
  filter(as.character(Campaña) == camp_sel, Semana == sem_sel, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo    = trimws(as.character(Módulo)),
    Turno     = trimws(as.character(Turno)),
    Variedad  = limpiar_variedad(Variedad),
    wday_num  = lubridate::wday(Fecha, week_start = 1)
  )

sap_valv_avance <- df_cosecha_sem %>%
  filter(!is.na(Válvula)) %>%
  group_by(codigo_norm = norm_codigo_valvula(Válvula)) %>%
  summarise(Ha_Prog_Sap = max(coalesce(Área, 0), na.rm = TRUE), Kg_Prog_Sap = sum(`Kg Total`, na.rm = TRUE), .groups = "drop")

# 4. Proyecciones
proy_sem <- df_proyecciones %>%
  filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel) %>%
  mutate(Módulo = trimws(as.character(Módulo)), Turno = trimws(as.character(Turno)), Variedad = limpiar_variedad(Variedad))

proy_turno <- proy_sem %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Proy_Kg = sum(Proyección, na.rm = TRUE), .groups = "drop")

# 5. Join
v_calc <- valvs_m1 %>%
  left_join(rc_valv_avance, by = "codigo_norm") %>%
  left_join(sap_valv_avance, by = "codigo_norm") %>%
  mutate(
    Ha_Prog = 0,
    Kg_Prog = 0,
    Ha_Lunes_Rc = 0,
    Ha_Martes_Rc = 0,
    Kg_Lunes_Valv = 0,
    Kg_Martes_Valv = 0,
    Ha_Faltante = Área,
    Dia_Habitual_Calc = "Martes", # as we saw earlier from w44
    Tiene_Ciclo_Previo_Valv = TRUE
  )

# 6. Rendimiento de turno
kg_real_turno <- df_cosecha_sem %>%
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

# Turno avance
turno_avance_ha <- v_calc %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Prog_Turno     = sum(Ha_Prog, na.rm = TRUE),
    Kg_Prog_Turno     = sum(Kg_Prog, na.rm = TRUE),
    Ha_Lunes_Turno    = sum(Ha_Lunes_Rc, na.rm = TRUE),
    Ha_Martes_Turno   = sum(Ha_Martes_Rc, na.rm = TRUE),
    Ha_Faltante_Turno = sum(Ha_Faltante, na.rm = TRUE),
    Area_Total_Turno  = sum(Área, na.rm = TRUE),
    Valvulas_Total    = n(),
    .groups           = "drop"
  ) %>%
  mutate(
    Ha_Manual_1ra          = 0,
    Ha_Manual_2da          = 0,
    Ha_Faltante_Turno_Neto = pmax(0, Area_Total_Turno - Ha_Prog_Turno - Ha_Manual_1ra),
    Turno_Completo_1ra     = (Ha_Faltante_Turno_Neto <= 0.05),
    Ha_Real_2da            = 0,
    Ha_Faltante_2da_Neto   = Area_Total_Turno,
    Turno_Completo_2da     = FALSE
  )

rend_turno <- turno_avance_ha %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(proy_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
    Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
    Proy_Kg            = coalesce(Proy_Kg, 0),
    Esta_Proyectado    = (Proy_Kg > 0),
    Rend_Ant_Turno     = 250, # from w44
    Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),
    Sap_Cosecho_Previo = coalesce(Sap_Cosecho_Previo, FALSE),
    Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
    Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
    Cosecho_Lunes      = Sap_Cosecho_Lunes,
    Cosecho_Martes     = Sap_Cosecho_Martes,
    Tiene_Cosecha_Act  = Sap_Cosecho_Previo | Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0,
    Ha_Recorrida_Turno = 0,
    Prop_Avance_Turno  = 0,
    Semana_Ant_No_Finalizo = FALSE,
    Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act & (Area_Total_Turno - Ha_Recorrida_Turno <= 0.05),
    Es_Ciclo_Corto         = FALSE,
    Rend_Base_Turno        = ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
    Rend_Base_Con_Floral   = round(Rend_Base_Turno, 2),
    Rend_Turno_Aplicar     = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),
    Motivo_Ajuste          = "Ajuste S-1"
  )

cat("rend_turno M1:\n")
print(rend_turno %>% select(Turno, Area_Total_Turno, Proy_Kg, Rend_Base_Turno, Rend_Turno_Aplicar, Turno_Completo_Lun_Mar))

# 7. valvulas_calc left_join rend_turno
v_calc <- v_calc %>%
  left_join(
    rend_turno %>% select(Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Base_Turno, Esta_Proyectado, Turno_Completo_Lun_Mar, Turno_Completo_1ra, Motivo_Ajuste, Rend_Ant_Turno, Rend_Proy),
    by = c("Módulo", "Turno", "Variedad")
  )

# 8. reg_regular:
reg_regular <- v_calc %>%
  filter(coalesce(Esta_Proyectado, FALSE), !Turno_Completo_Lun_Mar, !coalesce(Turno_Completo_1ra, FALSE)) %>%
  mutate(
    Ha_Regular = Área,
    Dia_Asignado = Dia_Habitual_Calc, # "Martes"
    Kg_Asignado = round(Ha_Regular * Rend_Turno_Aplicar, 2)
  ) %>%
  filter(Kg_Asignado > 0, Ha_Regular > 0, Dia_Asignado %in% dias_ajustables_nom)

cat("reg_regular rows:", nrow(reg_regular), "\n")
print(reg_regular %>% select(Módulo, Turno, Dia_Asignado, Kg_Asignado, Ha_Regular))

# 9. auto_ediciones:
auto_ediciones <- reg_regular %>%
  group_by(Sector, Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
  summarise(Valor = sum(Kg_Asignado), Ha_Asignada = sum(Ha_Regular), .groups = "drop")

cat("auto_ediciones rows:", nrow(auto_ediciones), "\n")
print(auto_ediciones)
