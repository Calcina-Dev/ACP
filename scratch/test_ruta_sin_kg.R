source("d:/ACP/scratch/test_flowchart_complete.R")

# Revisar cómo quedan los 22 turnos con Ha_Martes_Turno > 0 y Kg_Martes_Real == 0
# cuando:
# 1) Rend_Sem_Actual divide Kg_Real_Sem solo entre las Ha de los días que YA tienen Kg en SAP
# 2) Las Ha que ya tienen Ruta en un día sin Kg en SAP (Ha_Ruta_Sin_Kg) reciben su ajuste de kilos en ese día y se marcan con Tipo_Edicion = "ruta_sin_kg"

rend_turno2 <- turno_avance_ha %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(proy_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(rend_ant_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Real_Sem        = coalesce(Kg_Real_Sem, 0),
    Kg_Lunes_Real      = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real     = coalesce(Kg_Martes_Real, 0),
    Proy_Kg            = coalesce(Proy_Kg, 0),
    Ha_Ant_Turno       = coalesce(Ha_Ant_Turno, 0),
    Kg_Ant_Turno       = coalesce(Kg_Ant_Turno, 0),
    Rend_Ant_Turno     = coalesce(Rend_Ant_Turno, 0),
    Sap_Cosecho_Lunes  = coalesce(Sap_Cosecho_Lunes, FALSE),
    Sap_Cosecho_Martes = coalesce(Sap_Cosecho_Martes, FALSE),
    Rend_Proy          = ifelse(Area_Total_Turno > 0 & Proy_Kg > 0, Proy_Kg / Area_Total_Turno, 0),

    es_remanente_minimo = (Ha_Prog_Turno == 0) & (Kg_Real_Sem > 0) &
                          ((Kg_Real_Sem / pmax(1, Proy_Kg)) < 0.15 | (Kg_Real_Sem / pmax(0.1, Area_Total_Turno)) < 50),

    Cosecho_Lunes     = Sap_Cosecho_Lunes | (Ha_Lunes_Turno > 0) | (Kg_Lunes_Real > 0),
    Cosecho_Martes    = Sap_Cosecho_Martes | (Ha_Martes_Turno > 0) | (Kg_Martes_Real > 0),
    Tiene_Cosecha_Act = !es_remanente_minimo & (Cosecho_Lunes | Cosecho_Martes | Kg_Real_Sem > 0 | Ha_Prog_Turno > 0),

    Ha_Recorrida_Turno = case_when(
      Ha_Prog_Turno > 0   ~ Ha_Prog_Turno,
      es_remanente_minimo ~ round(pmin(Area_Total_Turno, Kg_Real_Sem / pmax(1, Rend_Proy)), 2),
      Kg_Real_Sem > 0     ~ Area_Total_Turno,
      TRUE                ~ 0
    ),

    Prop_Avance_Turno = ifelse(Area_Total_Turno > 0, pmin(1, Ha_Recorrida_Turno / Area_Total_Turno), 0),

    Semana_Ant_No_Finalizo = (Ha_Ant_Turno > 0) &
                             (Ha_Ant_Turno < 0.95 * Area_Total_Turno) &
                             (coalesce(Ultimo_Dia_Cosecha_Ant, "") %in% c("Sábado", "Viernes")) &
                             (Cosecho_Lunes | Cosecho_Martes),

    Turno_Completo_Lun_Mar = !Semana_Ant_No_Finalizo & Tiene_Cosecha_Act &
                             ((Ha_Recorrida_Turno >= 0.95 * Area_Total_Turno) | (Area_Total_Turno - Ha_Recorrida_Turno <= 0.15)),
    Es_Ciclo_Corto         = (Variedad == "ROCIO") | (Frecuencia_Dias <= 4L),

    # Hectáreas de los días que efectivamente ya tienen kilos en SAP
    Ha_Con_Kg_Sap = ifelse(Kg_Lunes_Real > 0, ifelse(Ha_Lunes_Turno > 0, Ha_Lunes_Turno, Area_Total_Turno), 0) +
                    ifelse(Kg_Martes_Real > 0, ifelse(Ha_Martes_Turno > 0, Ha_Martes_Turno, Area_Total_Turno), 0),

    Rend_Sem_Actual_Bruto = case_when(
      Kg_Real_Sem > 0 & Ha_Con_Kg_Sap > 0      ~ Kg_Real_Sem / Ha_Con_Kg_Sap,
      Kg_Real_Sem > 0 & Ha_Recorrida_Turno > 0 ~ Kg_Real_Sem / Ha_Recorrida_Turno,
      TRUE                                     ~ 0
    ),
    Rend_Sem_Actual = case_when(
      Rend_Sem_Actual_Bruto > 0 ~ Rend_Sem_Actual_Bruto,
      Rend_Ant_Turno > 0        ~ Rend_Ant_Turno,
      TRUE                      ~ Rend_Proy
    ),

    # Rendimiento base del turno (incluso si Turno_Completo_Lun_Mar == TRUE pero falta cargar Kg en SAP)
    Rend_Base_Turno = case_when(
      Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
      !Tiene_Cosecha_Act ~ aplicar_castigo_rend(Rend_Proy, Rend_Ant_Turno),
      Tiene_Cosecha_Act & Prop_Avance_Turno > 0.30 ~ aplicar_castigo_rend(Rend_Sem_Actual, Rend_Ant_Turno),
      Tiene_Cosecha_Act & Prop_Avance_Turno <= 0.30 ~ aplicar_castigo_rend(Rend_Proy, Rend_Ant_Turno),
      TRUE ~ Rend_Proy
    ),

    # Para pasadas futuras regulares: si Turno_Completo_Lun_Mar y !Es_Ciclo_Corto -> 0
    Rend_Turno_Aplicar = ifelse(Turno_Completo_Lun_Mar & !Es_Ciclo_Corto, 0, Rend_Base_Turno)
  )

cat("Comparación de Rend_Base_Turno en los 22 turnos con Ruta en Martes pero sin Kg SAP en Martes:\n")
print(as.data.frame(
  rend_turno2 %>%
    filter(Ha_Martes_Turno > 0 & Kg_Martes_Real == 0) %>%
    select(Módulo, Turno, Variedad, Area_Total_Turno, Ha_Lunes_Turno, Ha_Martes_Turno, Kg_Lunes_Real, Rend_Ant_Turno, Rend_Proy, Rend_Sem_Actual, Rend_Base_Turno, Turno_Completo_Lun_Mar)
))
