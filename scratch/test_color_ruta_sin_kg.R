source("d:/ACP/scratch/test_ruta_sin_kg.R")

valvulas_calc2 <- valvulas_calc %>%
  left_join(
    rend_turno2 %>% select(
      Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Base_Turno, Rend_Ant_Turno, Rend_Sem_Actual, Rend_Proy,
      Tiene_Cosecha_Act, Semana_Ant_No_Finalizo, Turno_Completo_Lun_Mar, Es_Ciclo_Corto,
      Cosecho_Lunes, Cosecho_Martes, Kg_Lunes_Real, Kg_Martes_Real, Ultimo_Dia_Cosecha_Ant, Prop_Avance_Turno
    ),
    by = c("Módulo", "Turno", "Variedad")
  ) %>%
  mutate(
    Valvula_Cosecho_Lunes  = (Ha_Lunes_Rc > 0) | (Semana_Ant_No_Finalizo & Cosecho_Lunes & is.na(Dia_7d) & Ha_Prog == 0),
    Valvula_Cosecho_Martes = (Ha_Martes_Rc > 0) | (Semana_Ant_No_Finalizo & !Cosecho_Lunes & Cosecho_Martes & is.na(Dia_7d) & Ha_Prog == 0),
    Valvula_Cerro_En_Lun_Mar = Semana_Ant_No_Finalizo & (Valvula_Cosecho_Lunes | Valvula_Cosecho_Martes),
    Ha_Cosechada_Lunes = case_when(Ha_Lunes_Rc > 0 ~ Ha_Lunes_Rc, Valvula_Cosecho_Lunes ~ Área, TRUE ~ 0),
    Ha_Cosechada_Martes = case_when(Ha_Martes_Rc > 0 ~ Ha_Martes_Rc, Valvula_Cosecho_Martes ~ Área, TRUE ~ 0),
    Ha_Cerrada_Lun_Mar = pmin(Área, Ha_Cosechada_Lunes + Ha_Cosechada_Martes)
  )

# Componente 0: Kilos para hectáreas que YA TIENEN RUTA pero NO TIENEN KILOS en SAP
reg_ruta_lun <- valvulas_calc2 %>%
  filter(Ha_Lunes_Rc > 0, Kg_Lunes_Real == 0) %>%
  mutate(
    Dia_Asignado   = "Lunes",
    Kg_Asignado    = round(Ha_Lunes_Rc * coalesce(Rend_Base_Turno, 0), 2),
    Es_Ruta_Sin_Kg = TRUE
  ) %>%
  filter(Kg_Asignado > 0)

reg_ruta_mar <- valvulas_calc2 %>%
  filter(Ha_Martes_Rc > 0, Kg_Martes_Real == 0) %>%
  mutate(
    Dia_Asignado   = "Martes",
    Kg_Asignado    = round(Ha_Martes_Rc * coalesce(Rend_Base_Turno, 0), 2),
    Es_Ruta_Sin_Kg = TRUE
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

reg_regular <- valvulas_calc2 %>%
  filter(!Turno_Completo_Lun_Mar) %>%
  mutate(
    Ha_Regular = case_when(
      Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar & Ha_Cerrada_Lun_Mar >= 0.90 * Área ~ 0,
      Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ pmax(0, Área - Ha_Cerrada_Lun_Mar),
      Semana_Ant_No_Finalizo ~ Área,
      Tiene_Cosecha_Act ~ Ha_Faltante,
      TRUE ~ Área
    ),
    Dia_Asignado = case_when(
      Semana_Ant_No_Finalizo & Tiene_Ciclo_Previo_Valv ~ Dia_Habitual_Calc,
      Semana_Ant_No_Finalizo & !Tiene_Ciclo_Previo_Valv & !Valvula_Cerro_En_Lun_Mar ~ dia_inicio_nom,
      Semana_Ant_No_Finalizo ~ dia_inicio_nom,
      Tiene_Cosecha_Act & Ha_Prog > 0 & Ha_Prog < 0.90 * Área ~ dia_inicio_nom,
      TRUE ~ Dia_Habitual_Calc
    ),
    Kg_Asignado    = round(Ha_Regular * coalesce(Rend_Turno_Aplicar, 0), 2),
    Es_Ruta_Sin_Kg = FALSE
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

comb <- bind_rows(
  reg_ruta_lun %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Es_Ruta_Sin_Kg),
  reg_ruta_mar %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Es_Ruta_Sin_Kg),
  reg_regular %>% select(Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Es_Ruta_Sin_Kg)
) %>%
  group_by(Módulo, Turno, Variedad, Campo = Dia_Asignado) %>%
  summarise(
    Valor = round(sum(Kg_Asignado, na.rm = TRUE), 2),
    Tipo_Edicion = ifelse(any(Es_Ruta_Sin_Kg), "ruta_sin_kg", "auto"),
    .groups = "drop"
  )

cat("\nCeldas marcadas como 'ruta_sin_kg' en la Semana 44:\n")
print(as.data.frame(comb %>% filter(Tipo_Edicion == "ruta_sin_kg")))
