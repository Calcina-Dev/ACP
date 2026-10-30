source("d:/ACP/scratch/test_flowchart_complete.R")

# Simular los 3 componentes tal como están en app.R ahora mismo
valvulas_calc <- valvulas_calc %>%
  left_join(
    rend_turno %>% select(
      Módulo, Turno, Variedad, Rend_Turno_Aplicar, Rend_Ant_Turno, Rend_Sem_Actual, Rend_Proy,
      Tiene_Cosecha_Act, Semana_Ant_No_Finalizo, Turno_Completo_Lun_Mar, Es_Ciclo_Corto,
      Cosecho_Lunes, Cosecho_Martes, Ultimo_Dia_Cosecha_Ant, Prop_Avance_Turno
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

reg_regular <- valvulas_calc %>%
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
    Kg_Asignado = round(Ha_Regular * coalesce(Rend_Turno_Aplicar, 0), 2)
  ) %>%
  filter(Kg_Asignado > 0, Dia_Asignado %in% dias_ajustables_nom)

auto_res <- reg_regular %>%
  group_by(Módulo, Turno, Variedad, Dia_Asignado) %>%
  summarise(Kg_Auto = sum(Kg_Asignado), Ha_Auto = sum(Ha_Regular), .groups = "drop")

ver_22 <- rend_turno %>%
  filter(Ha_Martes_Turno > 0) %>%
  select(Módulo, Turno, Variedad, Area_Total_Turno, Ha_Lunes_Turno, Ha_Martes_Turno, Ha_Faltante_Turno, Kg_Lunes_Real, Kg_Martes_Real, Rend_Turno_Aplicar, Turno_Completo_Lun_Mar) %>%
  left_join(auto_res, by = c("Módulo", "Turno", "Variedad"))

print(as.data.frame(ver_22))
