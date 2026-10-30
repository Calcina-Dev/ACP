source("d:/ACP/scratch/test_flowchart_complete.R")

rc_sem <- rc %>% filter(Semana == 44)
cat("Resumen de Ha por día en Rutas de Cosecha Sem 44:\n")
print(colSums(rc_sem[, c("Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado", "Kg_Lunes", "Kg_Martes")], na.rm = TRUE))

# Ver por turno qué días tienen Ha en Rutas vs Kg en SAP
rc_turno_dia <- rc_sem %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo = case_when(
      Módulo %in% c("13.1", "13.2") ~ "13",
      Módulo %in% c("14.1", "14.2") ~ "14",
      Módulo == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      Módulo == "9" & Turno_num >= 12            ~ "9.1",
      Módulo == "11" & between(Turno_num, 1, 12) ~ "11.1",
      Módulo == "11" & Turno_num >= 13           ~ "11.2",
      TRUE                                       ~ trimws(as.character(Módulo))
    ),
    Turno = trimws(as.character(Turno)),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Lun_RC = sum(Ha_Lunes, na.rm = TRUE),
    Ha_Mar_RC = sum(Ha_Martes, na.rm = TRUE),
    Kg_Lun_RC = sum(Kg_Lunes, na.rm = TRUE),
    Kg_Mar_RC = sum(Kg_Martes, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(kg_real_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Lunes_Real = coalesce(Kg_Lunes_Real, 0),
    Kg_Martes_Real = coalesce(Kg_Martes_Real, 0)
  )

cat("\nTurnos con Ha_Lun_RC > 0 pero Kg_Lunes_Real == 0:\n")
print(as.data.frame(rc_turno_dia %>% filter(Ha_Lun_RC > 0 & Kg_Lunes_Real == 0)))

cat("\nTurnos con Ha_Mar_RC > 0 pero Kg_Martes_Real == 0:\n")
print(as.data.frame(rc_turno_dia %>% filter(Ha_Mar_RC > 0 & Kg_Martes_Real == 0)))
