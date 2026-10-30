source("d:/ACP/scratch/test_color_ruta_sin_kg.R")

cat("=== DETALLE VÁLVULA POR VÁLVULA: MÓDULO 4, TURNO 7 Y TURNO 8 ===\n")
v_t7_t8 <- valvulas_calc2 %>%
  filter(Módulo == "4", Turno %in% c("7", "8")) %>%
  select(Turno, codigo_norm, Área, Dia_7d, Ha_Total_Ant, Ha_Lunes_Rc, Ha_Martes_Rc, Ha_Prog, Ha_Faltante, Valvula_Cerro_En_Lun_Mar, Ha_Cerrada_Lun_Mar, Rend_Turno_Aplicar)
print(as.data.frame(v_t7_t8))

cat("\n--- Componente 1 (reg_regular) en M4 T7 y T8 ---\n")
r1 <- reg_regular %>%
  filter(Módulo == "4", Turno %in% c("7", "8")) %>%
  select(Turno, codigo_norm, Área, Ha_Regular, Dia_Asignado, Rend_Turno_Aplicar, Kg_Asignado)
print(as.data.frame(r1))

cat("\n--- Componente 2 (reg_2da_remanente) en M4 T7 y T8 ---\n")
r2 <- valvulas_calc2 %>%
  filter(Semana_Ant_No_Finalizo, Valvula_Cerro_En_Lun_Mar, Ha_Cerrada_Lun_Mar > 0, Módulo == "4", Turno %in% c("7", "8")) %>%
  mutate(
    Ha_2da = Ha_Cerrada_Lun_Mar,
    Dia_Asignado = ifelse(Valvula_Cosecho_Lunes, "Viernes", "Sábado"),
    Factor_Crecimiento = 4 / 7,
    Kg_Asignado = round(Ha_2da * Rend_Turno_Aplicar * Factor_Crecimiento, 2)
  ) %>%
  select(Turno, codigo_norm, Área, Ha_2da, Dia_Asignado, Factor_Crecimiento, Kg_Asignado)
print(as.data.frame(r2))
