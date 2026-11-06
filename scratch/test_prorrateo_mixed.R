library(dplyr)
library(tibble)

cat("=== TESTING FIXED + PRORRATED MIXED LOGIC ===\n")
area_turno <- 5.11
ha_pool_1ra <- 5.11

# Miércoles entered with kgha: 1.50 ha, 150 kg (100 kg/ha)
ed_miercoles <- tibble(
  Campaña = "2026", Semana = 45, Sector = "Sector 3", Módulo = "Módulo 13", Turno = "Turno 1",
  Variedad = "ROCIO", Campo = "Miércoles", Valor = 150, Ha_Asignada = 1.50,
  Ha_Asignada_1ra = 1.50, Kg_Asignado_1ra = 150,
  Ha_Asignada_2da = 0, Kg_Asignado_2da = 0, Tipo_Edicion = "manual",
  Modo_Ingreso = "kgha", Motivo_Ajuste = "Manual (100 kg/ha en 1.50 ha)", Fecha_Edicion = Sys.time()
)

# Jueves entered with kilos directos: 200 kg
val_jueves <- 200
ed_otros_jueves <- ed_miercoles
ed_fijas <- ed_otros_jueves %>% filter(coalesce(Modo_Ingreso, "") == "kgha")
ha_fijas <- sum(ed_fijas$Ha_Asignada_1ra)
ha_pool_prorr <- max(0, round(ha_pool_1ra - ha_fijas, 2))
ed_prorr <- ed_otros_jueves %>% filter(coalesce(Modo_Ingreso, "") != "kgha")

# Since ed_prorr has 0 rows, Jueves takes all remaining ha_pool_prorr
ha_jueves <- ha_pool_prorr
cat(sprintf("Jueves toma el remanente disponible: %.2f ha\n", ha_jueves))
stopifnot(ha_jueves == 3.61)

ed_jueves <- tibble(
  Campaña = "2026", Semana = 45, Sector = "Sector 3", Módulo = "Módulo 13", Turno = "Turno 1",
  Variedad = "ROCIO", Campo = "Jueves", Valor = 200, Ha_Asignada = ha_jueves,
  Ha_Asignada_1ra = ha_jueves, Kg_Asignado_1ra = 200,
  Ha_Asignada_2da = 0, Kg_Asignado_2da = 0, Tipo_Edicion = "manual",
  Modo_Ingreso = "kilos", Motivo_Ajuste = "Manual (200 kg en 3.61 ha)", Fecha_Edicion = Sys.time()
)

# Viernes entered with kilos directos: 100 kg
val_viernes <- 100
ed_actuales <- bind_rows(ed_miercoles, ed_jueves)
ed_otros_viernes <- ed_actuales
ed_fijas <- ed_otros_viernes %>% filter(coalesce(Modo_Ingreso, "") == "kgha")
ha_fijas <- sum(ed_fijas$Ha_Asignada_1ra)
ha_pool_prorr <- max(0, round(ha_pool_1ra - ha_fijas, 2))
ed_prorr <- ed_otros_viernes %>% filter(coalesce(Modo_Ingreso, "") != "kgha")

kg_otros_sum <- sum(ed_prorr$Valor)
kg_total <- kg_otros_sum + val_viernes
ha_otros_nuevos <- numeric(nrow(ed_prorr))
for (i in seq_len(nrow(ed_prorr))) {
  ha_otros_nuevos[i] <- round(ha_pool_prorr * (ed_prorr$Valor[i] / kg_total), 2)
}
ha_viernes <- max(0, round(ha_pool_prorr - sum(ha_otros_nuevos), 2))

cat(sprintf("Resultado mixto:\n"))
cat(sprintf("  Miércoles (fijo): %.2f ha\n", ed_miercoles$Ha_Asignada_1ra))
cat(sprintf("  Jueves (prorr):   %.2f ha (Rend: %.2f kg/ha)\n", ha_otros_nuevos[1], ed_jueves$Valor / ha_otros_nuevos[1]))
cat(sprintf("  Viernes (prorr):  %.2f ha (Rend: %.2f kg/ha)\n", ha_viernes, val_viernes / ha_viernes))
cat(sprintf("  Total suma:       %.2f ha (Esperado: 5.11 ha)\n", ed_miercoles$Ha_Asignada_1ra + ha_otros_nuevos[1] + ha_viernes))

stopifnot(abs((ed_miercoles$Ha_Asignada_1ra + ha_otros_nuevos[1] + ha_viernes) - 5.11) < 0.001)
cat("=== MIXED TEST PASSED! ===\n")
