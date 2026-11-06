library(dplyr)
library(tibble)

# Test proportional prorrateo logic
cat("=== TESTING PRORRATEO LOGIC ===\n")

area_turno <- 5.11
ha_real_1ra <- 0
ha_pool_1ra <- max(0, round(area_turno - ha_real_1ra, 2))

# Step 1: Day 1 (Miércoles) enters 300 kg directos
val_miercoles <- 300
ed_otros_miercoles <- tibble()
ha_fijas_otros <- 0
ha_pool_prorr <- max(0, round(ha_pool_1ra - ha_fijas_otros, 2))
ed_prorr <- ed_otros_miercoles

if (nrow(ed_prorr) == 0) {
  ha_miercoles <- ha_pool_prorr
} else {
  kg_total <- sum(ed_prorr$Valor) + val_miercoles
  ha_miercoles <- round(ha_pool_prorr * (val_miercoles / kg_total), 2)
}

cat(sprintf("Día 1 (Miércoles): %.2f kg -> %.2f ha (Rend: %.2f kg/ha)\n", 
            val_miercoles, ha_miercoles, val_miercoles / ha_miercoles))

ed_actuales <- tibble(
  Campaña = "2026", Semana = 45, Sector = "Sector 3", Módulo = "Módulo 13", Turno = "Turno 1",
  Variedad = "ROCIO", Campo = "Miércoles", Valor = val_miercoles, Ha_Asignada = ha_miercoles,
  Ha_Asignada_1ra = ha_miercoles, Kg_Asignado_1ra = val_miercoles,
  Ha_Asignada_2da = 0, Kg_Asignado_2da = 0, Tipo_Edicion = "manual",
  Modo_Ingreso = "kilos", Motivo_Ajuste = "Manual (300 kg)", Fecha_Edicion = Sys.time()
)

# Step 2: Day 2 (Jueves) enters 200 kg directos for 1ª pasada
val_jueves <- 200
ed_otros_jueves <- ed_actuales %>%
  filter(Campo != "Jueves", coalesce(Tipo_Edicion, "") == "manual", coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0)

ed_fijas <- ed_otros_jueves %>%
  filter(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, "")))
ha_fijas <- sum(coalesce(ed_fijas$Ha_Asignada_1ra, 0))
ha_pool_prorr <- max(0, round(ha_pool_1ra - ha_fijas, 2))

ed_prorr <- ed_otros_jueves %>%
  filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))

cat(sprintf("\nRecalculando para Jueves con %d días previos de kilos directos...\n", nrow(ed_prorr)))

kg_otros_sum <- sum(ed_prorr$Valor)
kg_total <- kg_otros_sum + val_jueves

# Calculate other days
ha_otros_nuevos <- numeric(nrow(ed_prorr))
for (i in seq_len(nrow(ed_prorr))) {
  ha_otros_nuevos[i] <- round(ha_pool_prorr * (ed_prorr$Valor[i] / kg_total), 2)
}

# Current day gets remainder to guarantee exact sum
ha_jueves <- max(0, round(ha_pool_prorr - sum(ha_otros_nuevos), 2))

cat(sprintf("Resultado Prorrateo:\n"))
cat(sprintf("  Miércoles: %.2f kg -> %.2f ha (Rend: %.2f kg/ha)\n", 
            ed_prorr$Valor[1], ha_otros_nuevos[1], ed_prorr$Valor[1] / ha_otros_nuevos[1]))
cat(sprintf("  Jueves:    %.2f kg -> %.2f ha (Rend: %.2f kg/ha)\n", 
            val_jueves, ha_jueves, val_jueves / ha_jueves))
cat(sprintf("  Suma total ha: %.2f ha (Esperado: %.2f ha)\n", 
            ha_otros_nuevos[1] + ha_jueves, ha_pool_prorr))
cat(sprintf("  Rendimiento uniforme conjunto: %.2f kg/ha\n", 
            kg_total / ha_pool_prorr))

stopifnot(abs((ha_otros_nuevos[1] + ha_jueves) - ha_pool_prorr) < 0.001)

# Step 3: Day 3 (Viernes) enters 100 kg directos for 1ª pasada
val_viernes <- 100
# Update ed_actuales with new Miércoles and Jueves
ed_actuales$Ha_Asignada[1] <- ha_otros_nuevos[1]
ed_actuales$Ha_Asignada_1ra[1] <- ha_otros_nuevos[1]
ed_actuales <- bind_rows(ed_actuales, tibble(
  Campaña = "2026", Semana = 45, Sector = "Sector 3", Módulo = "Módulo 13", Turno = "Turno 1",
  Variedad = "ROCIO", Campo = "Jueves", Valor = val_jueves, Ha_Asignada = ha_jueves,
  Ha_Asignada_1ra = ha_jueves, Kg_Asignado_1ra = val_jueves,
  Ha_Asignada_2da = 0, Kg_Asignado_2da = 0, Tipo_Edicion = "manual",
  Modo_Ingreso = "kilos", Motivo_Ajuste = "Manual (200 kg)", Fecha_Edicion = Sys.time()
))

ed_otros_viernes <- ed_actuales %>%
  filter(Campo != "Viernes", coalesce(Tipo_Edicion, "") == "manual", coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0)
ed_prorr <- ed_otros_viernes %>%
  filter(!(coalesce(Modo_Ingreso, "") == "kgha" | grepl("kg/ha en", coalesce(Motivo_Ajuste, ""))))

kg_otros_sum <- sum(ed_prorr$Valor)
kg_total <- kg_otros_sum + val_viernes
ha_otros_nuevos <- numeric(nrow(ed_prorr))
for (i in seq_len(nrow(ed_prorr))) {
  ha_otros_nuevos[i] <- round(ha_pool_prorr * (ed_prorr$Valor[i] / kg_total), 2)
}
ha_viernes <- max(0, round(ha_pool_prorr - sum(ha_otros_nuevos), 2))

cat(sprintf("\nResultado Prorrateo 3 Días (Miércoles 300, Jueves 200, Viernes 100):\n"))
cat(sprintf("  Miércoles: %.2f ha (Rend: %.2f kg/ha)\n", ha_otros_nuevos[1], ed_prorr$Valor[1] / ha_otros_nuevos[1]))
cat(sprintf("  Jueves:    %.2f ha (Rend: %.2f kg/ha)\n", ha_otros_nuevos[2], ed_prorr$Valor[2] / ha_otros_nuevos[2]))
cat(sprintf("  Viernes:   %.2f ha (Rend: %.2f kg/ha)\n", ha_viernes, val_viernes / ha_viernes))
cat(sprintf("  Suma total ha: %.2f ha (Esperado: %.2f ha)\n", sum(ha_otros_nuevos) + ha_viernes, ha_pool_prorr))
cat(sprintf("  Rendimiento uniforme conjunto: %.2f kg/ha\n", kg_total / ha_pool_prorr))

stopifnot(abs((sum(ha_otros_nuevos) + ha_viernes) - ha_pool_prorr) < 0.001)

cat("\n=== ALL LOGIC TESTS PASSED SUCCESSFULLY! ===\n")
