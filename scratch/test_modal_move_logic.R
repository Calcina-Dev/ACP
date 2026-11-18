suppressPackageStartupMessages(library(dplyr))

# Test Case 1: Moving Martes (2515.88 kg in 12.32 ha -> 2736 kg) to Miercoles (which has 1004.72 kg in 4.92 ha)
area_turno <- 17.24
dia_origen <- "Martes"
dia_destino <- "Miércoles"
val_final <- 2736.00
ha_final <- 12.32

# Mock previous data in Miercoles
kg_dest_prev <- 1004.72
ha_dest_prev <- 4.92

val_guardar_dest <- round(val_final + kg_dest_prev, 2)
ha_guardar_dest  <- round(pmin(area_turno, ha_final + ha_dest_prev), 2)
rend_guardar_dest <- if (ha_guardar_dest > 0) round(val_guardar_dest / ha_guardar_dest, 1) else 0

cat("--- TEST 1: Move with existing destination harvest ---\n")
cat("Kilos guardados:", val_guardar_dest, "(Esperado: 3740.72)\n")
cat("Ha guardadas:", ha_guardar_dest, "(Esperado: 17.24)\n")
cat("Rendimiento:", rend_guardar_dest, "(Esperado: 217.0)\n")

stopifnot(abs(val_guardar_dest - 3740.72) < 0.01)
stopifnot(abs(ha_guardar_dest - 17.24) < 0.01)
stopifnot(abs(rend_guardar_dest - 217.0) < 0.1)

# Test Case 2: Moving to a day with 0 kg
dia_destino_vacio <- "Jueves"
kg_dest_vacio <- 0
ha_dest_vacio <- 0

val_vacio <- round(val_final + kg_dest_vacio, 2)
ha_vacio  <- round(pmin(area_turno, ha_final + ha_dest_vacio), 2)
rend_vacio <- if (ha_vacio > 0) round(val_vacio / ha_vacio, 1) else 0

cat("\n--- TEST 2: Move to empty day ---\n")
cat("Kilos guardados:", val_vacio, "(Esperado: 2736)\n")
cat("Ha guardadas:", ha_vacio, "(Esperado: 12.32)\n")
cat("Rendimiento:", rend_vacio, "(Esperado: 222.1)\n")

stopifnot(abs(val_vacio - 2736.00) < 0.01)
stopifnot(abs(ha_vacio - 12.32) < 0.01)
stopifnot(abs(rend_vacio - 222.1) < 0.1)

# Test Case 3: In-place edit (dia_destino == dia_origen)
dia_destino_mismo <- "Martes"
val_mismo <- if (dia_destino_mismo != dia_origen) round(val_final + kg_dest_prev, 2) else round(val_final, 2)
ha_mismo <- if (dia_destino_mismo != dia_origen) round(pmin(area_turno, ha_final + ha_dest_prev), 2) else round(ha_final, 2)

cat("\n--- TEST 3: In-place edit ---\n")
cat("Kilos guardados:", val_mismo, "(Esperado: 2736)\n")
cat("Ha guardadas:", ha_mismo, "(Esperado: 12.32)\n")

stopifnot(abs(val_mismo - 2736.00) < 0.01)
stopifnot(abs(ha_mismo - 12.32) < 0.01)

cat("\n>>> ALL TESTS PASSED SUCCESSFULLY! <<<\n")
