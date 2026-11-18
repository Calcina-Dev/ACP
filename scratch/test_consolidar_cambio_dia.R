suppressPackageStartupMessages(library(dplyr))

# Simulate M3 T2 values
area_turno <- 17.24
dia_origen <- "Martes"
dia_destino <- "Miércoles"

# Before moving:
# Martes:
kg_orig <- 2515.88
ha_orig <- 12.32
# Miércoles:
kg_dest_prev <- 1004.72
ha_dest_prev <- 4.92

# Moved kilos with frequency (+8.7%):
kg_movido <- 2736.00
ha_movida <- 12.32

# Consolidation:
kg_final_dest <- round(kg_dest_prev + kg_movido, 2)
ha_final_dest <- round(pmin(area_turno, ha_dest_prev + ha_movida), 2)
rend_final_dest <- round(kg_final_dest / ha_final_dest, 1)

cat("Consolidated Kilos:", kg_final_dest, "kg\n")
cat("Consolidated Hectares:", ha_final_dest, "ha\n")
cat("Recalculated Yield:", rend_final_dest, "kg/ha\n")

stopifnot(abs(kg_final_dest - 3740.72) < 0.01)
stopifnot(abs(ha_final_dest - 17.24) < 0.01)
stopifnot(abs(rend_final_dest - 217.0) < 0.1)

cat("✔ TEST PASSED: 3,740.72 kg, 17.24 ha, 217.0 kg/ha!\n")
