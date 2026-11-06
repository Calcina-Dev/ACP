suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Mock data
ed_test <- tibble(
  Campaña = c("2026", "2026", "2026", "2026", "2026"),
  Semana  = c(45, 45, 45, 45, 45),
  Sector  = c("9", "9", "9", "9", "3"),
  Módulo  = c("9.2", "9.2", "11.1", "11.1", "13"),
  Turno   = c("1", "2", "1", "2", "1"),
  Variedad= c("ROCIO", "ROCIO", "VENTURA", "VENTURA", "ROCIO"),
  Campo   = c("Jueves", "Viernes", "Jueves", "Viernes", "Lunes"),
  Valor   = c(100, 200, 300, 400, 500),
  Tipo_Edicion = c("manual", "auto", "manual", "auto", "manual")
)

valvs_test <- tibble(
  Campaña = c("2026", "2026"),
  Semana  = c(45, 45),
  Sector  = c("9", "9"),
  Módulo  = c("9.2", "11.1"),
  Turno   = c("1", "2"),
  Variedad= c("ROCIO", "VENTURA"),
  Dia_Asignado = c("Jueves", "Viernes"),
  Kg_Asignado  = c(150, 400),
  Ha_Asignada  = c(2, 4),
  Num_Pasada   = c(1L, 1L),
  Motivo_Ajuste= c("Auto test", "Auto test")
)

cat("=== Initial ed_test ===\n")
print(ed_test[, c("Módulo", "Turno", "Campo", "Valor", "Tipo_Edicion")])

# 1. Test Borrar Ajuste Manual filtrando solo Módulo 9.2
claves_filtro <- c("9.2___1___ROCIO", "9.2___2___ROCIO")
es_manual_sem <- ed_test$Campaña == "2026" & ed_test$Semana == 45 & ed_test$Tipo_Edicion == "manual"
claves_ed <- paste(trimws(ed_test$Módulo), trimws(ed_test$Turno), trimws(ed_test$Variedad), sep = "___")

a_borrar <- es_manual_sem & (claves_ed %in% claves_filtro)
cat("Indices a borrar de manuales para Modulo 9.2:", which(a_borrar), "\n")
stopifnot(length(which(a_borrar)) == 1) # Only row 1 (9.2 Turno 1)

# Check restoration of auto if exists in valvs_test
ed_a_eliminar <- ed_test[a_borrar, ]
claves_celdas_elim <- paste(trimws(ed_a_eliminar$Módulo), trimws(ed_a_eliminar$Turno), trimws(ed_a_eliminar$Variedad), trimws(ed_a_eliminar$Campo), sep = "___")
claves_valvs <- paste(trimws(valvs_test$Módulo), trimws(valvs_test$Turno), trimws(valvs_test$Variedad), trimws(valvs_test$Dia_Asignado), sep = "___")

valvs_restaurar <- valvs_test[claves_valvs %in% claves_celdas_elim, ]
cat("Valvulas restauradas count:", nrow(valvs_restaurar), "\n")
stopifnot(nrow(valvs_restaurar) == 1) # Row 1 restored!

# 2. Test Deshacer Ajuste filtrando solo Módulo 11.1
claves_filtro_11 <- c("11.1___1___VENTURA", "11.1___2___VENTURA")
es_auto_sem <- ed_test$Campaña == "2026" & ed_test$Semana == 45 & ed_test$Tipo_Edicion == "auto"
a_borrar_auto <- es_auto_sem & (claves_ed %in% claves_filtro_11)
cat("Indices a borrar de auto para Modulo 11.1:", which(a_borrar_auto), "\n")
stopifnot(length(which(a_borrar_auto)) == 1) # Only row 4 (11.1 Turno 2)
# Row 2 (9.2 Turno 2 auto) remains untouched!
stopifnot(!2 %in% which(a_borrar_auto))

cat("=== ALL UNIT TESTS PASSED SUCCESSFULLY! ===\n")
