suppressMessages({
  library(dplyr)
})

e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)

cat("Objects in RData:", ls(e), "\n")
if ("df_valvulas" %in% ls(e)) {
  valvs <- e$df_valvulas
  cat("df_valvulas columns:\n")
  print(names(valvs))
  print(head(valvs))
}

# Check areas_por_turno logic
areas_turnos <- e$df_valvulas %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Área_Total = sum(Área, na.rm = TRUE), .groups = "drop")

print(head(areas_turnos))
