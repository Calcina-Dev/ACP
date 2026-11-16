suppressMessages({
  library(dplyr)
})

# Load the local RData as app.R does
e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)

df_p <- e$df_proyecciones
cat("Loaded df_proyecciones rows:", nrow(df_p), "\n")

p46 <- df_p %>% filter(Semana == 46)
cat("Semana 46 rows:", nrow(p46), "\n")
cat("Semana 46 Proyección Sum:", sum(p46$Proyección, na.rm = TRUE), "\n")
cat("Formatted for KPI card:", format(round(sum(p46$Proyección, na.rm = TRUE), 1), big.mark = ","), "kg\n")
