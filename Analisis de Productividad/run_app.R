# Launcher for Analisis de Productividad
library(shiny)

cat("\n========================================================\n")
cat("  PROYECTO: ANÁLISIS DE PRODUCTIVIDAD (Shiny)\n")
cat("  Lineamientos Emil Kowalski • Reactividad en Tiempo Real\n")
cat("========================================================\n\n")

runApp(
  appDir = "d:/ACP/Analisis de Productividad",
  port = 7860,
  host = "127.0.0.1",
  launch.browser = TRUE
)
