# ==============================================================================
# SCRIPT DE PUBLICACIÓN / DESPLIEGUE A SHINYAPPS.IO
# Aplicación: ruta_cosecha
# Cuenta: acp-proyecciones
# ==============================================================================

# 1. Asegurar ruta de Quarto y dependencias
quarto_path <- "C:\\Program Files\\RStudio\\resources\\app\\bin\\quarto\\bin"
if (dir.exists(quarto_path) && !grepl("quarto", Sys.getenv("PATH"), ignore.case = TRUE)) {
  Sys.setenv(PATH = paste0(quarto_path, ";", Sys.getenv("PATH")))
}

if (!"rsconnect" %in% installed.packages()[, "Package"]) {
  install.packages("rsconnect")
}
library(rsconnect)

cat("\n========================================================\n")
cat("🚀 INICIANDO DESPLIEGUE A SHINYAPPS.IO (acp-proyecciones)\n")
cat("========================================================\n\n")

# 2. Archivos esenciales empaquetados para la nube
# Incluye la data maestra como respaldo sólido, que además se actualiza dinámicamente desde Google Drive
archivos_despliegue <- c(
  "app_mapa.R",
  "datos_maestros_cosecha_2026.RData",
  "logo_cerro_prieto_b64.R",
  "valvulas_normalizadas.rds",
  "www"
)

# Filtrar solo los que existen actualmente
archivos_existentes <- archivos_despliegue[file.exists(file.path("D:/modulo_mapa_valvulas", archivos_despliegue)) | 
                                           file.exists(archivos_despliegue)]

cat("Archivos incluidos en el despliegue:\n")
print(archivos_existentes)
cat("\nSubiendo paquete a shinyapps.io...\n")

# 3. Ejecución del despliegue
rsconnect::deployApp(
  appDir        = "D:/modulo_mapa_valvulas",
  appPrimaryDoc = "app_mapa.R",
  appName       = "ruta_cosecha",
  appTitle      = "Ruta de Cosecha - Mapa 2D",
  account       = "acp-proyecciones",
  appFiles      = archivos_existentes,
  forceUpdate   = TRUE,
  lint          = FALSE
)

cat("\n✅ ¡Despliegue completado con éxito!\n")
cat("🔗 URL: https://acp-proyecciones.shinyapps.io/ruta_cosecha/\n\n")
