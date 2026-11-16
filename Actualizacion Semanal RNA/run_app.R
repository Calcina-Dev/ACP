# ==============================================================================
# INICIADOR: CONTROL SEMANAL DE COSECHA - MODELO RNA
# Agrícola Cerro Prieto (ACP)
# ==============================================================================

message("Iniciando Control Semanal de Cosecha (Modelo RNA) en http://127.0.0.1:4242 ...")
shiny::runApp(
  appDir = "d:/ACP/Actualizacion Semanal RNA",
  port = 4242,
  launch.browser = TRUE,
  host = "127.0.0.1"
)
