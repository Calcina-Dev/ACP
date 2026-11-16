library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

source("D:/ACP/Actualizacion Semanal/R/helpers.R", local = FALSE, echo = FALSE)

# Let's extract and run ejecutar_ajuste_semana for M1
lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8")
start_idx <- grep("ejecutar_ajuste_semana <- function", lines)[1]
end_idx   <- grep("ejecutar_deshacer_ajuste <- function", lines)[1] - 1

# Let's see what happens to M1
# We can run the body in a local environment
env <- new.env(parent = globalenv())
env$input <- list(
  filtro_campana = "2026",
  filtro_semana  = 45,
  ajustar_desde_dia = "Martes",
  ajustar_hasta_dia = "Viernes",
  filtro_modulo  = "1",
  filtro_turno   = NULL,
  filtro_variedad = NULL,
  switch_ajuste_cualquier_semana = TRUE
)
env$rv_cosecha <- function() df_cosecha
env$rv_rutas_cosecha <- function() if (exists("df_rutas_cosecha")) df_rutas_cosecha else tibble()
env$rv_modo_frecuencia <- function() "auto"
env$rv_frecuencias <- function() tibble()
env$rv_pct_rocio <- function() tibble()
env$rv_umbral_avance <- function() 15
env$rv_ajuste_valvulas <- function() readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_historico.rds")
env$guardar_ajuste_valvulas_disco <- function(x) {}
env$ediciones_manuales <- function() readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
env$guardar_ediciones_disco <- function(x) {}
env$rv_escenario_activo_id <- function() "__actual__"
env$rv_escenarios <- function() readRDS("D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds")
env$showNotification <- function(msg, type, duration) cat("NOTIFICATION [", type, "]: ", msg, "\n")
env$withProgress <- function(..., expr) eval(substitute(expr), envir = parent.frame())
env$incProgress <- function(...) {}
env$turnos_activos_filtrados <- function() {
  tibble(Sector = "6", Módulo = "1", Turno = as.character(1:5), Variedad = "SEKOYA POP")
}
env$hay_filtro_tabla_activo <- function() TRUE

# In app.R, universo_base() is a reactive
# Let's inspect how universo_base is constructed in app.R
idx_ub <- grep("universo_base <- reactive", lines)[1]
cat("universo_base starts at line:", idx_ub, "\n")
