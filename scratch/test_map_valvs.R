suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Cargar las funciones o datos
source("Actualizacion Semanal/global.R", chdir = TRUE)

valvs_ajuste_tot <- readRDS('Actualizacion Semanal/ajuste_valvulas_semana.rds')
valvs_ajuste_sem <- valvs_ajuste_tot %>% filter(as.numeric(Semana) == 46)
ed_totales <- readRDS('Actualizacion Semanal/ediciones_manuales_cosecha.rds')

payload <- obtener_datos_mapa_semana(
  sem_num = 46,
  pasada_filtro = "1",
  grupos_filtro = character(0),
  mods_filtro = "5",
  turnos_filtro = c("9", "10"),
  vars_filtro = character(0),
  modo_mapa = "ajustado",
  df_ajuste_valvs = valvs_ajuste_sem,
  df_ediciones = ed_totales
)

cat("Valvulas in payload:\n")
for (v in payload$valvulas) {
  cat(sprintf("Valvula %s: Pct_Miercoles=%.1f, Pct_Jueves=%.1f, Kg_Miercoles=%.1f, Kg_Jueves=%.1f\n",
              v$codigo, v$Pct_Miercoles, v$Pct_Jueves, v$Kg_Miercoles, v$Kg_Jueves))
}
