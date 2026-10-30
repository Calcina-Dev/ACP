suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

source("d:/ACP/Actualizacion Semanal/app.R", local = TRUE)

ed <- cargar_ediciones_disco()

# For Semana 44 Módulo 4:
# We know the exact Ha_Asignada for each record:
m4_ha_map <- tibble::tribble(
  ~Turno, ~Campo, ~Ha_Asignada,
  "1", "Martes", 3.20,
  "1", "Miércoles", 3.22,
  "2", "Martes", 3.19,
  "2", "Miércoles", 3.20,
  "3", "Jueves", 3.22,
  "3", "Viernes", 3.22,
  "4", "Jueves", 3.21,
  "4", "Viernes", 3.20,
  "5", "Jueves", 3.23,
  "5", "Viernes", 3.22,
  "6", "Jueves", 3.22,
  "6", "Viernes", 1.62,
  "6", "Sábado", 1.58,
  "7", "Viernes", 1.94,
  "7", "Sábado", 4.53,
  "8", "Viernes", 1.55,
  "8", "Sábado", 3.97
)

ed_updated <- ed %>%
  left_join(
    m4_ha_map %>% mutate(Módulo = "4"),
    by = c("Módulo", "Turno", "Campo"),
    suffix = c("", "_new")
  ) %>%
  mutate(
    Ha_Asignada = coalesce(Ha_Asignada_new, Ha_Asignada)
  ) %>%
  select(-any_of("Ha_Asignada_new"))

ok <- guardar_ediciones_disco(ed_updated)
cat("Guardado en disco:", ok, "\n")
