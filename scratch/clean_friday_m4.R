suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")
source("d:/ACP/Actualizacion Semanal/app.R", local = TRUE)

ed <- cargar_ediciones_disco()

# Remover las entradas duplicadas de Viernes en Turno 7 y Turno 8 (Semana 44 Módulo 4)
ed_cleaned <- ed %>%
  filter(!(
    as.character(Campaña) == "2026" &
      Semana == 44 &
      as.character(Módulo) == "4" &
      as.character(Turno) %in% c("7", "8") &
      Campo == "Viernes" &
      Tipo_Edicion %in% c("auto", "ruta_sin_kg")
  ))

# Asignar Ha_Asignada para Turno 7 Sábado (4.53) y Turno 8 Sábado (3.97)
ed_cleaned <- ed_cleaned %>%
  mutate(
    Ha_Asignada = case_when(
      as.character(Campaña) == "2026" & Semana == 44 & as.character(Módulo) == "4" & as.character(Turno) == "7" & Campo == "Sábado" ~ 4.53,
      as.character(Campaña) == "2026" & Semana == 44 & as.character(Módulo) == "4" & as.character(Turno) == "8" & Campo == "Sábado" ~ 3.97,
      TRUE ~ Ha_Asignada
    )
  )

ok <- guardar_ediciones_disco(ed_cleaned)
cat("Guardado y limpiado en disco:", ok, "\n")
