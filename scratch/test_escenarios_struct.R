suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Let's test the data structure for escenarios_ajuste_semanal.rds
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
escenarios <- if (file.exists(escenarios_file)) {
  readRDS(escenarios_file)
} else {
  tibble(
    Id_Escenario          = character(),
    Nombre                = character(),
    Descripcion           = character(),
    Campaña               = character(),
    Semana                = numeric(),
    Dia_Desde             = character(),
    Dia_Hasta             = character(),
    Rango_Dias            = character(),
    Dias_Ajustados        = character(),
    Total_Kg_Ajustados    = numeric(),
    Total_Kg_Real_Prev    = numeric(),
    Cosecha_Ajustada_Tot  = numeric(),
    Num_Turnos            = numeric(),
    Fecha_Creacion        = as.POSIXct(character()),
    Fecha_Modificacion    = as.POSIXct(character()),
    Ediciones_Data        = list(),
    Valvulas_Data         = list()
  )
}

cat("Estructura de escenarios lista. Filas existentes:", nrow(escenarios), "\n")
