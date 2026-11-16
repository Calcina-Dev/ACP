suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  v <- as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

# Check frecuencias files
ruta_frecuencias_principal <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/frecuencias_modulo_variedad_cosecha.rds"
ruta_frecuencias_nube      <- "D:/ACP/Actualizacion Semanal/frecuencias_modulo_variedad_cosecha.rds"
ruta_frec_file <- if (file.exists(ruta_frecuencias_principal)) ruta_frecuencias_principal else ruta_frecuencias_nube

cat("Frec file exists:", file.exists(ruta_frec_file), "\n")
df_frec_mv <- if (file.exists(ruta_frec_file)) readRDS(ruta_frec_file) else NULL
if (!is.null(df_frec_mv)) {
  cat("df_frec_mv for M10 ROCIO:\n")
  print(df_frec_mv %>% filter(Módulo == "10", Variedad == "ROCIO"))
}

# Check modo_frecuencia
ruta_modo <- "D:/ACP/Actualizacion Semanal/modo_frecuencia_cosecha.rds"
modo_frec <- if (file.exists(ruta_modo)) readRDS(ruta_modo) else "automatico"
cat("Modo frecuencia:", modo_frec, "\n")

# Now check calcular_frecuencias_valvula_2semanas
# Let's find definition of calcular_frecuencias_valvula_2semanas in app.R
source_lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
frec_func_start <- which(grepl("calcular_frecuencias_valvula_2semanas <- function", source_lines))[1]
cat("calcular_frecuencias_valvula_2semanas at line:", frec_func_start, "\n")
