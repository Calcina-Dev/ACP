suppressPackageStartupMessages({
  library(dplyr)
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

orden_semana_campana <- function(sem) {
  sem_num <- suppressWarnings(as.numeric(sem))
  ifelse(!is.na(sem_num) & sem_num < 20, sem_num + 53, sem_num)
}

source_lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")
eval(parse(text = source_lines[1023:1123]))

df_frec_valvs_2sem <- calcular_frecuencias_valvula_2semanas(df_cosecha, 2026, 46)

cat("df_frec_valvs_2sem rows:", nrow(df_frec_valvs_2sem), "\n")
m10_frec <- df_frec_valvs_2sem %>% filter(grepl("M10-T1-", codigo_norm) | grepl("M10-T3-", codigo_norm) | grepl("M10-T5-", codigo_norm) | grepl("M10-T7-", codigo_norm))
print(m10_frec)
