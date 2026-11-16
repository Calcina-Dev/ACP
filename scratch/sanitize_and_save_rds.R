suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# 1. Update the RDS files
ruta_local <- "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds"
ruta_drive <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/ediciones_manuales_cosecha.rds"

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  v <- as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

sanitizar_ediciones <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df %>%
    mutate(
      is_not_rocio = (limpiar_variedad(Variedad) != "ROCIO"),
      Ha_Asignada_1ra = ifelse(is_not_rocio, coalesce(Ha_Asignada, 0), Ha_Asignada_1ra),
      Kg_Asignado_1ra = ifelse(is_not_rocio, coalesce(Valor, 0), Kg_Asignado_1ra),
      Ha_Asignada_2da = ifelse(is_not_rocio, 0, Ha_Asignada_2da),
      Kg_Asignado_2da = ifelse(is_not_rocio, 0, Kg_Asignado_2da)
    ) %>%
    select(-is_not_rocio)
}

if (file.exists(ruta_local)) {
  ed_l <- readRDS(ruta_local)
  ed_l_fixed <- sanitizar_ediciones(ed_l)
  saveRDS(ed_l_fixed, ruta_local)
  cat("Local file sanitized and saved!\n")
}

if (file.exists(ruta_drive)) {
  ed_d <- readRDS(ruta_drive)
  ed_d_fixed <- sanitizar_ediciones(ed_d)
  saveRDS(ed_d_fixed, ruta_drive)
  cat("Drive file sanitized and saved!\n")
}

# Verify M2 T4
ed_check <- readRDS(ruta_local)
m2t4 <- ed_check %>% filter(Semana == 46, Módulo == "2", Turno == "4")
print(m2t4 %>% select(Semana, Módulo, Turno, Variedad, Campo, Valor, Ha_Asignada, Ha_Asignada_1ra, Ha_Asignada_2da, Kg_Asignado_1ra, Kg_Asignado_2da))
