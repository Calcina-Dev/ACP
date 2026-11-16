suppressPackageStartupMessages(library(dplyr))

ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  v <- as.character(chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

bad_records <- ed %>%
  filter(limpiar_variedad(Variedad) != "ROCIO", coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0)

cat("Bad records count in local file:", nrow(bad_records), "\n")
if (nrow(bad_records) > 0) {
  print(bad_records %>% select(Campaña, Semana, Módulo, Turno, Variedad, Campo, Valor, Ha_Asignada, Ha_Asignada_1ra, Ha_Asignada_2da))
}

# Also check in Google Drive if file exists
drive_file <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/ediciones_manuales_cosecha.rds"
if (file.exists(drive_file)) {
  ed_drive <- readRDS(drive_file)
  bad_drive <- ed_drive %>%
    filter(limpiar_variedad(Variedad) != "ROCIO", coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0)
  cat("\nBad records count in Google Drive file:", nrow(bad_drive), "\n")
  if (nrow(bad_drive) > 0) {
    print(bad_drive %>% select(Campaña, Semana, Módulo, Turno, Variedad, Campo, Valor, Ha_Asignada, Ha_Asignada_1ra, Ha_Asignada_2da))
  }
}
