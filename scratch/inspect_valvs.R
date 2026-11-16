suppressPackageStartupMessages(library(dplyr))
valvs <- readRDS('Actualizacion Semanal/ajuste_valvulas_semana.rds')
cat("\nTurno 9 & 10 in valvs for Semana 46:\n")
sub_v <- valvs %>% filter(Turno %in% c("9", "10", 9, 10), Módulo %in% c("5", 5), as.numeric(Semana) == 46)
print(sub_v %>% select(any_of(c("codigo", "Turno", "Dia_Asignado", "Kg_Asignado", "Ha_Asignada", "Num_Pasada"))))

ruta_ed <- if (file.exists("Actualizacion Semanal/ediciones_ajuste_semana.rds")) "Actualizacion Semanal/ediciones_ajuste_semana.rds" else "ediciones_ajuste_semana.rds"
if (!file.exists(ruta_ed)) {
  ff <- list.files("Actualizacion Semanal", pattern = "edicion.*\\.rds$", full.names = TRUE)
  if (length(ff) > 0) ruta_ed <- ff[1]
}
cat("Reading ediciones from:", ruta_ed, "\n")
if (file.exists(ruta_ed)) {
  ed <- readRDS(ruta_ed)
  sub_ed <- ed %>% filter(Turno %in% c("9", "10", 9, 10), Módulo %in% c("5", 5), as.numeric(Semana) == 46)
  print(sub_ed %>% select(any_of(c("Turno", "Campo", "Valor", "Ha_Asignada", "Tipo_Edicion", "Motivo_Ajuste"))))
}
