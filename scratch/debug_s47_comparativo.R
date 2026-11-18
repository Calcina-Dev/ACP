# Check what evaluar_escenario does for S47 with actual inputs
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

ed_all <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
cat("Total ed_all rows:", nrow(ed_all), "\n")
ed_s47 <- ed_all %>% filter(as.numeric(Semana) == 47)
cat("ed_s47 rows:", nrow(ed_s47), "\n")
cat("ed_s47 Campos (dias):", paste(unique(ed_s47$Campo), collapse = ", "), "\n")

# Check if there are any edits on Lunes in S47
ed_lun <- ed_s47 %>% filter(Campo == "Lunes")
cat("Edits on Lunes in S47:", nrow(ed_lun), "\n")
if (nrow(ed_lun) > 0) {
  print(head(ed_lun %>% select(Sector, Módulo, Turno, Variedad, Campo, Valor, Motivo_Ajuste)))
}
