suppressPackageStartupMessages(library(dplyr))

ed_all <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
s47_lun <- ed_all %>% filter(as.numeric(Semana) == 47, Campo == "Lunes")
print(s47_lun %>% select(Sector, Módulo, Turno, Variedad, Campo, Valor, any_of(c("Tipo_Edicion", "Tipo", "Motivo_Ajuste"))))
