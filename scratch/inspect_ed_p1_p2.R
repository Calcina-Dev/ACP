load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

# Look at universo_base code in app.R around lines 6500-6570:
# universo has columns:
# Sector, Módulo, Turno, Variedad, Tipo, Proyección, Área
# Ha Real 1ª Pas, Ha Real 2ª Pas, Ha Recorrida, Kg_SAP_Real, Kg_Real_Sem_Tot, Kg_Real_2da_Sem, Kg_Real_1ra_Sem, Kg_Ha_Real_1ra_Sem
# Lunes, Martes, Miércoles, Jueves, Viernes, Sábado

# And in ediciones_manuales:
# Campaña, Semana, Sector, Módulo, Turno, Variedad, Campo, Valor
# Ha_Asignada, Ha_Asignada_1ra, Kg_Asignado_1ra, Ha_Asignada_2da, Kg_Asignado_2da, Motivo_Ajuste, Tipo_Edicion

ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
sub_ed <- ed %>% filter(as.numeric(Semana) == 47, as.character(Módulo) == "13", as.character(Turno) == "1")
print(sub_ed %>% select(Campo, Valor, Ha_Asignada_1ra, Kg_Asignado_1ra, Ha_Asignada_2da, Kg_Asignado_2da, Motivo_Ajuste))
