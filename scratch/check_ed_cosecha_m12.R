library(dplyr)
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
cat("Total rows in ediciones_manuales_cosecha.rds:", nrow(ed), "\n")
m12 <- ed %>% filter(grepl("^12$", trimws(as.character(Módulo))))
cat("Módulo 12 rows in ediciones_manuales_cosecha.rds:\n")
print(m12 %>% select(any_of(c("Campaña", "Semana", "Sector", "Módulo", "Turno", "Variedad", "Campo", "Valor", "Tipo_Edicion", "Motivo_Ajuste"))))

cat("\nCheck all distinct Sector for each Módulo in ediciones_manuales_cosecha.rds:\n")
print(ed %>% group_by(Módulo) %>% summarise(Sectores = paste(unique(Sector), collapse=", ")))
