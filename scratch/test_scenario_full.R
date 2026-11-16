suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(lubridate)
})

# Path to scenarios
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
valvs_file <- "D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds"
valvs <- if (file.exists(valvs_file)) readRDS(valvs_file) else tibble()

camp_sel <- "2026"
sem_sel  <- 45

# Create two test scenarios to test comparison and persistence
ed_45_v1 <- ediciones %>% filter(Campaña == camp_sel, Semana == sem_sel)
ed_45_v2 <- ed_45_v1 %>% mutate(Valor = ifelse(Campo == "Viernes", round(Valor * 1.05, 1), Valor))

esc_1 <- tibble(
  Id_Escenario         = paste0("ESC-", camp_sel, "-S", sem_sel, "-BASE"),
  Nombre               = "Escenario Viernes Base",
  Descripcion          = "Ajuste oficial para el viernes",
  Campaña              = camp_sel,
  Semana               = sem_sel,
  Dia_Desde            = "Viernes",
  Dia_Hasta            = "Viernes",
  Rango_Dias           = "Viernes a Viernes",
  Dias_Ajustados       = "Viernes",
  Total_Kg_Ajustados   = sum(ed_45_v1$Valor, na.rm = TRUE),
  Total_Kg_Real_Prev   = 318280.0,
  Cosecha_Ajustada_Tot = 318280.0 + sum(ed_45_v1$Valor, na.rm = TRUE),
  Num_Turnos           = length(unique(paste(ed_45_v1$Sector, ed_45_v1$Módulo, ed_45_v1$Turno))),
  Fecha_Creacion       = Sys.time() - 3600,
  Fecha_Modificacion   = Sys.time() - 3600,
  Ediciones_Data       = list(ed_45_v1),
  Valvulas_Data        = list(valvs)
)

esc_2 <- tibble(
  Id_Escenario         = paste0("ESC-", camp_sel, "-S", sem_sel, "-OPT"),
  Nombre               = "Escenario Viernes Optimista (+5%)",
  Descripcion          = "Sensibilidad optimista de cierre",
  Campaña              = camp_sel,
  Semana               = sem_sel,
  Dia_Desde            = "Viernes",
  Dia_Hasta            = "Viernes",
  Rango_Dias           = "Viernes a Viernes",
  Dias_Ajustados       = "Viernes",
  Total_Kg_Ajustados   = sum(ed_45_v2$Valor, na.rm = TRUE),
  Total_Kg_Real_Prev   = 318280.0,
  Cosecha_Ajustada_Tot = 318280.0 + sum(ed_45_v2$Valor, na.rm = TRUE),
  Num_Turnos           = length(unique(paste(ed_45_v2$Sector, ed_45_v2$Módulo, ed_45_v2$Turno))),
  Fecha_Creacion       = Sys.time(),
  Fecha_Modificacion   = Sys.time(),
  Ediciones_Data       = list(ed_45_v2),
  Valvulas_Data        = list(valvs)
)

escenarios_test <- bind_rows(esc_1, esc_2)
saveRDS(escenarios_test, escenarios_file)
cat("Guardados 2 escenarios de prueba en", escenarios_file, "\n")

# Verify loading
cargados <- readRDS(escenarios_file)
cat("Escenarios cargados:", nrow(cargados), "\n")
print(cargados %>% select(Id_Escenario, Nombre, Rango_Dias, Total_Kg_Ajustados))
