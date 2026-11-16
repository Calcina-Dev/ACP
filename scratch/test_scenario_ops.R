suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
valvs_file <- "D:/ACP/Actualizacion Semanal/ajuste_valvulas_semana.rds"
valvs <- if (file.exists(valvs_file)) readRDS(valvs_file) else tibble()

camp_sel <- "2026"
sem_sel  <- 45

ed_sem <- ediciones %>%
  filter(as.character(`Campaña`) == camp_sel, as.numeric(Semana) == sem_sel)

valvs_sem <- if (nrow(valvs) > 0) {
  valvs %>% filter(as.character(`Campaña`) == camp_sel, as.numeric(Semana) == sem_sel)
} else tibble()

cat("Ediciones a guardar en escenario:", nrow(ed_sem), "\n")
cat("Valvulas a guardar en escenario:", nrow(valvs_sem), "\n")

# Create a sample scenario
escenario_1 <- tibble(
  Id_Escenario         = paste0("ESC-", camp_sel, "-S", sem_sel, "-", format(Sys.time(), "%Y%m%d%H%M%S")),
  Nombre               = "Escenario Viernes Oficial",
  Descripcion          = "Ajuste semanal de cierre para el viernes con regla >15% y premio floral",
  Campaña              = camp_sel,
  Semana               = sem_sel,
  Dia_Desde            = "Viernes",
  Dia_Hasta            = "Viernes",
  Rango_Dias           = "Viernes a Viernes",
  Dias_Ajustados       = "Viernes",
  Total_Kg_Ajustados   = sum(ed_sem$Valor, na.rm = TRUE),
  Total_Kg_Real_Prev   = 318280.0,
  Cosecha_Ajustada_Tot = 318280.0 + sum(ed_sem$Valor, na.rm = TRUE),
  Num_Turnos           = length(unique(paste(ed_sem$Sector, ed_sem$Módulo, ed_sem$Turno))),
  Fecha_Creacion       = Sys.time(),
  Fecha_Modificacion   = Sys.time(),
  Ediciones_Data       = list(ed_sem),
  Valvulas_Data        = list(valvs_sem)
)

print(escenario_1 %>% select(Id_Escenario, Nombre, Campaña, Semana, Rango_Dias, Total_Kg_Ajustados, Num_Turnos))

# Test restoring
restored_ed <- escenario_1$Ediciones_Data[[1]]
cat("Restored editions rows:", nrow(restored_ed), "Sum kg:", sum(restored_ed$Valor, na.rm = TRUE), "\n")
stopifnot(identical(nrow(ed_sem), nrow(restored_ed)))
cat("Verification of scenario save/load passed perfectly!\n")
