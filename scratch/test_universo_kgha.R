suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Cargar app.R en un entorno o probar universo_completo
# Vamos a simular cómo universo_completo procesa ha_ajustadas_ed
ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
# Actualizar Jueves y Viernes de S8 M4 T5
nuevos_regs <- tibble(
  Campaña         = "2026",
  Semana          = 45,
  Sector          = "8",
  Módulo          = "4",
  Turno           = "5",
  Variedad        = "MAGICA",
  Campo           = c("Jueves", "Viernes"),
  Valor           = c(2776.80, 2768.20),
  Fecha_Edicion   = Sys.time(),
  Tipo_Edicion    = "manual",
  Ha_Asignada     = c(3.23, 3.22),
  Ha_Asignada_1ra = c(3.23, 3.22),
  Kg_Asignado_1ra = c(2776.80, 2768.20),
  Ha_Asignada_2da = c(0, 0),
  Kg_Asignado_2da = c(0, 0),
  Motivo_Ajuste   = "Manual (859.69 kg/ha en 6.45 ha)",
  Modo_Ingreso    = "kgha"
)

ed_test <- ed %>%
  filter(!(Campaña == "2026" & Semana == 45 & Sector == "8" & Módulo == "4" & Turno == "5" & Campo %in% c("Jueves", "Viernes"))) %>%
  bind_rows(nuevos_regs)

# Comprobar el procesamiento en ha_ajustadas_ed
dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
ha_aj_t <- ed_test %>%
  filter(Campaña == "2026", Semana == 45, Sector == "8", Módulo == "4", Turno == "5", Campo %in% dias_validos) %>%
  summarise(
    Ha_Ajustada_Total = round(sum(Ha_Asignada, na.rm = TRUE), 2),
    Ha_Aj_1ra_Ed      = round(sum(Ha_Asignada_1ra, na.rm = TRUE), 2),
    Kg_Aj_1ra_Ed      = round(sum(Kg_Asignado_1ra, na.rm = TRUE), 2),
    Ha_Aj_2da_Ed      = round(sum(Ha_Asignada_2da, na.rm = TRUE), 2),
    Kg_Aj_2da_Ed      = round(sum(Kg_Asignado_2da, na.rm = TRUE), 2)
  )
print(ha_aj_t)
cat(sprintf("Kg/Ha resultante 1ra pas: %.2f kg/ha\n", ha_aj_t$Kg_Aj_1ra_Ed / ha_aj_t$Ha_Aj_1ra_Ed))
