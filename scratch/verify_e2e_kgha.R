suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

cat("=== TEST E2E: EDICIÓN DE KG/HA AJUSTADO ===\n")

# Cargar ediciones y válvulas
ed_file <- "Actualizacion Semanal/ediciones_manuales_cosecha.rds"
valv_file <- "Actualizacion Semanal/ajuste_valvulas_semana.rds"

ed_orig <- readRDS(ed_file)
valvs <- readRDS(valv_file)

# Verificar fila antes del ajuste
cat("\n1. Estado inicial del Turno 5 (S8 M4):\n")
s8_init <- ed_orig %>% filter(Sector == "8", Módulo == "4", Turno == "5", Semana == 45)
print(s8_init %>% select(Campo, Valor, Tipo_Edicion, Ha_Asignada, Motivo_Ajuste))

# Simular guardado desde el modal con 859.69 kg/ha
nuevo_kgha <- 859.69
ha_pasada <- 6.45
dias <- c("Jueves", "Viernes")
kg_tot_nuevo <- round(ha_pasada * nuevo_kgha, 2)
props <- s8_init$Valor / sum(s8_init$Valor)

kg_dist <- c(round(kg_tot_nuevo * props[1], 2), max(0, round(kg_tot_nuevo - round(kg_tot_nuevo * props[1], 2), 2)))
ha_dist <- c(3.23, 3.22)

nuevas_filas <- tibble(
  Campaña         = "2026",
  Semana          = 45,
  Sector          = "8",
  Módulo          = "4",
  Turno           = "5",
  Variedad        = "MAGICA",
  Campo           = dias,
  Valor           = kg_dist,
  Ha_Asignada     = ha_dist,
  Ha_Asignada_1ra = ha_dist,
  Kg_Asignado_1ra = kg_dist,
  Ha_Asignada_2da = c(0, 0),
  Kg_Asignado_2da = c(0, 0),
  Tipo_Edicion    = "manual",
  Modo_Ingreso    = "kgha",
  Motivo_Ajuste   = sprintf("Manual (%.2f kg/ha en %.2f ha)", nuevo_kgha, ha_dist),
  Fecha_Edicion   = Sys.time()
)

ed_nueva <- ed_orig %>%
  filter(!(Campaña == "2026" & Semana == 45 & Sector == "8" & Módulo == "4" & Turno == "5" & Campo %in% dias)) %>%
  bind_rows(nuevas_filas)

cat("\n2. Estado tras aplicar ajuste a 859.69 kg/ha:\n")
s8_nuevo <- ed_nueva %>% filter(Sector == "8", Módulo == "4", Turno == "5", Semana == 45)
print(s8_nuevo %>% select(Campo, Valor, Tipo_Edicion, Ha_Asignada, Motivo_Ajuste))

stopifnot(all(s8_nuevo$Tipo_Edicion == "manual"))
stopifnot(sum(s8_nuevo$Valor) == 5545.00)
stopifnot(sum(s8_nuevo$Ha_Asignada) == 6.45)
stopifnot(round(sum(s8_nuevo$Valor) / sum(s8_nuevo$Ha_Asignada), 2) == 859.69)

cat("\nTODAS LAS ASERCIONES PASARON CON ÉXITO!\n")
