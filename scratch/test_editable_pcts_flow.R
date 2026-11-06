suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Test scenario with Sector 8, Módulo 5, Turno 10 (as in user's image)
ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")

ha_pasada <- 6.40
nuevo_kgha <- 801.74
dias <- c("Jueves", "Viernes")

# User edits percentages: Jueves = 70%, Viernes = 30%
dist_info_from_client <- list(
  nuevo_kgha = 801.74,
  total_kg = 5131.14,
  area_pasada = 6.40,
  dias = list(
    list(dia = "Jueves", pct = 70.0, kg = 3591.80, ha = 4.48),
    list(dia = "Viernes", pct = 30.0, kg = 1539.34, ha = 1.92)
  )
)

df_dias_cli <- bind_rows(lapply(dist_info_from_client$dias, as_tibble))
fila <- list(Sector = "8", Módulo = "5", Turno = "10", Variedad = "MAGICA")
pasada_sel <- 1L

nuevas_filas <- lapply(seq_len(nrow(df_dias_cli)), function(i) {
  d_nom <- as.character(df_dias_cli$dia[i])
  k_val <- round(as.numeric(df_dias_cli$kg[i]), 2)
  h_val <- round(as.numeric(df_dias_cli$ha[i]), 2)
  p_val <- round(as.numeric(df_dias_cli$pct[i]), 1)
  tibble(
    Campaña         = "2026",
    Semana          = 45,
    Sector          = as.character(fila$Sector),
    Módulo          = as.character(fila$Módulo),
    Turno           = as.character(fila$Turno),
    Variedad        = "MAGICA",
    Campo           = d_nom,
    Valor           = k_val,
    Ha_Asignada     = h_val,
    Ha_Asignada_1ra = if (pasada_sel == 1L) h_val else 0,
    Kg_Asignado_1ra = if (pasada_sel == 1L) k_val else 0,
    Ha_Asignada_2da = if (pasada_sel == 2L) h_val else 0,
    Kg_Asignado_2da = if (pasada_sel == 2L) k_val else 0,
    Tipo_Edicion    = "manual",
    Modo_Ingreso    = "kgha",
    Motivo_Ajuste   = sprintf("Manual (%.2f kg/ha en %.2f ha, %.1f%%)", nuevo_kgha, h_val, p_val),
    Fecha_Edicion   = Sys.time()
  )
}) %>% bind_rows()

print(nuevas_filas %>% select(Campo, Valor, Ha_Asignada, Tipo_Edicion, Motivo_Ajuste))

stopifnot(sum(nuevas_filas$Valor) == 5131.14)
stopifnot(sum(nuevas_filas$Ha_Asignada) == 6.40)
stopifnot(round(sum(nuevas_filas$Valor) / sum(nuevas_filas$Ha_Asignada), 2) == 801.74)

cat("\nSimulación completada con éxito!\n")
