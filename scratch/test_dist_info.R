suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Simular dist_info que Shiny entrega desde JS
dist_info_sim <- list(
  nuevo_kgha = 801.74,
  total_kg = 5131.14,
  area_pasada = 6.40,
  dias = list(
    list(dia = "Jueves", pct = 70.0, kg = 3591.80, ha = 4.48),
    list(dia = "Viernes", pct = 30.0, kg = 1539.34, ha = 1.92)
  )
)

df_dias <- bind_rows(lapply(dist_info_sim$dias, as_tibble))
print(df_dias)

nuevo_kgha <- dist_info_sim$nuevo_kgha
pasada_sel <- 1L
fila <- list(Sector = "8", Módulo = "5", Turno = "10", Variedad = "MAGICA")

nuevas_filas <- lapply(seq_len(nrow(df_dias)), function(i) {
  d <- df_dias$dia[i]
  k_val <- round(as.numeric(df_dias$kg[i]), 2)
  h_val <- round(as.numeric(df_dias$ha[i]), 2)
  p_val <- round(as.numeric(df_dias$pct[i]), 1)
  tibble(
    Campaña         = "2026",
    Semana          = 45,
    Sector          = as.character(fila$Sector),
    Módulo          = as.character(fila$Módulo),
    Turno           = as.character(fila$Turno),
    Variedad        = as.character(fila$Variedad),
    Campo           = as.character(d),
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

print(nuevas_filas)
stopifnot(sum(nuevas_filas$Valor) == 5131.14)
stopifnot(sum(nuevas_filas$Ha_Asignada) == 6.40)
cat("Test passed successfully!\n")
