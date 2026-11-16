suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

cat("=== TEST: Mover Día en Edición Manual ===\n")

# Mock de contexto
ctx <- list(
  fila = tibble(
    Sector = "8",
    Módulo = "4",
    Turno = "1",
    Variedad = "MAGICA",
    Tipo = "Convencional",
    Área = 6.42,
    Jueves = 1016.49,
    Martes = 3017.20,
    Lunes_bloqueado = TRUE,
    Martes_bloqueado = FALSE,
    Miércoles_bloqueado = FALSE,
    Jueves_bloqueado = FALSE,
    Viernes_bloqueado = FALSE,
    Sábado_bloqueado = FALSE
  ),
  campo = "Jueves",
  area_turno = 6.42,
  ha_real_1ra = 0,
  ha_real_2da = 0,
  pasada_sug = 1L
)

# Ediciones actuales simuladas (como en la captura del usuario: Martes y Jueves)
ed_actuales <- tibble(
  Campaña = "2026",
  Semana = 46,
  Sector = "8",
  Módulo = "4",
  Turno = "1",
  Variedad = "MAGICA",
  Campo = c("Martes", "Jueves"),
  Valor = c(3017.20, 1016.49),
  Ha_Asignada = c(4.80, 1.62),
  Ha_Asignada_1ra = c(4.80, 1.62),
  Kg_Asignado_1ra = c(3017.20, 1016.49),
  Ha_Asignada_2da = c(0, 0),
  Kg_Asignado_2da = c(0, 0),
  Tipo_Edicion = c("manual", "manual"),
  Modo_Ingreso = c("kilos", "kilos"),
  Motivo_Ajuste = c("Manual prorrateado", "Manual prorrateado"),
  Fecha_Edicion = Sys.time()
)

valvs_act <- tibble(
  Campaña = "2026",
  Semana = 46,
  id = c("v1", "v2"),
  codigo = c("M4-T1-V1", "M4-T1-V3"),
  Sector = "8",
  Módulo = "4",
  Turno = "1",
  Variedad = "MAGICA",
  Dia_Asignado = c("Martes", "Jueves"),
  Kg_Asignado = c(3017.20, 1016.49),
  Ha_Asignada = c(4.80, 1.62),
  Num_Pasada = c(1L, 1L),
  Tipo_Asignacion = c("auto", "auto"),
  Dia_Desde = "Martes",
  Dia_Hasta = "Viernes",
  Motivo_Ajuste = "Ajuste",
  Fecha_Ajuste = Sys.time()
)

# Usuario mueve de Jueves a Viernes con 1016.49 kg
dia_origen <- as.character(ctx$campo)
dia_destino <- "Viernes"
val_final <- 1016.49
modo <- "kilos"
pasada_activa <- 1L
ha_pool_pasada <- 6.42

limpiar_variedad <- function(x) toupper(trimws(as.character(x)))

# 1. ed_otros_pasada excluye dia_origen y dia_destino
ed_otros_pasada <- ed_actuales %>%
  filter(
    as.character(Campaña) == "2026",
    as.numeric(Semana) == 46,
    as.character(Módulo) == "4",
    as.character(Turno) == "1",
    limpiar_variedad(Variedad) == "MAGICA",
    !(as.character(Campo) %in% c(dia_origen, dia_destino)),
    coalesce(Tipo_Edicion, "") == "manual"
  )

cat("Otros días encontrados:", paste(ed_otros_pasada$Campo, collapse = ", "), "(esperado: Martes)\n")
stopifnot(identical(ed_otros_pasada$Campo, "Martes"))

# Prorrateo
ed_prorr <- ed_otros_pasada
ha_fijas <- 0
ha_pool_prorr <- ha_pool_pasada - ha_fijas
kg_tot_otros <- sum(coalesce(as.numeric(ed_prorr$Valor), 0), na.rm = TRUE)
kg_total_conjunto <- kg_tot_otros + val_final
ha_acum <- 0
for (i in seq_len(nrow(ed_prorr))) {
  d_campo <- ed_prorr$Campo[i]
  ha_nueva_i <- round(ha_pool_prorr * (as.numeric(ed_prorr$Valor[i]) / kg_total_conjunto), 2)
  ha_acum <- ha_acum + ha_nueva_i
}
ha_final <- max(0, round(ha_pool_prorr - ha_acum, 2))

cat(sprintf("Hectáreas prorrateadas: Martes = %.2f ha, Viernes = %.2f ha (Total: %.2f ha)\n", ha_acum, ha_final, ha_acum + ha_final))
stopifnot(abs((ha_acum + ha_final) - 6.42) < 0.01)

# Construir nueva edición en dia_destino
motivo_final <- sprintf("Movido desde %s (%s kg en %.2f ha)", dia_origen, format(round(val_final, 1)), ha_final)

nueva_edicion <- tibble(
  Campaña = "2026",
  Semana = 46,
  Sector = "8",
  Módulo = "4",
  Turno = "1",
  Variedad = "MAGICA",
  Campo = dia_destino,
  Valor = val_final,
  Ha_Asignada = ha_final,
  Ha_Asignada_1ra = ha_final,
  Kg_Asignado_1ra = val_final,
  Ha_Asignada_2da = 0,
  Kg_Asignado_2da = 0,
  Tipo_Edicion = "manual",
  Modo_Ingreso = "kilos",
  Motivo_Ajuste = motivo_final,
  Fecha_Edicion = Sys.time()
)

# Actualizar ed_actuales
ed_final <- ed_actuales %>%
  filter(!(
    as.character(Campaña) == "2026" &
    as.numeric(Semana) == 46 &
    as.character(Módulo) == "4" &
    as.character(Turno) == "1" &
    limpiar_variedad(Variedad) == "MAGICA" &
    as.character(Campo) %in% c(dia_origen, dia_destino)
  )) %>%
  bind_rows(nueva_edicion)

cat("Días en ed_final:\n")
print(ed_final %>% select(Campo, Valor, Ha_Asignada, Motivo_Ajuste))

stopifnot(!("Jueves" %in% ed_final$Campo))
stopifnot("Viernes" %in% ed_final$Campo)
stopifnot("Martes" %in% ed_final$Campo)

# Actualizar válvulas en rv_ajuste_valvulas
idx_v <- which(
  as.character(valvs_act$Campaña) == "2026" &
  as.numeric(valvs_act$Semana) == 46 &
  as.character(valvs_act$Módulo) == "4" &
  as.character(valvs_act$Turno) == "1" &
  limpiar_variedad(valvs_act$Variedad) == "MAGICA" &
  as.character(valvs_act$Dia_Asignado) == dia_origen
)
valvs_act$Dia_Asignado[idx_v] <- dia_destino

cat("Válvulas actualizadas:\n")
print(valvs_act %>% select(codigo, Dia_Asignado, Kg_Asignado, Ha_Asignada))
stopifnot(valvs_act$Dia_Asignado[2] == "Viernes")

cat("\n>>> TEST PASSED SUCCESSFULLY! <<<\n")
