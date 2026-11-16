suppressPackageStartupMessages({
  library(dplyr)
})

limpiar_variedad <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("VARIEDAD\\s*", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Test ROCIO with 2da pasada
ed_rocio <- tibble(
  Campaña = "2026", Semana = 46, Sector = "5", Módulo = "10", Turno = "4",
  Variedad = "ROCIO", Campo = "Jueves", Valor = 4980.42,
  Fecha_Edicion = Sys.time(), Tipo_Edicion = "manual",
  Ha_Asignada = 5.53, Ha_Asignada_1ra = 0, Kg_Asignado_1ra = 0,
  Ha_Asignada_2da = 5.53, Kg_Asignado_2da = 4980.42,
  Motivo_Ajuste = "Ajuste 2da pasada", Modo_Ingreso = "kilos"
)

dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
ed_filtradas <- ed_rocio

ha_ajustadas_ed <- ed_filtradas %>%
  filter(Campo %in% dias_validos, coalesce(Valor, 0) > 0, coalesce(Tipo_Edicion, "") != "ruta_sin_kg") %>%
  mutate(
    Módulo_k = trimws(as.character(Módulo)),
    Turno_k  = trimws(as.character(Turno)),
    Var_k    = limpiar_variedad(Variedad),
    es_rocio = (Var_k == "ROCIO"),
    Ha_val   = if ("Ha_Asignada" %in% names(.)) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
    Ha_1ra   = if ("Ha_Asignada_1ra" %in% names(.)) {
      ifelse(es_rocio, suppressWarnings(as.numeric(Ha_Asignada_1ra)), suppressWarnings(as.numeric(Ha_Asignada)))
    } else if (!es_rocio) suppressWarnings(as.numeric(Ha_Asignada)) else NA_real_,
    Kg_1ra   = if ("Kg_Asignado_1ra" %in% names(.)) {
      ifelse(es_rocio, suppressWarnings(as.numeric(Kg_Asignado_1ra)), suppressWarnings(as.numeric(Valor)))
    } else if (!es_rocio) suppressWarnings(as.numeric(Valor)) else NA_real_,
    Ha_2da   = if ("Ha_Asignada_2da" %in% names(.)) {
      ifelse(es_rocio, suppressWarnings(as.numeric(Ha_Asignada_2da)), 0)
    } else 0,
    Kg_2da   = if ("Kg_Asignado_2da" %in% names(.)) {
      ifelse(es_rocio, suppressWarnings(as.numeric(Kg_Asignado_2da)), 0)
    } else 0,
    Mot_val  = if ("Motivo_Ajuste" %in% names(.)) as.character(Motivo_Ajuste) else NA_character_
  ) %>%
  group_by(Módulo_k, Turno_k, Var_k) %>%
  summarise(
    Ha_Ajustada_Total = round(sum(Ha_val, na.rm = TRUE), 2),
    Ha_Aj_1ra_Ed      = round(sum(Ha_1ra, na.rm = TRUE), 2),
    Kg_Aj_1ra_Ed      = round(sum(Kg_1ra, na.rm = TRUE), 2),
    Ha_Aj_2da_Ed      = round(sum(Ha_2da, na.rm = TRUE), 2),
    Kg_Aj_2da_Ed      = round(sum(Kg_2da, na.rm = TRUE), 2),
    Tiene_Ha_Asig     = any(!is.na(Ha_val) & Ha_val > 0),
    Tiene_Detalle_Pas = first(es_rocio) & any(!is.na(Ha_1ra) | !is.na(Ha_2da)),
    .groups           = "drop"
  )

Tipo <- "Orgánico"
Tiene_Dias_Ajustados <- TRUE
Variedad <- "ROCIO"
Área <- 5.54
kg_aj_neto_u <- 4980.42
Ajuste <- 4980.42
Tiene_Detalle_Pas <- ha_ajustadas_ed$Tiene_Detalle_Pas
Ha_Aj_1ra_Ed <- ha_ajustadas_ed$Ha_Aj_1ra_Ed
Ha_Aj_2da_Ed <- ha_ajustadas_ed$Ha_Aj_2da_Ed
Tiene_Ha_Asig <- ha_ajustadas_ed$Tiene_Ha_Asig
Ha_Ajustada_Total <- ha_ajustadas_ed$Ha_Ajustada_Total
Kg_Aj_1ra_Ed <- ha_ajustadas_ed$Kg_Aj_1ra_Ed
Kg_Aj_2da_Ed <- ha_ajustadas_ed$Kg_Aj_2da_Ed
Ha_Recorrida <- 0

ha_aj_1ra <- case_when(
  Tipo == "Testblock" ~ 0,
  !Tiene_Dias_Ajustados ~ 0,
  limpiar_variedad(Variedad) != "ROCIO" & coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > 0 ~ round(pmin(Área, Ha_Ajustada_Total), 2),
  coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_1ra_Ed, 0) > 0 ~ round(pmin(Área, Ha_Aj_1ra_Ed), 2),
  coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_2da_Ed, 0) > 0 ~ 0,
  coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > 0 & limpiar_variedad(Variedad) != "ROCIO" ~ round(pmin(Área, Ha_Ajustada_Total), 2),
  coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > 0 & limpiar_variedad(Variedad) == "ROCIO" ~ 
    round(pmin(Área, pmax(0, Área - coalesce(Ha_Recorrida, 0))), 2),
  coalesce(Ha_Recorrida, 0) > 0 ~ round(pmin(Área, pmax(0, Área - Ha_Recorrida)), 2),
  Ajuste > 0 ~ round(Área, 2),
  TRUE ~ 0
)

ha_aj_2da <- case_when(
  Tipo == "Testblock" ~ 0,
  limpiar_variedad(Variedad) != "ROCIO" ~ 0,
  !Tiene_Dias_Ajustados ~ 0,
  coalesce(Tiene_Detalle_Pas, FALSE) & coalesce(Ha_Aj_2da_Ed, 0) > 0 ~ round(pmin(Área, Ha_Aj_2da_Ed), 2),
  coalesce(Tiene_Ha_Asig, FALSE) & coalesce(Ha_Ajustada_Total, 0) > ha_aj_1ra ~ 
    round(pmin(Área, Ha_Ajustada_Total - ha_aj_1ra), 2),
  TRUE ~ 0
)

kg_aj_1ra <- case_when(
  Tipo == "Testblock" ~ 0,
  !Tiene_Dias_Ajustados ~ 0,
  limpiar_variedad(Variedad) != "ROCIO" ~ kg_aj_neto_u,
  coalesce(Tiene_Detalle_Pas, FALSE) & (coalesce(Kg_Aj_1ra_Ed, 0) + coalesce(Kg_Aj_2da_Ed, 0)) > 0 ~ 
    round(kg_aj_neto_u * (coalesce(Kg_Aj_1ra_Ed, 0) / pmax(1, coalesce(Kg_Aj_1ra_Ed, 0) + coalesce(Kg_Aj_2da_Ed, 0))), 2),
  coalesce(ha_aj_1ra, 0) > 0 & coalesce(ha_aj_2da, 0) > 0 ~ 
    round(kg_aj_neto_u * (ha_aj_1ra / pmax(0.01, ha_aj_1ra + ha_aj_2da * 0.6)), 2),
  TRUE ~ kg_aj_neto_u
)

kg_aj_2da <- case_when(
  Tipo == "Testblock" ~ 0,
  limpiar_variedad(Variedad) != "ROCIO" ~ 0,
  !Tiene_Dias_Ajustados ~ 0,
  TRUE ~ round(pmax(0, kg_aj_neto_u - kg_aj_1ra), 2)
)

cat("\n--- RESULTADO ROCIO 2da PASADA ---\n")
cat(sprintf("Tiene_Detalle_Pas: %s\n", Tiene_Detalle_Pas))
cat(sprintf("Ha Aj. 1ª Pas:     %.2f ha\n", ha_aj_1ra))
cat(sprintf("Ha Aj. 2ª Pas:     %.2f ha\n", ha_aj_2da))
cat(sprintf("Kg Aj. 2ª Pas:     %.2f kg\n", kg_aj_2da))
cat(sprintf("Kg/Ha Aj. 2ª Pas:  %.2f kg/ha\n", round(kg_aj_2da / ha_aj_2da, 2)))
