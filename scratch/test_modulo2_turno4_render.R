suppressPackageStartupMessages({
  library(dplyr)
})

source_lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

# Source helper functions: limpiar_variedad, cargar_ediciones_disco, etc.
ruta_ediciones_principal <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/ediciones_manuales_cosecha.rds"
ruta_ediciones_backup    <- "D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds"

limpiar_variedad <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("VARIEDAD\\s*", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Read RDS directly
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")
ed_s46_m2_t4 <- ed %>% filter(Semana == 46, Módulo == "2", Turno == "4")
cat("--- Edición en RDS para S46 M2 T4 ---\n")
print(as.data.frame(ed_s46_m2_t4))

# Check ha_ajustadas_ed calculation for this turn
dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
ed_filtradas <- ed_s46_m2_t4

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
    Motivo_Guardado   = {
      m <- na.omit(Mot_val[nzchar(Mot_val)])
      if (length(m) > 0) m[1] else NA_character_
    },
    .groups           = "drop"
  )

cat("\n--- ha_ajustadas_ed para M2 T4 ---\n")
print(as.data.frame(ha_ajustadas_ed))

# Simulate tabla_detalle_turnos columns for M2 T4
Tipo <- "Convencional"
Tiene_Dias_Ajustados <- TRUE
Variedad <- "SEKOYA POP"
Área <- 9.32
kg_aj_neto_u <- 1648.04
Ajuste <- 1648.04
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

kgha_aj_1ra <- ifelse(coalesce(ha_aj_1ra, 0) > 0 & coalesce(kg_aj_1ra, 0) > 0, 
                      round(kg_aj_1ra / ha_aj_1ra, 2), NA_real_)

cat("\n--- RESULTADO FINAL PARA M2 T4 ---\n")
cat(sprintf("Ha Aj. 1ª Pas:     %.2f ha\n", ha_aj_1ra))
cat(sprintf("Ha Aj. 2ª Pas:     %.2f ha\n", ha_aj_2da))
cat(sprintf("Kg Aj. 1ª Pas:     %.2f kg\n", kg_aj_1ra))
cat(sprintf("Kg/Ha Aj. 1ª Pas:  %.2f kg/ha\n", kgha_aj_1ra))
