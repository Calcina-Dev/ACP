load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  v <- toupper(trimws(as.character(x)))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
sub_ed <- ed %>% filter(as.numeric(Semana) == 47, as.character(Módulo) == "13", as.character(Turno) == "1")

# Test splitting 1ra vs 2da pasada
ed_filtrada <- sub_ed %>%
  filter(coalesce(Valor, 0) > 0) %>%
  mutate(
    Turno_num    = suppressWarnings(as.numeric(Turno)),
    Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),
    Módulo_clean = trimws(as.character(Módulo)),
    Sector_clean = trimws(as.character(Sector)),
    Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),
    es_rocio     = (Var_clean == "ROCIO"),
    es_p2        = (es_rocio & (
      coalesce(Kg_Asignado_2da, 0) > 0 |
      coalesce(Ha_Asignada_2da, 0) > 0 |
      grepl("2da pasada|2ª pasada", coalesce(Motivo_Ajuste, ""), ignore.case = TRUE)
    )),
    kg_p1        = ifelse(!es_p2, coalesce(Kg_Asignado_1ra, Valor), 0),
    ha_p1        = ifelse(!es_p2, coalesce(Ha_Asignada_1ra, Ha_Asignada), 0),
    kg_p2        = ifelse(es_p2, coalesce(Kg_Asignado_2da, Valor), 0),
    ha_p2        = ifelse(es_p2, coalesce(Ha_Asignada_2da, Ha_Asignada), 0)
  )

ed_p1 <- ed_filtrada %>%
  filter(kg_p1 > 0 | ha_p1 > 0) %>%
  group_by(Sector = Sector_clean, Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%
  summarise(
    Ha_Ajustada_Esc = round(sum(coalesce(ha_p1, 0), na.rm = TRUE), 2),
    Kg_Ajustado_Esc = round(sum(coalesce(kg_p1, 0), na.rm = TRUE), 1),
    Motivo_Ajuste   = first(na.omit(Motivo_Ajuste[kg_p1 > 0 | ha_p1 > 0])),
    .groups = "drop"
  )

ed_p2 <- ed_filtrada %>%
  filter(kg_p2 > 0 | ha_p2 > 0) %>%
  group_by(Sector = Sector_clean, Módulo = Módulo_clean, Turno = Turno_clean, Variedad = Var_clean) %>%
  summarise(
    Ha_Ajustada_Esc = round(sum(coalesce(ha_p2, 0), na.rm = TRUE), 2),
    Kg_Ajustado_Esc = round(sum(coalesce(kg_p2, 0), na.rm = TRUE), 1),
    Motivo_Ajuste   = first(na.omit(Motivo_Ajuste[kg_p2 > 0 | ha_p2 > 0])),
    .groups = "drop"
  )

cat("=== 1ª Pasada ===\n")
print(ed_p1)
cat("\n=== 2ª Pasada ===\n")
print(ed_p2)

stopifnot(round(ed_p1$Kg_Ajustado_Esc, 1) == 252.7)
stopifnot(round(ed_p2$Kg_Ajustado_Esc, 1) == 147.2)
cat("\n>>> TEST PASSED! 1ª Pasada = 252.7 kg, 2ª Pasada = 147.2 kg <<<\n")
