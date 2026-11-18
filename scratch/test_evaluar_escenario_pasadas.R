load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  v <- toupper(trimws(as.character(x)))
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", v, ignore.case = TRUE), "MAGICA", v))
}

ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
df_ed <- ed %>% filter(as.numeric(Semana) == 47)
dias_semana_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_ajuste <- dias_semana_nombres
cols_prev_u <- character(0)
cols_sem_u <- intersect(dias_semana_nombres, names(df_hist_oficial_wide))

# Simular u_base para Módulo 13 Turno 1
u_base <- tibble(
  Sector = "3",
  Módulo = "13",
  Turno = "1",
  Variedad = "ROCIO",
  Tipo = "Orgánico",
  Proyección = 252.65,
  Área = 5.11,
  `Ha Real 1ª Pas` = 5.11,
  `Ha Real 2ª Pas` = 0.00,
  Kg_Real_1ra_Sem = 294.0,
  Kg_Real_2da_Sem = 0.0,
  Lunes = 0, Martes = 0, Miércoles = 0, Jueves = 0, Viernes = 0, Sábado = 0
)

# 1. Separar df_ed en 1ra y 2da pasada
ed_filtrada <- df_ed %>%
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

# 2. Base para 1ª Pasada
res_1ra <- u_base %>%
  mutate(
    Turno_num    = suppressWarnings(as.numeric(Turno)),
    Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),
    Módulo_clean = trimws(as.character(Módulo)),
    Sector_clean = trimws(as.character(Sector)),
    Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),
    Pasada       = "1ª Pasada",
    Kg_Real_Prev = 0,
    Kg_Real_Semana = if ("Kg_Real_1ra_Sem" %in% names(.)) coalesce(Kg_Real_1ra_Sem, 0) else 0,
    Kg_Proy_Oficial = round(coalesce(Proyección, 0), 1)
  ) %>%
  left_join(ed_p1, by = c("Sector_clean" = "Sector", "Módulo_clean" = "Módulo", "Turno_clean" = "Turno", "Var_clean" = "Variedad")) %>%
  mutate(
    Ha_Ajustada_Esc    = coalesce(Ha_Ajustada_Esc, 0),
    Kg_Ajustado_Esc    = coalesce(Kg_Ajustado_Esc, 0),
    Kg_Ajustado_Semana = round(Kg_Real_Prev + Kg_Ajustado_Esc, 1),
    Diferencia_Kg      = round(Kg_Real_Semana - Kg_Ajustado_Semana, 1),
    Pct_Desvio         = ifelse(Kg_Ajustado_Semana > 0, round((Diferencia_Kg / Kg_Ajustado_Semana) * 100, 1), NA_real_),
    Turno_Label        = sprintf("M%s-T%s (%s)", Módulo_clean, Turno_clean, Var_clean),
    Motivo_Ajuste      = coalesce(Motivo_Ajuste, "Sin ajuste programado"),
    Módulo             = Módulo_clean,
    Turno              = Turno_clean,
    Variedad           = Var_clean
  )

# 3. Base para 2ª Pasada
u_base_2da <- u_base %>%
  filter(
    limpiar_variedad(Variedad) == "ROCIO" |
    coalesce(`Ha Real 2ª Pas`, 0) > 0 |
    (if ("Kg_Real_2da_Sem" %in% names(.)) coalesce(Kg_Real_2da_Sem, 0) > 0 else FALSE) |
    paste(trimws(as.character(Módulo)), trimws(as.character(Turno))) %in% paste(ed_p2$Módulo, ed_p2$Turno)
  )

res_2da <- if (nrow(u_base_2da) > 0) {
  u_base_2da %>%
    mutate(
      Turno_num    = suppressWarnings(as.numeric(Turno)),
      Turno_clean  = ifelse(!is.na(Turno_num), as.character(Turno_num), trimws(as.character(Turno))),
      Módulo_clean = trimws(as.character(Módulo)),
      Sector_clean = trimws(as.character(Sector)),
      Var_clean    = toupper(trimws(limpiar_variedad(Variedad))),
      Pasada       = "2ª Pasada",
      Kg_Real_Prev = 0,
      Kg_Real_Semana = if ("Kg_Real_2da_Sem" %in% names(.)) coalesce(Kg_Real_2da_Sem, 0) else 0,
      Kg_Proy_Oficial = 0
    ) %>%
    left_join(ed_p2, by = c("Sector_clean" = "Sector", "Módulo_clean" = "Módulo", "Turno_clean" = "Turno", "Var_clean" = "Variedad")) %>%
    mutate(
      Ha_Ajustada_Esc    = coalesce(Ha_Ajustada_Esc, 0),
      Kg_Ajustado_Esc    = coalesce(Kg_Ajustado_Esc, 0),
      Kg_Ajustado_Semana = round(Kg_Real_Prev + Kg_Ajustado_Esc, 1),
      Diferencia_Kg      = round(Kg_Real_Semana - Kg_Ajustado_Semana, 1),
      Pct_Desvio         = ifelse(Kg_Ajustado_Semana > 0, round((Diferencia_Kg / Kg_Ajustado_Semana) * 100, 1), NA_real_),
      Turno_Label        = sprintf("M%s-T%s (%s) 2ª Pas", Módulo_clean, Turno_clean, Var_clean),
      Motivo_Ajuste      = coalesce(Motivo_Ajuste, "Sin ajuste 2ª pasada"),
      Módulo             = Módulo_clean,
      Turno              = Turno_clean,
      Variedad           = Var_clean
    )
} else tibble()

res <- bind_rows(res_1ra, res_2da)

cat("=== Resultado Completo (Todas las pasadas) ===\n")
print(res %>% select(Módulo, Turno, Variedad, Pasada, Kg_Proy_Oficial, Kg_Ajustado_Semana, Kg_Real_Semana, Diferencia_Kg))

# Test con filtro 1ª Pasada
res_f1 <- res %>% filter(Pasada == "1ª Pasada")
cat("\n=== Con Filtro: 1ª Pasada ===\n")
print(res_f1 %>% select(Módulo, Turno, Variedad, Pasada, Kg_Proy_Oficial, Kg_Ajustado_Semana, Kg_Real_Semana))
stopifnot(res_f1$Kg_Ajustado_Semana == 252.7)
stopifnot(res_f1$Kg_Proy_Oficial == 252.7)

# Test con filtro 2ª Pasada
res_f2 <- res %>% filter(Pasada == "2ª Pasada")
cat("\n=== Con Filtro: 2ª Pasada ===\n")
print(res_f2 %>% select(Módulo, Turno, Variedad, Pasada, Kg_Proy_Oficial, Kg_Ajustado_Semana, Kg_Real_Semana))
stopifnot(res_f2$Kg_Ajustado_Semana == 147.2)

cat("\n>>> TEST PASSED 100%! <<<\n")
