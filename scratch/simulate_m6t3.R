library(dplyr)

df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")
hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

norm_codigo_valvula <- function(x) toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
limpiar_variedad <- function(x) toupper(trimws(as.character(x)))

sem_sel <- 45
dia_inicio_nom <- "Viernes"
dia_fin_nom <- "Viernes"
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- match(dia_inicio_nom, dias_nombres)
dia_fin_idx <- match(dia_fin_nom, dias_nombres)
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]
dias_previos_nom <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)
cols_ha_prev <- paste0("Ha_", dias_previos_nom)
cols_kg_prev <- paste0("Kg_", dias_previos_nom)

cat("dias_previos_nom:", paste(dias_previos_nom, collapse = ", "), "\n")
cat("dias_ajustables_nom:", paste(dias_ajustables_nom, collapse = ", "), "\n\n")

rc_sem <- rc_map %>% filter(as.numeric(Semana) == sem_sel)

rc_temp <- rc_sem %>% mutate(codigo_norm = norm_codigo_valvula(codigo))
ha_prev_vec <- numeric(nrow(rc_temp))
kg_prev_vec <- numeric(nrow(rc_temp))
for (d in dias_previos_nom) {
  col_h <- paste0("Ha_", d)
  col_k <- paste0("Kg_", d)
  val_h <- if (col_h %in% names(rc_temp)) coalesce(rc_temp[[col_h]], 0) else 0
  val_k <- if (col_k %in% names(rc_temp)) coalesce(rc_temp[[col_k]], 0) else 0
  es_ejecutado <- (d %in% c("Lunes", "Martes")) | (val_k > 0)
  ha_prev_vec <- ha_prev_vec + ifelse(es_ejecutado, val_h, 0)
  kg_prev_vec <- kg_prev_vec + val_k
}
rc_temp$Ha_Prev_Valv <- ha_prev_vec
rc_temp$Kg_Prev_Valv <- kg_prev_vec

rc_valv_avance <- rc_temp %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Prog      = sum(coalesce(Ha_Prev_Valv, 0), na.rm = TRUE),
    Kg_Prog      = sum(coalesce(Kg_Prev_Valv, 0), na.rm = TRUE),
    Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
    Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
    .groups      = "drop"
  )

cols_ha_sap_exist <- intersect(cols_ha_prev, names(hist_wide))
cols_kg_sap_exist <- intersect(cols_kg_prev, names(hist_wide))

sap_valv_avance <- hist_wide %>%
  filter(as.numeric(Semana) == sem_sel) %>%
  mutate(
    codigo_norm = norm_codigo_valvula(Codigo_Valv),
    Ha_Sap_Prev = if (length(cols_ha_sap_exist) > 0) rowSums(across(all_of(cols_ha_sap_exist)), na.rm = TRUE) else 0,
    Kg_Sap_Prev = if (length(cols_kg_sap_exist) > 0) rowSums(across(all_of(cols_kg_sap_exist)), na.rm = TRUE) else 0
  ) %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Prog_Sap      = sum(coalesce(Ha_Sap_Prev, 0), na.rm = TRUE),
    Kg_Prog_Sap      = sum(coalesce(Kg_Sap_Prev, 0), na.rm = TRUE),
    Ha_Lunes_Sap     = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Kg_Lunes_Sap     = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
    Ha_Martes_Sap    = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    Kg_Martes_Sap    = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
    .groups          = "drop"
  )

valvulas_sub <- df_valvulas %>%
  filter(as.character(Módulo) == "6", as.character(Turno) == "3") %>%
  mutate(
    codigo_norm = norm_codigo_valvula(codigo),
    Turno       = trimws(as.character(Turno)),
    Variedad    = limpiar_variedad(Variedad),
    Área        = suppressWarnings(as.numeric(Área))
  ) %>%
  left_join(rc_valv_avance, by = "codigo_norm") %>%
  left_join(sap_valv_avance, by = "codigo_norm") %>%
  mutate(
    Ha_Prog      = case_when(
      coalesce(Ha_Prog_Sap, 0) > 0                        ~ pmin(Área, Ha_Prog_Sap),
      coalesce(Kg_Prog_Sap, 0) > 0                        ~ Área,
      (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
      (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
      coalesce(Ha_Prog, 0) > 0 & coalesce(Kg_Prog, 0) > 0 ~ pmin(Área, Ha_Prog),
      TRUE                                                ~ 0
    ),
    Kg_Prog      = case_when(
      coalesce(Kg_Prog_Sap, 0) > 0 ~ Kg_Prog_Sap,
      coalesce(Kg_Prog, 0) > 0     ~ Kg_Prog,
      TRUE                         ~ 0
    ),
    Ha_Faltante   = pmax(0, Área - Ha_Prog)
  )

cat("=== DETALLE DE VÁLVULAS M6 T3 ===\n")
print(as.data.frame(valvulas_sub %>% select(codigo, Área, Ha_Prog_Sap, Kg_Prog_Sap, Ha_Prog, Kg_Prog, Ha_Faltante)))
cat("Suma Área:", sum(valvulas_sub$Área), "\n")
cat("Suma Ha_Prog:", sum(valvulas_sub$Ha_Prog), "\n")
cat("Suma Ha_Faltante:", sum(valvulas_sub$Ha_Faltante), "\n")
