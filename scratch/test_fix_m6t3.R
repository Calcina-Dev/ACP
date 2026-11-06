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

rc_sem <- rc_map %>% filter(as.numeric(Semana) == sem_sel)

rc_temp <- rc_sem %>% mutate(codigo_norm = norm_codigo_valvula(codigo))
dias_cols_all <- paste0("Ha_", dias_nombres)
dias_cols_ex  <- intersect(dias_cols_all, names(rc_temp))
if (length(dias_cols_ex) > 0) {
  mat_ha <- as.matrix(rc_temp[, dias_cols_ex])
  has_any <- rowSums(mat_ha > 0, na.rm = TRUE) > 0
  max_idx <- max.col(mat_ha, ties.method = "first")
  dias_noms_sub <- sub("^Ha_", "", dias_cols_ex)
  rc_temp$Dia_Ruta_Sem <- ifelse(has_any, dias_noms_sub[max_idx], NA_character_)
} else {
  rc_temp$Dia_Ruta_Sem <- NA_character_
}

rc_valv_avance <- rc_temp %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Lunes_Rc  = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Kg_Lunes_Rc  = if ("Kg_Lunes" %in% names(.)) sum(coalesce(Kg_Lunes, 0), na.rm = TRUE) else 0,
    Ha_Martes_Rc = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    Kg_Martes_Rc = if ("Kg_Martes" %in% names(.)) sum(coalesce(Kg_Martes, 0), na.rm = TRUE) else 0,
    Dia_Ruta_Sem = first(na.omit(Dia_Ruta_Sem)),
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
    Área        = suppressWarnings(as.numeric(Área)),
    Frecuencia_Dias = 7L,
    Dia_Auto_Valv   = NA_character_
  ) %>%
  left_join(rc_valv_avance, by = "codigo_norm") %>%
  left_join(sap_valv_avance, by = "codigo_norm") %>%
  mutate(
    # AVANCE EJECUTADO REAL ANTES DE DIA_INICIO:
    # 1. Si SAP tiene avance oficial (ha o kg) en los días previos, esa área ya se ejecutó.
    # 2. Si Lunes/Martes tuvieron ruta y dia_inicio es posterior, se toma la ruta de Lunes/Martes (cobertura inicio semana).
    # 3. NO considerar rutas planificadas de Miércoles/Jueves/etc. si NO tienen respaldo de kilos en SAP,
    #    porque son proyecciones que aún no se han cosechado y deben ajustarse en los días disponibles.
    Ha_Prog = case_when(
      coalesce(Ha_Prog_Sap, 0) > 0                        ~ pmin(Área, Ha_Prog_Sap),
      coalesce(Kg_Prog_Sap, 0) > 0                        ~ Área,
      (dia_inicio_idx > 1 & coalesce(Ha_Lunes_Rc, 0) > 0) ~ pmin(Área, Ha_Lunes_Rc),
      (dia_inicio_idx > 2 & coalesce(Ha_Martes_Rc, 0) > 0) ~ pmin(Área, Ha_Martes_Rc),
      TRUE                                                ~ 0
    ),
    Kg_Prog = case_when(
      coalesce(Kg_Prog_Sap, 0) > 0 ~ Kg_Prog_Sap,
      (dia_inicio_idx > 1 & coalesce(Kg_Lunes_Rc, 0) > 0) ~ Kg_Lunes_Rc,
      (dia_inicio_idx > 2 & coalesce(Kg_Martes_Rc, 0) > 0) ~ Kg_Martes_Rc,
      TRUE                         ~ 0
    ),
    Ha_Lunes_Rc   = pmax(coalesce(Ha_Lunes_Rc, 0), coalesce(Ha_Lunes_Sap, 0)),
    Ha_Martes_Rc  = pmax(coalesce(Ha_Martes_Rc, 0), coalesce(Ha_Martes_Sap, 0)),
    Kg_Lunes_Valv = coalesce(Kg_Lunes_Sap, 0) + ifelse(coalesce(Kg_Lunes_Sap, 0) == 0, coalesce(Kg_Lunes_Rc, 0), 0),
    Kg_Martes_Valv = coalesce(Kg_Martes_Sap, 0) + ifelse(coalesce(Kg_Martes_Sap, 0) == 0, coalesce(Kg_Martes_Rc, 0), 0),
    Ha_Faltante   = pmax(0, Área - Ha_Prog)
  )

cat("=== DETALLE DE VÁLVULAS M6 T3 CON FIX ===\n")
print(as.data.frame(valvulas_sub %>% select(codigo, Área, Ha_Prog_Sap, Kg_Prog_Sap, Ha_Prog, Kg_Prog, Ha_Faltante)))
cat("\nSuma Área Total:", sum(valvulas_sub$Área), "\n")
cat("Suma Ha_Prog:", sum(valvulas_sub$Ha_Prog), "\n")
cat("Suma Ha_Faltante (Pendientes):", sum(valvulas_sub$Ha_Faltante), "\n")

valvulas_sub <- valvulas_sub %>%
  mutate(
    Dia_Base_Ant = "Jueves",
    Dia_Natural_Valv = case_when(
      !is.na(Dia_Ruta_Sem) & nzchar(Dia_Ruta_Sem) ~ Dia_Ruta_Sem,
      TRUE ~ dia_inicio_nom
    ),
    idx_natural = match(Dia_Natural_Valv, dias_nombres),
    idx_natural = ifelse(is.na(idx_natural), dia_inicio_idx, idx_natural),
    idx_asig_rango = pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_natural)),
    Dia_Habitual_Calc = dias_nombres[idx_asig_rango],
    Tiene_Ciclo_Previo_Valv = TRUE
  )

turno_avance_ha <- valvulas_sub %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Prog_Turno     = sum(Ha_Prog, na.rm = TRUE),
    Kg_Prog_Turno     = sum(Kg_Prog, na.rm = TRUE),
    Ha_Lunes_Turno    = sum(Ha_Lunes_Rc, na.rm = TRUE),
    Ha_Martes_Turno   = sum(Ha_Martes_Rc, na.rm = TRUE),
    Ha_Faltante_Turno = sum(Ha_Faltante, na.rm = TRUE),
    Area_Total_Turno  = sum(Área, na.rm = TRUE),
    Valvulas_Total    = n(),
    Frecuencia_Dias   = first(Frecuencia_Dias),
    .groups           = "drop"
  ) %>%
  mutate(
    Ha_Manual_1ra          = 0,
    Ha_Manual_2da          = 0,
    Ha_Faltante_Turno_Neto = pmax(0, Area_Total_Turno - Ha_Prog_Turno - Ha_Manual_1ra),
    Turno_Completo_1ra     = (Ha_Faltante_Turno_Neto <= 0.05)
  )

rend_turno <- turno_avance_ha %>%
  mutate(
    Rend_Turno_Aplicar = 773.52,
    Esta_Proyectado = TRUE,
    Turno_Completo_Lun_Mar = FALSE,
    Es_Ciclo_Corto = FALSE,
    Semana_Ant_No_Finalizo = FALSE,
    Motivo_Ajuste = "Avance >30%"
  )

valvulas_calc <- valvulas_sub %>%
  left_join(rend_turno, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Valvula_Cerro_En_Lun_Mar = FALSE,
    Ha_Cerrada_Lun_Mar = 0,
    Tiene_Cosecha_Act = TRUE
  )

reg_regular <- valvulas_calc %>%
  filter(coalesce(Esta_Proyectado, FALSE), !Turno_Completo_Lun_Mar, !coalesce(Turno_Completo_1ra, FALSE)) %>%
  mutate(
    Ha_Regular = case_when(
      Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar & Ha_Cerrada_Lun_Mar >= 0.90 * Área ~ 0,
      Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ pmax(0, Área - Ha_Cerrada_Lun_Mar),
      Semana_Ant_No_Finalizo ~ Área,
      Tiene_Cosecha_Act ~ Ha_Faltante,
      TRUE ~ Ha_Faltante
    ),
    Dia_Asignado = case_when(
      limpiar_variedad(Variedad) == "ROCIO" ~ dia_inicio_nom,
      Semana_Ant_No_Finalizo & Tiene_Ciclo_Previo_Valv ~ Dia_Habitual_Calc,
      Semana_Ant_No_Finalizo & !Tiene_Ciclo_Previo_Valv & !Valvula_Cerro_En_Lun_Mar ~ dia_inicio_nom,
      Semana_Ant_No_Finalizo ~ dia_inicio_nom,
      Tiene_Cosecha_Act & Ha_Prog > 0 & Ha_Faltante > 0.02 ~ dia_inicio_nom,
      TRUE ~ Dia_Habitual_Calc
    ),
    idx_asig = match(Dia_Asignado, dias_nombres),
    Dia_Asignado = case_when(
      !is.na(idx_asig) & idx_asig > dia_fin_idx    ~ dia_fin_nom,
      !is.na(idx_asig) & idx_asig < dia_inicio_idx ~ dia_inicio_nom,
      TRUE                                         ~ Dia_Asignado
    )
  ) %>%
  select(-idx_asig) %>%
  group_by(Módulo, Turno, Variedad) %>%
  mutate(
    tot_ha_reg = sum(Ha_Regular, na.rm = TRUE),
    Ha_Regular = if_else(
      tot_ha_reg > coalesce(Ha_Faltante_Turno_Neto, tot_ha_reg) & tot_ha_reg > 0,
      round(Ha_Regular * (coalesce(Ha_Faltante_Turno_Neto, tot_ha_reg) / tot_ha_reg), 2),
      Ha_Regular
    ),
    Rend_Valv_Regular = coalesce(Rend_Turno_Aplicar, 0),
    Kg_Asignado = round(Ha_Regular * Rend_Valv_Regular, 2),
    Ha_Asignada = Ha_Regular,
    Num_Pasada = 1L,
    Es_Ruta_Sin_Kg = FALSE
  ) %>%
  ungroup() %>%
  filter(Kg_Asignado > 0, Ha_Regular > 0, Dia_Asignado %in% dias_ajustables_nom)

cat("\n=== REG_REGULAR RESULT ===\n")
print(as.data.frame(reg_regular %>% select(Turno, codigo, Ha_Regular, Rend_Valv_Regular, Kg_Asignado, Dia_Asignado)))
cat("Total Ha Asignadas a Viernes:", sum(reg_regular$Ha_Asignada), "\n")
cat("Total Kg Asignados a Viernes:", sum(reg_regular$Kg_Asignado), "\n")
