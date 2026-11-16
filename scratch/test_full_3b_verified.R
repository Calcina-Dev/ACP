load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

limpiar_variedad <- function(x) toupper(trimws(as.character(x)))

desplazar_dia_2da_pasada <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(idx_base == 1L, ifelse(!is.na(frec_int) & frec_int <= 3L, 3L, 4L), 3L)
  idx_target <- pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_base + desp))
  dia_target <- dias_nombres[idx_target]
  dia_res <- ifelse(is.na(dia_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
  idx_res <- match(dia_res, dias_nombres)
  ifelse(!is.na(idx_res) & idx_res > dia_fin_idx, dia_fin_nom, dia_res)
}

dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- 2L
dia_inicio_nom <- "Martes"
dia_fin_idx <- 5L
dia_fin_nom <- "Viernes"
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

esc <- readRDS("Actualizacion Semanal/escenarios_ajuste_semanal.rds")
e4 <- esc[4, ]
vdata <- e4$Valvulas_Data[[1]]

reg_regular <- vdata %>% filter(Num_Pasada == 1) %>% mutate(codigo_norm = codigo)
reg_2da_lun_orig <- vdata %>% filter(Num_Pasada == 2) %>% mutate(codigo_norm = codigo)

reg_2da_lun <- reg_2da_lun_orig %>%
  mutate(
    Dia_Asignado = as.character(desplazar_dia_2da_pasada("Lunes", 6, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom))
  )

valvs_mar_reg_info <- if (dia_inicio_idx <= 2L && nrow(reg_regular) > 0) {
  reg_regular %>%
    filter(Dia_Asignado == "Martes", Num_Pasada == 1L, Ha_Asignada > 0) %>%
    group_by(codigo_norm) %>%
    summarise(
      Ha_Mar_Reg = sum(Ha_Asignada, na.rm = TRUE),
      Rend_Mar_Reg = sum(Kg_Asignado, na.rm = TRUE) / pmax(0.01, sum(Ha_Asignada, na.rm = TRUE)),
      .groups = "drop"
    )
} else {
  tibble(codigo_norm = character(), Ha_Mar_Reg = numeric(), Rend_Mar_Reg = numeric())
}

ha_2da_previa_mar <- bind_rows(
  if (exists("reg_2da_lun") && nrow(reg_2da_lun) > 0) reg_2da_lun %>% select(Módulo, Turno, Variedad, Ha_Asignada) else tibble()
) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(ha_ya_2da_asig = sum(Ha_Asignada, na.rm = TRUE), .groups = "drop")

# Build mock valvulas_calc for Modulo 12
m12_valvs <- df_valvulas %>% filter(as.character(Módulo) == "12") %>%
  mutate(
    codigo_norm = paste0("M", Módulo, "-T", Turno, "-V", Válvula),
    codigo = codigo_norm,
    id = paste0(codigo_norm, "_test"),
    Sector = as.character(Sector),
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Esta_Proyectado = TRUE,
    Semana_Ant_No_Finalizo = FALSE,
    Es_Ciclo_Corto = TRUE,
    Ha_Cosechada_Martes = 0,
    Turno_Completo_2da = FALSE,
    Valvula_Tiene_2da_Real = FALSE,
    Rend_Valv_Martes = 150,
    Rend_Turno_Aplicar = 150,
    Frecuencia_Dias = 6,
    Pct_Pasada_Mar = 60
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  mutate(
    Area_Total_Turno = sum(Área, na.rm = TRUE),
    Ha_Faltante_2da_Neto = Area_Total_Turno
  ) %>%
  ungroup()

reg_2da_mar <- m12_valvs %>%
  left_join(valvs_mar_reg_info, by = "codigo_norm") %>%
  left_join(ha_2da_previa_mar, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    Ha_Mar_Reg = coalesce(Ha_Mar_Reg, 0),
    Rend_Mar_Reg = coalesce(Rend_Mar_Reg, 0),
    ha_ya_2da_asig = coalesce(ha_ya_2da_asig, 0),
    Cupo_2da_Mar_Turno = pmax(0, coalesce(Ha_Faltante_2da_Neto, Area_Total_Turno) - ha_ya_2da_asig),
    Ha_Base_Mar = if_else(Ha_Cosechada_Martes > 0, Ha_Cosechada_Martes, Ha_Mar_Reg),
    Rend_Base_Mar = if_else(Ha_Cosechada_Martes > 0, Rend_Valv_Martes, ifelse(Rend_Mar_Reg > 0, Rend_Mar_Reg, Rend_Turno_Aplicar))
  ) %>%
  filter(
    coalesce(Esta_Proyectado, FALSE),
    !Semana_Ant_No_Finalizo,
    Es_Ciclo_Corto,
    Ha_Base_Mar > 0,
    Cupo_2da_Mar_Turno > 0.05,
    !coalesce(Turno_Completo_2da, FALSE),
    !coalesce(Valvula_Tiene_2da_Real, FALSE)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  mutate(
    tot_2da_mar = sum(Ha_Base_Mar, na.rm = TRUE),
    Ha_2da_Corta = if_else(
      tot_2da_mar > Cupo_2da_Mar_Turno & tot_2da_mar > 0,
      round(Ha_Base_Mar * (Cupo_2da_Mar_Turno / tot_2da_mar), 2),
      Ha_Base_Mar
    ),
    Dia_Asignado   = as.character(desplazar_dia_2da_pasada('Martes', Frecuencia_Dias, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),
    Pct_Rend       = if_else(limpiar_variedad(Variedad) == 'ROCIO', coalesce(Pct_Pasada_Mar, 60) / 100, 1.0),
    Rend_2da_Aplicar = round(Rend_Base_Mar * Pct_Rend, 2),
    Kg_Asignado    = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
    Ha_Asignada    = Ha_2da_Corta,
    Num_Pasada     = 2L,
    Es_Ruta_Sin_Kg = FALSE,
    Motivo_Ajuste  = sprintf('2da pasada Rocío (Rend Martes: %s kg/ha • %d%% sin floración)', 
                             format(round(Rend_Base_Mar), big.mark = ','), round(Pct_Rend * 100))
  ) %>%
  ungroup() %>%
  filter(Kg_Asignado > 0, Ha_Asignada > 0, Dia_Asignado %in% dias_ajustables_nom)

cat("Summary by Turno in reg_2da_mar:\n")
print(reg_2da_mar %>% group_by(Turno, Dia_Asignado) %>% summarise(Ha = sum(Ha_Asignada), Kg = sum(Kg_Asignado), .groups = "drop"))

cat("\nDetail for Turno 1:\n")
print(reg_2da_mar %>% filter(Turno == "1") %>% select(Turno, codigo_norm, Dia_Asignado, Ha_Asignada, Kg_Asignado, Motivo_Ajuste))

cat("\nDetail for Turno 5:\n")
print(reg_2da_mar %>% filter(Turno == "5") %>% select(Turno, codigo_norm, Dia_Asignado, Ha_Asignada, Kg_Asignado, Motivo_Ajuste))
