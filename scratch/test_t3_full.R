suppressMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

norm_codigo_valvula <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
}
limpiar_variedad <- function(x) {
  toupper(trimws(as.character(x)))
}
desplazar_dia_frecuencia <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(frec_int <= 4L, frec_int, frec_int - 7L)
  idx_target <- pmin(dia_fin_idx, pmax(1L, idx_base + desp))
  dia_target <- ifelse(is.na(idx_target), dia_inicio_nom, dias_nombres[idx_target])
  dia_res <- ifelse(is.na(idx_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
  idx_res <- match(dia_res, dias_nombres)
  ifelse(!is.na(idx_res) & idx_res > dia_fin_idx, dia_fin_nom, dia_res)
}

hist_wide <- readRDS("D:/ACP/Rutas de Cosecha/historico_oficial_wide_2026.rds")
rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
df_valvulas <- readRDS("D:/ACP/Rutas de Cosecha/valvulas_normalizadas.rds")

sem_sel <- 45
sem_ant <- 44
dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
cols_dias <- dias_nombres
cols_kg <- paste0("Kg_", cols_dias)

df_ant_valvs <- hist_wide %>%
  filter(as.numeric(Semana) == sem_ant)
df_ant_valvs$Kg_Total_Ant <- rowSums(df_ant_valvs[, intersect(cols_kg, names(df_ant_valvs))], na.rm = TRUE)
df_ant_valvs$Ha_Total_Ant <- rowSums(df_ant_valvs[, intersect(paste0("Ha_", cols_dias), names(df_ant_valvs))], na.rm = TRUE)

# 7d map
valv_7d_map <- df_ant_valvs %>%
  filter(Kg_Total_Ant > 0) %>%
  mutate(
    codigo_norm = norm_codigo_valvula(Codigo_Valv),
    Dia_7d = {
      mat_kg <- as.matrix(select(., any_of(cols_kg)))
      cols_presentes <- intersect(cols_kg, names(.))
      if (ncol(mat_kg) > 0) {
        max_idx <- max.col(mat_kg, ties.method = "first")
        sub("^Kg_", "", cols_presentes[max_idx])
      } else NA_character_
    }
  ) %>%
  select(codigo_norm, Dia_7d, Kg_Total_Ant, Ha_Total_Ant)

turno_dia_habitual <- df_ant_valvs %>%
  filter(Kg_Total_Ant > 0) %>%
  group_by(Módulo = as.character(Módulo_Norm), Turno = as.character(Turno_Norm)) %>%
  summarise(
    Dia_Habitual = {
      tot_dia <- colSums(across(any_of(cols_kg)), na.rm = TRUE)
      if (length(tot_dia) > 0 && max(tot_dia) > 0) sub("^Kg_", "", names(which.max(tot_dia))) else NA_character_
    },
    .groups = "drop"
  )

t3_7d <- valv_7d_map %>% filter(codigo_norm %in% c("M2T3V9", "M2T3V10", "M2T3V11", "M2T3V12"))
cat("=== valv_7d_map for T3 ===\n")
print(t3_7d)

t3_hab <- turno_dia_habitual %>% filter(Módulo == "2", Turno == "3")
cat("=== turno_dia_habitual for M2 T3 ===\n")
print(t3_hab)
