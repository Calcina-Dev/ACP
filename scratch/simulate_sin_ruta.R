suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
rc_raw <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")

# Helper to normalize valve codes
norm_codigo_valvula <- function(x) {
  if (is.null(x)) return(character(0))
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", "", x)
  x <- sub("^VALVULA[-_]?", "", x)
  x <- sub("^V[-_]?", "", x)
  m_sub <- sub(".*M0*([0-9]+(?:\\.[0-9]+)?).*", "\\1", x)
  t_sub <- sub(".*T0*([0-9]+).*", "\\1", x)
  v_sub <- sub(".*V0*([0-9]+)$", "\\1", x)
  valid <- grepl("M0*[0-9]+.*T0*[0-9]+.*V0*[0-9]+", x)
  ifelse(valid, sprintf("M%s-T%s-V%s", m_sub, t_sub, v_sub), x)
}

sem_num <- 45
sap_sem <- df_hist_oficial_wide %>% filter(as.numeric(Semana) == sem_num)
rc_sem <- rc_raw %>% filter(as.numeric(Semana) == sem_num)

col_cod_rc <- intersect(c("Codigo_Valvula", "codigo", "Codigo_Valv"), names(rc_sem))[1]

dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

# Aggregate SAP by normalized valve
sap_agg <- sap_sem %>%
  mutate(codigo_norm = norm_codigo_valvula(Codigo_Valv)) %>%
  group_by(codigo_norm) %>%
  summarise(
    Sector = first(Sector),
    Módulo = first(Módulo_Norm),
    Turno = first(Turno_Norm),
    Variedad = first(Variedad),
    Kg_Lunes = sum(Kg_Lunes, na.rm=TRUE),
    Kg_Martes = sum(Kg_Martes, na.rm=TRUE),
    Kg_Miércoles = sum(Kg_Miércoles, na.rm=TRUE),
    Kg_Jueves = sum(Kg_Jueves, na.rm=TRUE),
    Kg_Viernes = sum(Kg_Viernes, na.rm=TRUE),
    Kg_Sábado = sum(Kg_Sábado, na.rm=TRUE),
    Kg_Total = sum(Kg_Total_Sem, na.rm=TRUE),
    .groups = "drop"
  )

# Aggregate RC by normalized valve
rc_agg <- rc_sem %>%
  filter(!("Tipo_Registro" %in% names(.)) | Tipo_Registro != "Rectificacion") %>%
  mutate(codigo_norm = norm_codigo_valvula(.data[[col_cod_rc]])) %>%
  group_by(codigo_norm) %>%
  summarise(
    Ha_Lunes = sum(Ha_Lunes, na.rm=TRUE),
    Ha_Martes = sum(Ha_Martes, na.rm=TRUE),
    Ha_Miércoles = sum(Ha_Miércoles, na.rm=TRUE),
    Ha_Jueves = sum(Ha_Jueves, na.rm=TRUE),
    Ha_Viernes = sum(Ha_Viernes, na.rm=TRUE),
    Ha_Sábado = sum(Ha_Sábado, na.rm=TRUE),
    Ha_Total = sum(Ha_Total_Prog, na.rm=TRUE),
    .groups = "drop"
  )

# Join SAP and RC
joined <- full_join(sap_agg, rc_agg, by = "codigo_norm") %>%
  mutate(
    across(starts_with("Kg_"), ~ coalesce(.x, 0)),
    across(starts_with("Ha_"), ~ coalesce(.x, 0))
  )

cat("Total distinct valves in joined:", nrow(joined), "\n")
cat("Valves with SAP > 0:", sum(joined$Kg_Total > 0), "\n")
cat("Valves with RC > 0:", sum(joined$Ha_Total > 0), "\n")
cat("Valves with SAP > 0 & RC == 0:", sum(joined$Kg_Total > 0 & joined$Ha_Total == 0), "\n")

# Now let's analyze day by day:
# For each day d:
# If Kg_d > 0:
# Is there Ha on day d? If yes -> Coincide.
# If Ha on day d == 0:
# Is there Ha on day d-1 (previous day)? If yes -> Desfase D+1 (adjacent reception).
# If neither day d nor day d-1 has Ha > 0: -> Kilos SAP SIN RUTA!

dias_order <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

sin_ruta_cases <- list()

for (i in 1:nrow(joined)) {
  r <- joined[i, ]
  for (d_idx in seq_along(dias_order)) {
    d <- dias_order[d_idx]
    kg_col <- paste0("Kg_", d)
    ha_col <- paste0("Ha_", d)
    kg_val <- r[[kg_col]]
    if (kg_val > 0) {
      ha_val <- r[[ha_col]]
      ha_prev <- 0
      if (d_idx > 1) {
        prev_d <- dias_order[d_idx - 1]
        ha_prev <- r[[paste0("Ha_", prev_d)]]
      }
      if (ha_val == 0 && ha_prev == 0) {
        sin_ruta_cases[[length(sin_ruta_cases) + 1]] <- tibble(
          codigo_norm = r$codigo_norm,
          Dia = d,
          Kg_SAP = kg_val,
          Ha_Total_Valv = r$Ha_Total,
          Kg_Total_Valv = r$Kg_Total
        )
      }
    }
  }
}

df_sin_ruta <- bind_rows(sin_ruta_cases)
cat("\n--- DETECTED SAP KILOS WITHOUT ROUTE (day d has 0 ha and day d-1 has 0 ha) ---\n")
print(df_sin_ruta)
cat("\nTotal valves with sin ruta:", length(unique(df_sin_ruta$codigo_norm)), "\n")
cat("Total kg sin ruta:", sum(df_sin_ruta$Kg_SAP), "\n")
