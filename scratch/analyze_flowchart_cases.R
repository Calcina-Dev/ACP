load("G:/Unidades compartidas/Compartido Proyecciones/3.- Ar\u00e1ndano/datos_maestros_cosecha_2026.RData")
suppressPackageStartupMessages(library(dplyr))

limpiar_variedad <- function(x) toupper(trimws(gsub("\\s+", " ", as.character(x))))
norm_codigo_valvula <- function(x) toupper(trimws(gsub("\\s+", "", as.character(x))))

# Catalogo de valvulas por turno
cat_turno <- df_valvulas %>%
  mutate(
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad),
    Área = as.numeric(Área)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Area_Cat = sum(Área, na.rm = TRUE), N_Valvs_Cat = n(), .groups = "drop")

# Semana 43 historico
s43 <- df_hist_oficial_wide %>%
  filter(Semana == 43) %>%
  mutate(
    Módulo = as.character(Módulo_Norm),
    Turno = as.character(Turno_Norm),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_S43 = sum(Kg_Total_Sem, na.rm = TRUE),
    Ha_S43 = sum(Ha_Total_Sem, na.rm = TRUE),
    N_Valvs_S43 = n_distinct(Codigo_Valv[Kg_Total_Sem > 0]),
    Kg_Sab_S43 = sum(Kg_Sábado, na.rm = TRUE),
    Kg_Vie_S43 = sum(Kg_Viernes, na.rm = TRUE),
    .groups = "drop"
  )

# Rutas Semana 44
rc <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
rc44 <- rc %>%
  filter(Semana == 44) %>%
  mutate(
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Lun_S44 = sum(coalesce(Ha_Lunes, 0), na.rm = TRUE),
    Ha_Mar_S44 = sum(coalesce(Ha_Martes, 0), na.rm = TRUE),
    Ha_Prog_S44 = sum(coalesce(Ha_Total_Prog, 0), na.rm = TRUE),
    .groups = "drop"
  )

# SAP Semana 44
sap44 <- df_cosecha %>%
  filter(Semana == 44, Campaña == "2026") %>%
  mutate(
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad),
    wday_num = lubridate::wday(Fecha, week_start = 1)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(
    Kg_Lun_SAP = sum(`Kg Total`[wday_num == 1], na.rm = TRUE),
    Kg_Mar_SAP = sum(`Kg Total`[wday_num == 2], na.rm = TRUE),
    Kg_Tot_SAP = sum(`Kg Total`, na.rm = TRUE),
    .groups = "drop"
  )

# Proyeccion Semana 44
proy44 <- df_proyecciones %>%
  filter(Semana == 44, Campaña == "2026") %>%
  mutate(
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(Proy_S44 = sum(Proyección, na.rm = TRUE), .groups = "drop")

resumen <- cat_turno %>%
  left_join(s43, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(rc44, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(sap44, by = c("Módulo", "Turno", "Variedad")) %>%
  left_join(proy44, by = c("Módulo", "Turno", "Variedad")) %>%
  mutate(
    across(where(is.numeric), ~coalesce(., 0)),
    Pct_S43 = round(100 * Ha_S43 / pmax(0.1, Area_Cat), 1),
    Pct_S44 = round(100 * Ha_Prog_S44 / pmax(0.1, Area_Cat), 1),
    Rend_S43 = round(Kg_S43 / pmax(0.1, Ha_S43), 0),
    Rend_Proy44 = round(Proy_S44 / pmax(0.1, Area_Cat), 0),
    Rend_Act44 = ifelse(Ha_Prog_S44 > 0, round(Kg_Tot_SAP / Ha_Prog_S44, 0), 0)
  )

cat("=== CASOS DONDE SEMANA 43 NO FINALIZO (Pct_S43 < 95% y > 0%) ===\n")
resumen %>%
  filter(Pct_S43 > 0 & Pct_S43 < 95) %>%
  select(Módulo, Turno, Variedad, Area_Cat, Ha_S43, Pct_S43, Kg_Sab_S43, Ha_Lun_S44, Kg_Lun_SAP, Rend_S43, Rend_Proy44) %>%
  as.data.frame() %>%
  print()

cat("\n=== CASOS CON COSECHA PARCIAL EN S44 (0 < Pct_S44 < 90%) ===\n")
resumen %>%
  filter(Pct_S44 > 0 & Pct_S44 < 90) %>%
  select(Módulo, Turno, Variedad, Area_Cat, Ha_Prog_S44, Pct_S44, Kg_Tot_SAP, Rend_S43, Rend_Act44, Rend_Proy44) %>%
  as.data.frame() %>%
  print()
