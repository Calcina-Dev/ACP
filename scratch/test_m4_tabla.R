source("d:/ACP/scratch/test_color_ruta_sin_kg.R")
library(tidyr)

# Simular las columnas finales en Módulo 4
# Universo base simulado para M4 T1..T8
area_cat_m4 <- df_valvulas %>%
  filter(Módulo == "4") %>%
  mutate(codigo_norm = norm_codigo_valvula(codigo)) %>%
  group_by(Turno = as.character(Turno), Variedad = limpiar_variedad(Variedad)) %>%
  summarise(Área = sum(as.numeric(Área), na.rm = TRUE), .groups = "drop")

proy_m4 <- df_proyecciones %>%
  filter(Semana == 44, Campaña == "2026", Módulo_join == "4") %>%
  group_by(Turno = as.character(Turno_join), Variedad = limpiar_variedad(Variedad_join)) %>%
  summarise(Proyección = sum(Proyección, na.rm = TRUE), .groups = "drop")

rc_sem <- rc %>% filter(Semana == 44)
rc_m4 <- rc_sem %>%
  filter(Módulo == "4") %>%
  group_by(Turno = as.character(Turno), Variedad = limpiar_variedad(Variedad)) %>%
  summarise(Ha_Rc = sum(Ha_Total_Prog, na.rm = TRUE), .groups = "drop")

sap_m4 <- df_cosecha %>%
  filter(Semana == 44, Campaña == "2026", Módulo == "4") %>%
  mutate(wday_num = lubridate::wday(Fecha, week_start = 1)) %>%
  group_by(Turno = as.character(Turno), Variedad = limpiar_variedad(Variedad)) %>%
  summarise(
    Lunes_SAP = sum(`Kg Total`[wday_num == 1], na.rm = TRUE),
    Kg_SAP_Real = sum(`Kg Total`, na.rm = TRUE),
    .groups = "drop"
  )

ed_m4 <- comb %>%
  filter(Módulo == "4") %>%
  pivot_wider(names_from = Campo, values_from = Valor, values_fill = 0)

tabla_m4 <- area_cat_m4 %>%
  left_join(proy_m4, by = c("Turno", "Variedad")) %>%
  left_join(rc_m4, by = c("Turno", "Variedad")) %>%
  left_join(sap_m4, by = c("Turno", "Variedad")) %>%
  left_join(ed_m4 %>% select(Turno, Variedad, any_of(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado"))), by = c("Turno", "Variedad")) %>%
  mutate(
    across(any_of(c("Proyección", "Área", "Ha_Rc", "Lunes_SAP", "Kg_SAP_Real", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")), ~ coalesce(.x, 0)),
    `Ha Recorrida` = round(Ha_Rc, 2),
    Lunes          = ifelse(Lunes_SAP > 0, Lunes_SAP, Lunes),
    Ajuste         = round(Lunes + Martes + Miércoles + Jueves + Viernes + Sábado, 2),
    `Ha Ajustada`  = case_when(
      Ajuste <= 0 ~ 0,
      `Ha Recorrida` > 0 ~ round(pmax(0, Área - `Ha Recorrida`), 2),
      Ajuste > 0 ~ round(Área, 2),
      TRUE ~ 0
    ),
    `Kg/Ha Proy`     = ifelse(Área > 0 & Proyección > 0, round(Proyección / Área, 2), NA_real_),
    `Kg/Ha Real`     = ifelse(`Ha Recorrida` > 0 & Kg_SAP_Real > 0, round(Kg_SAP_Real / `Ha Recorrida`, 2), NA_real_),
    `Kg/Ha Ajustado` = ifelse(Área > 0, round(Ajuste / Área, 2), 0),
    Diferencia       = round(Ajuste - Proyección, 2)
  ) %>%
  arrange(suppressWarnings(as.numeric(Turno)))

cat("\n=== RESULTADO FINAL DE MÓDULO 4 CON COLUMNAS ORDENADAS ===\n")
print(as.data.frame(
  tabla_m4 %>%
    select(Turno, Variedad, Proyección, Área, `Ha Recorrida`, `Ha Ajustada`, `Kg/Ha Proy`, `Kg/Ha Real`, `Kg/Ha Ajustado`, Lunes, Martes, Viernes, Sábado, Ajuste, Diferencia)
))

cat("\n=== SUMA DE HECTÁREAS ===\n")
cat("Área Total:", sum(tabla_m4$Área), "\n")
cat("Ha Recorrida:", sum(tabla_m4$`Ha Recorrida`), "\n")
cat("Ha Ajustada:", sum(tabla_m4$`Ha Ajustada`), "\n")
cat("Suma (Ha Recorrida + Ha Ajustada):", sum(tabla_m4$`Ha Recorrida`) + sum(tabla_m4$`Ha Ajustada`), "\n")
