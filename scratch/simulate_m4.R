suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

load("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/datos_maestros_cosecha_2026.RData")

limpiar_variedad <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x_str <- toupper(trimws(as.character(x)))
  x_str <- chartr("ÁÉÍÓÚÜ", "AEIOUU", x_str)
  as.character(ifelse(grepl("EB\\s*9\\s*[-_.]\\s*2", x_str, ignore.case = TRUE), "MAGICA", x_str))
}

rc_df <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
ed_df <- readRDS("G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/ediciones_manuales_cosecha.rds")

# Valvulas Módulo 4
m4_valvs <- df_valvulas %>%
  filter(as.character(Módulo) == "4") %>%
  mutate(
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Área = round(sum(Área, na.rm = TRUE), 2), .groups = "drop")

# Proyecciones Módulo 4
m4_proy <- df_proyecciones %>%
  filter(as.character(Campaña) == "2026", Semana == 44, as.character(Módulo) == "4") %>%
  mutate(
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Proyección = round(sum(Proyección, na.rm = TRUE), 2), .groups = "drop")

# Cosecha SAP Lunes
m4_sap <- df_cosecha %>%
  filter(as.character(Campaña) == "2026", Semana == 44, as.character(Módulo) == "4") %>%
  mutate(
    Sector = as.character(Sector),
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Lunes_SAP = round(sum(.data[["Kg Total"]], na.rm = TRUE), 2),
    .groups = "drop"
  )

# Rutas de campo
m4_rc <- rc_df %>%
  filter(as.character(Campaña) == "2026", Semana == 44, as.character(Módulo) == "4", Tipo_Registro == "CosechaCampo") %>%
  mutate(
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad),
    Ha_Lunes = coalesce(Ha_Lunes, 0),
    Ha_Martes = coalesce(Ha_Martes, 0)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Rc = round(sum(pmax(coalesce(Ha_Total_Prog, 0), Ha_Lunes + Ha_Martes), na.rm = TRUE), 2),
    .groups = "drop"
  )

# Ediciones aplicadas con Ha_Asignada
dias_cols <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

m4_ed_vals <- ed_df %>%
  filter(as.character(Campaña) == "2026", Semana == 44, as.character(Módulo) == "4", Campo %in% dias_cols) %>%
  mutate(
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad)
  ) %>%
  select(Sector, Módulo, Turno, Variedad, Campo, Valor) %>%
  pivot_wider(names_from = Campo, values_from = Valor, values_fill = 0)

for (d in dias_cols) {
  if (!d %in% names(m4_ed_vals)) m4_ed_vals[[d]] <- 0
}

m4_ed_ha <- ed_df %>%
  filter(as.character(Campaña) == "2026", Semana == 44, as.character(Módulo) == "4", Campo %in% dias_cols) %>%
  mutate(
    Turno = as.character(Turno),
    Variedad = limpiar_variedad(Variedad),
    Ha_val = coalesce(as.numeric(Ha_Asignada), 0)
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Ha_Ajustada_Real = round(sum(Ha_val, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Consolidador final
tabla_m4 <- m4_valvs %>%
  left_join(m4_proy, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(m4_sap, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(m4_rc, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(m4_ed_vals, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  left_join(m4_ed_ha, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    across(any_of(c("Proyección", "Lunes_SAP", "Ha_Rc", dias_cols)), ~ coalesce(.x, 0)),
    Lunes = ifelse(Lunes_SAP > 0, Lunes_SAP, Lunes),
    Ha_Recorrida = Ha_Rc,
    Kg_SAP_Real = Lunes_SAP,
    Ajuste = round(Lunes + Martes + Miércoles + Jueves + Viernes + Sábado, 2),
    `Kg/Ha Proy` = ifelse(Área > 0 & Proyección > 0, round(Proyección / Área, 2), NA_real_),
    `Kg/Ha Real` = ifelse(Ha_Recorrida > 0 & Kg_SAP_Real > 0, round(Kg_SAP_Real / Ha_Recorrida, 2), NA_real_),
    `Kg/Ha Ajustado` = ifelse(Área > 0, round(Ajuste / Área, 2), 0),
    `Ha Ajustada` = round(coalesce(Ha_Ajustada_Real, 0), 2),
    Suma_Ha = round(Ha_Recorrida + `Ha Ajustada`, 2),
    Diferencia = round(Ajuste - Proyección, 2)
  )

cat("\n========================= TABLA MODULO 4 (DESDE DISCO ACTUALIZADO) =========================\n\n")
tabla_m4 %>%
  select(Turno, Variedad, Proyección, Área, Ha_Recorrida, `Ha Ajustada`, Suma_Ha, `Kg/Ha Proy`, `Kg/Ha Real`, `Kg/Ha Ajustado`, Lunes, Viernes, Sábado, Ajuste) %>%
  print(width = 150)
