suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cat("=== SOLO VENTURA (M7 - T4) ===\n")
df_ventura <- df_cosecha %>%
  filter(
    as.character(Campaña) == "2026",
    trimws(as.character(Módulo)) == "7",
    trimws(as.character(Turno)) == "4",
    trimws(toupper(as.character(Variedad))) == "VENTURA",
    !coalesce(Es_Testblock, FALSE),
    `Kg Total` > 0
  ) %>%
  mutate(Semana_Num = as.numeric(Semana)) %>%
  group_by(Semana_Num) %>%
  summarise(Kg = sum(`Kg Total`), Area = first(na.omit(Área)), Rend = Kg / Area, .groups = "drop")
print(as.data.frame(df_ventura))

cat("\n=== BILOXI + VENTURA JUNTOS (como hace el modal de calibración sin filtro de variedad) ===\n")
df_ambos <- df_cosecha %>%
  filter(
    as.character(Campaña) == "2026",
    trimws(as.character(Módulo)) == "7",
    trimws(as.character(Turno)) == "4",
    !coalesce(Es_Testblock, FALSE),
    `Kg Total` > 0
  ) %>%
  mutate(Semana_Num = as.numeric(Semana)) %>%
  group_by(Semana_Num) %>%
  summarise(Kg = sum(`Kg Total`), Area = first(na.omit(Área)), Rend = Kg / Area, .groups = "drop")
print(as.data.frame(df_ambos))
