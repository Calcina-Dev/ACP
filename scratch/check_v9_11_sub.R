suppressPackageStartupMessages(library(dplyr))
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

cat("=== CATASTRO: Válvulas de M9 y M11 ===\n")
v9_11 <- df_valvulas %>%
  filter(as.character(Módulo) %in% c("9", "11", "9.1", "9.2", "11.1", "11.2"))
print(table(as.character(v9_11$Sector), as.character(v9_11$Módulo)))

cat("\n=== CATASTRO si aplicamos la subdivisión de turnos ===\n")
v9_11_sub <- v9_11 %>%
  mutate(
    Turno_num = suppressWarnings(as.numeric(Turno)),
    Módulo_Sub = case_when(
      as.character(Módulo) == "9" & between(Turno_num, 1, 11)  ~ "9.2",
      as.character(Módulo) == "9" & Turno_num >= 12            ~ "9.1",
      as.character(Módulo) == "11" & between(Turno_num, 1, 12) ~ "11.1",
      as.character(Módulo) == "11" & Turno_num >= 13           ~ "11.2",
      TRUE ~ as.character(Módulo)
    )
  )
print(table(as.character(v9_11_sub$Sector), as.character(v9_11_sub$Módulo_Sub)))
