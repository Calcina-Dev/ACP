suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(plotly)
  library(DT)
  library(writexl)
})

# Let's verify that comparing two scenarios generates everything without errors
escenarios_file <- "D:/ACP/Actualizacion Semanal/escenarios_ajuste_semanal.rds"
df_esc <- readRDS(escenarios_file)

esc_a <- df_esc[1, ]
esc_b <- df_esc[2, ]

ed_a <- esc_a$Ediciones_Data[[1]]
ed_b <- esc_b$Ediciones_Data[[1]]

turnos_a <- ed_a %>%
  filter(coalesce(Valor, 0) > 0) %>%
  group_by(Sector = as.character(Sector), Módulo = as.character(Módulo), Turno = as.character(Turno), Variedad = toupper(trimws(as.character(Variedad)))) %>%
  summarise(
    Ha_Esc_A = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Esc_A = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    Motivo_A = first(na.omit(as.character(Motivo_Ajuste))),
    .groups  = "drop"
  )

turnos_b <- ed_b %>%
  filter(coalesce(Valor, 0) > 0) %>%
  group_by(Sector = as.character(Sector), Módulo = as.character(Módulo), Turno = as.character(Turno), Variedad = toupper(trimws(as.character(Variedad)))) %>%
  summarise(
    Ha_Esc_B = round(sum(coalesce(Ha_Asignada, 0), na.rm = TRUE), 2),
    Kg_Esc_B = round(sum(coalesce(Valor, 0), na.rm = TRUE), 1),
    Motivo_B = first(na.omit(as.character(Motivo_Ajuste))),
    .groups  = "drop"
  )

comp_2esc <- full_join(turnos_a, turnos_b, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    Ha_Esc_A   = coalesce(Ha_Esc_A, 0),
    Kg_Esc_A   = coalesce(Kg_Esc_A, 0),
    Ha_Esc_B   = coalesce(Ha_Esc_B, 0),
    Kg_Esc_B   = coalesce(Kg_Esc_B, 0),
    Dif_Kg     = round(Kg_Esc_B - Kg_Esc_A, 1),
    Pct_Var    = ifelse(Kg_Esc_A > 0, round(((Kg_Esc_B - Kg_Esc_A) / Kg_Esc_A) * 100, 1), NA_real_),
    Rend_Esc_A = ifelse(Ha_Esc_A > 0 & Kg_Esc_A > 0, round(Kg_Esc_A / Ha_Esc_A, 1), NA_real_),
    Rend_Esc_B = ifelse(Ha_Esc_B > 0 & Kg_Esc_B > 0, round(Kg_Esc_B / Ha_Esc_B, 1), NA_real_),
    Delta_Rend = ifelse(!is.na(Rend_Esc_A) & !is.na(Rend_Esc_B), round(Rend_Esc_B - Rend_Esc_A, 1), NA_real_),
    Turno_Label = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  ) %>%
  arrange(desc(abs(Dif_Kg)))

cat("Comparison between scenarios successful! Rows:", nrow(comp_2esc), "\n")
print(head(comp_2esc %>% select(Turno_Label, Kg_Esc_A, Kg_Esc_B, Dif_Kg, Pct_Var), 5))
