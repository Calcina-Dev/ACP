suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(plotly)
})

# Let's mock two scenarios
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")

ed_a <- ediciones %>% filter(Campaña == "2026", Semana == 45, coalesce(Valor, 0) > 0)
ed_b <- ed_a %>% mutate(Valor = round(Valor * 1.08, 1)) # Scenario B is 8% higher

turnos_a <- ed_a %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Kg_Esc_A = sum(Valor, na.rm = TRUE), Ha_Esc_A = sum(Ha_Asignada, na.rm = TRUE), .groups = "drop")

turnos_b <- ed_b %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(Kg_Esc_B = sum(Valor, na.rm = TRUE), Ha_Esc_B = sum(Ha_Asignada, na.rm = TRUE), .groups = "drop")

comp_esc <- full_join(turnos_a, turnos_b, by = c("Sector", "Módulo", "Turno", "Variedad")) %>%
  mutate(
    Kg_Esc_A = coalesce(Kg_Esc_A, 0),
    Kg_Esc_B = coalesce(Kg_Esc_B, 0),
    Dif_Kg   = round(Kg_Esc_B - Kg_Esc_A, 1),
    Pct_Var  = ifelse(Kg_Esc_A > 0, round(((Kg_Esc_B - Kg_Esc_A) / Kg_Esc_A) * 100, 1), NA_real_),
    Turno_Label = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  )

cat("Comparativo entre escenarios rows:", nrow(comp_esc), "\n")
cat("Total Escenario A:", sum(comp_esc$Kg_Esc_A), "\n")
cat("Total Escenario B:", sum(comp_esc$Kg_Esc_B), "\n")
cat("Diferencia neta:", sum(comp_esc$Dif_Kg), "\n")
