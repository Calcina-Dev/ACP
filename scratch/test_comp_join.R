suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ed <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")

# Helper
norm_codigo_valvula <- function(x) {
  sub("^M0*([0-9]+)[-_]T0*([0-9]+)[-_]V0*([0-9]+)$", "M\\1-T\\2-V\\3", toupper(trimws(as.character(x))))
}

sem_num <- 45
ed_sem <- ed %>%
  filter(as.numeric(Semana) == sem_num) %>%
  mutate(
    Sector = as.character(Sector),
    Módulo = as.character(Módulo),
    Turno = as.character(Turno),
    Variedad = toupper(trimws(as.character(Variedad)))
  )

# Real harvest by Turno and Day from df_hist_oficial_wide
sap_sem <- df_hist_oficial_wide %>%
  filter(as.numeric(Semana) == sem_num) %>%
  mutate(
    Sector = as.character(Sector),
    Módulo = as.character(Módulo_Norm),
    Turno = as.character(Turno_Norm),
    Variedad = toupper(trimws(as.character(Variedad)))
  ) %>%
  group_by(Sector, Módulo, Turno, Variedad) %>%
  summarise(
    Real_Lunes     = sum(Kg_Lunes, na.rm = TRUE),
    Real_Martes    = sum(Kg_Martes, na.rm = TRUE),
    Real_Miércoles = sum(Kg_Miércoles, na.rm = TRUE),
    Real_Jueves    = sum(Kg_Jueves, na.rm = TRUE),
    Real_Viernes   = sum(Kg_Viernes, na.rm = TRUE),
    Real_Sábado    = sum(Kg_Sábado, na.rm = TRUE),
    .groups        = "drop"
  )

# Reshape real to long by day
sap_long <- sap_sem %>%
  pivot_longer(cols = starts_with("Real_"), names_to = "Campo", values_to = "Kg_Real_SAP") %>%
  mutate(Campo = sub("^Real_", "", Campo))

# Join adjustment with real
comp <- ed_sem %>%
  left_join(sap_long, by = c("Sector", "Módulo", "Turno", "Variedad", "Campo")) %>%
  mutate(
    Kg_Real_SAP = coalesce(Kg_Real_SAP, 0),
    Kg_Ajustado = coalesce(Valor, 0),
    Dif_Kg      = round(Kg_Real_SAP - Kg_Ajustado, 2),
    Desvio_Pct  = ifelse(Kg_Ajustado > 0, round((Dif_Kg / Kg_Ajustado) * 100, 1), 0),
    Turno_Label = sprintf("M%s-T%s (%s)", Módulo, Turno, Variedad)
  )

cat("Comparison rows for Sem 45:", nrow(comp), "\n")
cat("Friday comparison:\n")
vie_comp <- comp %>% filter(Campo == "Viernes")
cat("Friday turnos adjusted:", nrow(vie_comp), "\n")
cat("Total Kg Ajustados (Vie):", sum(vie_comp$Kg_Ajustado), "\n")
cat("Total Kg Real SAP (Vie):", sum(vie_comp$Kg_Real_SAP), "\n")
cat("Total Dif Kg (Vie):", sum(vie_comp$Dif_Kg), "\n")

print(head(vie_comp %>% select(Turno_Label, Campo, Kg_Ajustado, Kg_Real_SAP, Dif_Kg, Desvio_Pct)))
