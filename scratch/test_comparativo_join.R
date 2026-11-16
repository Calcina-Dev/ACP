suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
ediciones <- readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")

ed_45 <- ediciones %>% 
  filter(`Campaña` == "2026", Semana == 45, Campo %in% c("Viernes", "Sábado", "Lunes", "Martes", "Miércoles", "Jueves"))

cat("Ediciones por día en sem 45:\n")
print(table(ed_45$Campo))

# Now let's check df_cosecha for Semana 45
cs_45 <- df_cosecha %>%
  filter(Campaña == "2026", Semana == 45, !coalesce(Es_Testblock, FALSE)) %>%
  mutate(
    Fecha_d = as.Date(Fecha),
    Dia_num = wday(Fecha_d, week_start = 1),
    Dia_nom = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")[Dia_num],
    Sector_k = trimws(as.character(Sector)),
    Modulo_k = trimws(as.character(Módulo)),
    Turno_k  = trimws(as.character(Turno)),
    Var_k    = toupper(trimws(as.character(Variedad)))
  )

cat("SAP cosechado por día en sem 45:\n")
print(cs_45 %>% group_by(Dia_nom) %>% summarise(Kg = sum(`Kg Total`, na.rm = TRUE), .groups = "drop"))

# Turno aggregate on Friday in SAP
sap_vie <- cs_45 %>%
  filter(Dia_nom == "Viernes") %>%
  group_by(Sector_k, Modulo_k, Turno_k, Var_k) %>%
  summarise(Kg_SAP_Vie = sum(`Kg Total`, na.rm = TRUE), .groups = "drop")

cat("SAP Viernes turnos con cosecha:", nrow(sap_vie), "Total kg:", sum(sap_vie$Kg_SAP_Vie), "\n")

# Now let's join with ed_45 for Viernes
ed_vie <- ed_45 %>%
  filter(Campo == "Viernes") %>%
  mutate(
    Sector_k = trimws(as.character(Sector)),
    Modulo_k = trimws(as.character(Módulo)),
    Turno_k  = trimws(as.character(Turno)),
    Var_k    = toupper(trimws(as.character(Variedad)))
  )

comp_vie <- full_join(
  ed_vie %>% select(Sector_k, Modulo_k, Turno_k, Var_k, Ha_Ajustada = Ha_Asignada, Kg_Ajustado = Valor, Motivo_Ajuste),
  sap_vie,
  by = c("Sector_k", "Modulo_k", "Turno_k", "Var_k")
) %>%
  mutate(
    Kg_Ajustado = coalesce(Kg_Ajustado, 0),
    Kg_SAP_Vie  = coalesce(Kg_SAP_Vie, 0),
    Dif_Kg      = round(Kg_Ajustado - Kg_SAP_Vie, 1),
    Pct_Desvio  = ifelse(Kg_SAP_Vie > 0, round(((Kg_Ajustado - Kg_SAP_Vie) / Kg_SAP_Vie) * 100, 1), NA_real_)
  )

cat("Comparativo Viernes rows:", nrow(comp_vie), "\n")
print(head(comp_vie, 10))
cat("Total Kg Ajustados Viernes:", sum(comp_vie$Kg_Ajustado), "\n")
cat("Total Kg SAP Reales Viernes:", sum(comp_vie$Kg_SAP_Vie), "\n")
cat("Diferencia Neta:", sum(comp_vie$Kg_Ajustado) - sum(comp_vie$Kg_SAP_Vie), "\n")
