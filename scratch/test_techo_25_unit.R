suppressPackageStartupMessages({
  library(dplyr)
})

# Test cases for the 25% cap rule
prop_umbral_cfg <- 0.30

test_df <- tibble(
  Turno = c("T1_Excede_62pct", "T2_Dentro_18pct", "T3_Castigo", "T4_Sin_S1", "T5_Con_Avance_Real"),
  Semana_Ant_No_Finalizo = c(FALSE, FALSE, FALSE, FALSE, FALSE),
  Tiene_Cosecha_Fresca = c(FALSE, FALSE, FALSE, FALSE, FALSE),
  Prop_Fresca_Turno = c(0, 0, 0, 0, 0),
  Tiene_Cosecha_Act = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  Prop_Avance_Turno = c(0, 0, 0, 0, 0.45),
  Rend_Sem_Actual = c(0, 0, 0, 0, 750),
  Rend_Ant_Turno = c(500, 450, 300, 0, 400),
  Rend_Proy = c(400, 400, 400, 400, 400),
  Factor_Floral = c(1.30, 1.05, 0.85, 1.10, 1.20),
  Tipo_Efecto_Floral = c("Premio", "Premio", "Castigo", "Premio", "Premio"),
  Esta_Proyectado = c(TRUE, TRUE, TRUE, TRUE, TRUE),
  Turno_Completo_Lun_Mar = c(FALSE, FALSE, FALSE, FALSE, FALSE),
  Es_Ciclo_Corto = c(FALSE, FALSE, FALSE, FALSE, FALSE)
)

test_calc <- test_df %>%
  mutate(
    Rend_Base_Turno = case_when(
      Turno_Completo_Lun_Mar & Es_Ciclo_Corto ~ Rend_Sem_Actual,
      Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ Rend_Sem_Actual,
      Semana_Ant_No_Finalizo ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      !Tiene_Cosecha_Act ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ Rend_Sem_Actual,
      Tiene_Cosecha_Act & Prop_Avance_Turno <= prop_umbral_cfg ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy),
      TRUE ~ ifelse(Rend_Ant_Turno > 0, Rend_Ant_Turno, Rend_Proy)
    ),
    
    Usa_Base_S1     = (Rend_Base_Turno == Rend_Ant_Turno) & (Rend_Ant_Turno > 0),
    Rend_S1_Floral  = round(Rend_Ant_Turno * Factor_Floral, 2),
    Excede_25_Proy  = Usa_Base_S1 & Esta_Proyectado & (Rend_Proy > 0) & (Rend_S1_Floral > 1.25 * Rend_Proy),
    Pct_Exceso_Proy = round(((Rend_S1_Floral / pmax(1, Rend_Proy)) - 1) * 100, 1),
    
    Rend_Base_Con_Floral = case_when(
      Semana_Ant_No_Finalizo & Tiene_Cosecha_Fresca & Prop_Fresca_Turno > prop_umbral_cfg ~ round(Rend_Base_Turno, 2),
      Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg                              ~ round(Rend_Base_Turno, 2),
      Excede_25_Proy                                                                      ~ round(Rend_Proy, 2),
      TRUE                                                                                ~ round(Rend_Base_Turno * Factor_Floral, 2)
    ),
    
    Rend_Turno_Aplicar = ifelse(!Esta_Proyectado | (Turno_Completo_Lun_Mar & !Es_Ciclo_Corto), 0, Rend_Base_Con_Floral),
    
    Motivo_Base = case_when(
      !Esta_Proyectado ~ "No proyectado",
      Turno_Completo_Lun_Mar & !Es_Ciclo_Corto ~ "Cerró 100% en Lun/Mar (0 kg pend.)",
      Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ sprintf("Avance >%d%% (Rend act sem: %s kg/ha)", round(prop_umbral_cfg*100), trimws(format(round(Rend_Sem_Actual), big.mark = ","))),
      !Tiene_Cosecha_Act & Excede_25_Proy     ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
      !Tiene_Cosecha_Act & Rend_Ant_Turno > 0 ~ sprintf("Sin avance previo (Rend S-1: %s kg/ha)", trimws(format(round(Rend_Ant_Turno), big.mark = ","))),
      !Tiene_Cosecha_Act                      ~ sprintf("Sin avance previo (Rend proy: %s kg/ha)", trimws(format(round(Rend_Proy), big.mark = ","))),
      TRUE ~ "Otro"
    ),
    
    Pct_Ajuste_Flor = round((Factor_Floral - 1) * 100, 1),
    Detalle_Floral = case_when(
      Tiene_Cosecha_Act & Prop_Avance_Turno > prop_umbral_cfg ~ "",
      Excede_25_Proy                  ~ sprintf("S-1 + floral supera >25%% proy (+%.1f%%) -> Usa proy", Pct_Exceso_Proy),
      Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", Pct_Ajuste_Flor),
      Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", Pct_Ajuste_Flor),
      TRUE                            ~ "Floral neutro (0%)"
    )
  )

cat("--- RESULTADOS TEST DE LA REGLA DEL 25% ---\n")
for (i in seq_len(nrow(test_calc))) {
  cat(sprintf("\nCaso: %s\n", test_calc$Turno[i]))
  cat(sprintf("  Rend S-1: %.1f | Factor Floral: %.2f | S-1 + Floral: %.1f | Proy: %.1f (Tope +25%%: %.1f)\n",
              test_calc$Rend_Ant_Turno[i], test_calc$Factor_Floral[i], test_calc$Rend_S1_Floral[i],
              test_calc$Rend_Proy[i], 1.25 * test_calc$Rend_Proy[i]))
  cat(sprintf("  Excede > 25%% Proy?: %s (Exceso: +%.1f%%)\n", test_calc$Excede_25_Proy[i], test_calc$Pct_Exceso_Proy[i]))
  cat(sprintf("  Rendimiento Final Aplicado: %.2f kg/ha\n", test_calc$Rend_Turno_Aplicar[i]))
  cat(sprintf("  Motivo Base: %s\n", test_calc$Motivo_Base[i]))
  cat(sprintf("  Detalle Floral: %s\n", test_calc$Detalle_Floral[i]))
}
