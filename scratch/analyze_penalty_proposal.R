source("d:/ACP/scratch/analyze_flowchart_cases.R")

cat("\n======================================================\n")
cat("DISTRIBUCION DE DIFERENCIAS % (PROY o ACT vs SEM ANT)\n")
cat("======================================================\n")

analisis_dif <- resumen %>%
  filter(Rend_S43 > 0, (Rend_Proy44 > Rend_S43 | Rend_Act44 > Rend_S43)) %>%
  mutate(
    Rend_Ref = ifelse(Pct_S44 > 30 & Rend_Act44 > 0, Rend_Act44, Rend_Proy44),
    Tipo_Ref = ifelse(Pct_S44 > 30 & Rend_Act44 > 0, "Real_Act (>30%)", "Proy (<=30% o Sin Kg)"),
    Dif_Pct_Sobre_Ant = round(100 * (Rend_Ref - Rend_S43) / Rend_S43, 1),
    Dif_Pct_Sobre_Ref = round(100 * (Rend_Ref - Rend_S43) / pmax(1, Rend_Ref), 1)
  ) %>%
  filter(Rend_Ref > Rend_S43) %>%
  arrange(desc(Dif_Pct_Sobre_Ant))

print(as.data.frame(analisis_dif %>% select(Módulo, Turno, Variedad, Tipo_Ref, Rend_S43, Rend_Ref, Dif_Pct_Sobre_Ant, Dif_Pct_Sobre_Ref) %>% head(35)))
