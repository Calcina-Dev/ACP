suppressMessages({
  library(dplyr)
})

# Create a tibble with 4 rows in group Módulo=5, Turno=10 (the exact group that failed)
df_test <- tibble(
  Módulo = rep("5", 4),
  Turno = rep("10", 4),
  Variedad = rep("MAGICA", 4),
  Semana_Ant_No_Finalizo = rep(TRUE, 4),
  Valvula_Cerro_En_Lun_Mar = c(TRUE, FALSE, TRUE, FALSE),
  Rend_Ant_Turno = rep(800.5, 4),
  Rend_Proy = rep(700.0, 4),
  Factor_Floral = rep(0.95, 4),
  Tipo_Efecto_Floral = rep("Castigo", 4),
  Motivo_Ajuste = rep("Avance >30% (Rend act sem: 850 kg/ha)", 4)
)

res <- df_test %>%
  group_by(Módulo, Turno, Variedad) %>%
  mutate(
    Motivo_Ajuste = case_when(
      Semana_Ant_No_Finalizo & Valvula_Cerro_En_Lun_Mar ~ paste0(
        ifelse(
          coalesce(Rend_Ant_Turno, 0) > 0,
          sprintf("Cierre S-1 incompleta (Rend S-1: %s kg/ha)", trimws(format(round(coalesce(Rend_Ant_Turno, 0)), big.mark = ","))),
          sprintf("Cierre S-1 incompleta (Rend proy: %s kg/ha)", trimws(format(round(coalesce(Rend_Proy, 0)), big.mark = ",")))
        ),
        " • ",
        case_when(
          Tipo_Efecto_Floral == "Premio"  ~ sprintf("Premio floral %+0.1f%%", round((Factor_Floral - 1) * 100, 1)),
          Tipo_Efecto_Floral == "Castigo" ~ sprintf("Castigo floral %+0.1f%%", round((Factor_Floral - 1) * 100, 1)),
          TRUE                            ~ "Floral neutro (0%)"
        )
      ),
      TRUE ~ Motivo_Ajuste
    )
  )

cat("Success! Results:\n")
print(res %>% select(Módulo, Turno, Valvula_Cerro_En_Lun_Mar, Motivo_Ajuste))
