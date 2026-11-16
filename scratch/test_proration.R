suppressMessages({
  library(dplyr)
})

e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
df_p <- e$df_proyecciones

p46 <- df_p %>% filter(Semana == 46)

ajustes_mod_var <- tibble(
  Módulo   = c("2", "3", "4", "5", "12", "14", "12"),
  Variedad = c("SEKOYA POP", "SEKOYA POP", "MAGICA", "MAGICA", "ROCIO", "ROCIO", "MADEIRA"),
  Kg_Delta = c(2000, 4000, 4000, 2300, 2000, 3000, 5000)
)

p46_ajustado <- p46 %>%
  left_join(ajustes_mod_var, by = c("Módulo", "Variedad")) %>%
  group_by(Módulo, Variedad) %>%
  mutate(
    Base_Mod_Var = sum(Proyección, na.rm = TRUE),
    factor_aj = if (!is.na(first(Kg_Delta)) && first(Base_Mod_Var) > 0) {
      (1 + first(Kg_Delta) / first(Base_Mod_Var))
    } else {
      1
    },
    Proy_Nueva = round(Proyección * factor_aj, 2)
  ) %>%
  ungroup()

cat("Prorated p46 sum:", sum(p46_ajustado$Proy_Nueva), "\n")

p46_mod_sum <- p46_ajustado %>%
  group_by(Módulo, Variedad) %>%
  summarise(Kg_Ajustado = sum(Proy_Nueva), .groups = "drop") %>%
  inner_join(ajustes_mod_var, by = c("Módulo", "Variedad"))

print(p46_mod_sum)
