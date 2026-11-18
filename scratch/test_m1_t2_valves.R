source("Actualizacion Semanal/app.R", local = TRUE)

# Let's inspect df_hist_oficial_wide for M1-T2 in S45 and S46
dias_cols <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
df_w <- df_hist_oficial_wide %>%
  filter(trimws(as.character(Módulo_Norm)) == "1", trimws(as.character(Turno_Norm)) == "2") %>%
  filter(as.numeric(Semana) %in% c(45, 46))

cat("Valves for M1-T2 in S45 and S46:\n")
print(df_w %>% select(Semana, Codigo_Valv, Variedad, any_of(paste0("Kg_", dias_cols))))
