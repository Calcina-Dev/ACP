source("Actualizacion Semanal/app.R", local = TRUE)

df_w <- df_hist_oficial_wide %>%
  filter(as.character(Módulo_Norm) == "10", as.character(Turno_Norm) == "18", as.numeric(Semana) == 46)

cat("M10-T18 in df_hist_oficial_wide (S46):\n")
print(df_w %>% select(Codigo_Valv, Area_Ha, any_of(paste0("Kg_", c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")))))

cat("\nHa columns:\n")
print(df_w %>% select(Codigo_Valv, Area_Ha, any_of(paste0("Ha_", c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")))))
