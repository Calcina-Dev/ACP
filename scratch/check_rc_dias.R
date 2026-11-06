suppressMessages({
  library(dplyr)
})

rc_map <- readRDS("D:/ACP/Rutas de Cosecha/rutas_cosecha_mapa.rds")
rc45 <- rc_map %>% filter(Semana == 45)

dias_cols <- c("Ha_Lunes", "Ha_Martes", "Ha_Miércoles", "Ha_Jueves", "Ha_Viernes", "Ha_Sábado")
dias_cols_ex <- intersect(dias_cols, names(rc45))

mat_ha <- as.matrix(rc45[, dias_cols_ex])
has_any <- rowSums(mat_ha > 0, na.rm = TRUE) > 0
max_idx <- max.col(mat_ha, ties.method = "first")
dias_noms <- sub("^Ha_", "", dias_cols_ex)
rc45$Dia_Ruta <- ifelse(has_any, dias_noms[max_idx], NA_character_)

cat("Summary of Dia_Ruta in rc45:\n")
print(table(rc45$Dia_Ruta, useNA = "always"))

t3_rc45 <- rc45 %>% filter(Módulo == "2", Turno == "3")
cat("\nDia_Ruta for T3 in rc45:\n")
print(t3_rc45 %>% select(codigo, Dia_Ruta, all_of(dias_cols_ex)))
