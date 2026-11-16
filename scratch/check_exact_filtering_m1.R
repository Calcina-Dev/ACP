library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

sem_sel  <- 45
sem_ant  <- 44
camp_sel <- "2026"
dia_inicio_nom <- "Martes"
dia_fin_nom    <- "Viernes"
dias_nombres   <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- which(dias_nombres == dia_inicio_nom)[1]
dia_fin_idx    <- which(dias_nombres == dia_fin_nom)[1]
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

limpiar_variedad <- function(x) trimws(toupper(as.character(x)))
norm_codigo_valvula <- function(x) trimws(toupper(as.character(x)))

# Let's inspect df_hist_oficial_wide
if (exists("df_hist_oficial_wide")) {
  h44 <- df_hist_oficial_wide %>% filter(Semana == 44, as.character(Módulo_Norm) == "1")
  cat("df_hist_oficial_wide M1 rows:", nrow(h44), "\n")
  if (nrow(h44) > 0) print(head(h44 %>% select(Codigo_Valv, Turno_Norm, starts_with("Kg_"))))
}
