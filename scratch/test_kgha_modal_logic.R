suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Cargar datos
ed <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")

# Filtrar S8 M4 T5
ed_t <- ed %>% filter(Sector == "8", Módulo == "4", Turno == "5", Semana == 45)
print(ed_t)

fila_simulada <- list(
  Sector = "8", Módulo = "4", Turno = "5", Variedad = "MAGICA", Tipo = "Convencional",
  `Ha Aj. 1ª Pas` = 6.45, `Ha Aj. 2ª Pas` = 0,
  `Kg/Ha Aj. 1ª Pas` = 930.95, `Kg/Ha Aj. 2ª Pas` = NA,
  `Kg/Ha Proy` = 859.70,
  Kg_Aj_1ra = 6004.63, Kg_Aj_2da = 0,
  Lunes = 0, Martes = 0, Miércoles = 0, Jueves = 3006.97, Viernes = 2997.66, Sábado = 0,
  Lunes_tipo = "none", Martes_tipo = "none", Miércoles_tipo = "none", 
  Jueves_tipo = "auto", Viernes_tipo = "auto", Sábado_tipo = "none",
  Jueves_bloqueado = FALSE, Viernes_bloqueado = FALSE
)

dias_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dias_ajustados <- dias_semana[sapply(dias_semana, function(d) {
  val <- coalesce(as.numeric(fila_simulada[[d]]), 0)
  tip <- coalesce(as.character(fila_simulada[[paste0(d, "_tipo")]]), "none")
  bloq <- isTRUE(fila_simulada[[paste0(d, "_bloqueado")]])
  val > 0 && tip %in% c("auto", "manual") && !bloq
})]
cat("Días ajustados detectados:", paste(dias_ajustados, collapse=", "), "\n")

# Probar nuevo rendimiento 859.69
nuevo_kgha <- 859.69
ha_pasada <- 6.45
kg_total_nuevo <- round(ha_pasada * nuevo_kgha, 2)
cat(sprintf("Kg Total Nuevo: %.2f kg (de %.2f)\n", kg_total_nuevo, fila_simulada$Kg_Aj_1ra))

kg_orig_dias <- sapply(dias_ajustados, function(d) as.numeric(fila_simulada[[d]]))
tot_orig_kg <- sum(kg_orig_dias)

# Hectáreas originales por día
ha_orig_dias <- c()
for (d in dias_ajustados) {
  ed_d <- ed_t %>% filter(Campo == d)
  if (nrow(ed_d) > 0 && coalesce(ed_d$Ha_Asignada[1], 0) > 0) {
    ha_orig_dias[d] <- ed_d$Ha_Asignada[1]
  } else {
    ha_orig_dias[d] <- round(ha_pasada * (kg_orig_dias[d] / tot_orig_kg), 2)
  }
}
cat("Ha orig dias:\n")
print(ha_orig_dias)

# Proporción
props <- kg_orig_dias / tot_orig_kg
kg_nuevos <- c()
kg_acum <- 0
for (i in seq_along(dias_ajustados)) {
  d <- dias_ajustados[i]
  if (i == length(dias_ajustados)) {
    kg_nuevos[d] <- max(0, round(kg_total_nuevo - kg_acum, 2))
  } else {
    kg_nuevos[d] <- round(kg_total_nuevo * props[i], 2)
    kg_acum <- kg_acum + kg_nuevos[d]
  }
}
cat("Kg nuevos dias:\n")
print(kg_nuevos)
cat(sprintf("Suma kg nuevos: %.2f (esperado: %.2f)\n", sum(kg_nuevos), kg_total_nuevo))
cat("Rendimiento resultante por dia:\n")
for (d in dias_ajustados) {
  cat(sprintf("  %s: %.2f kg / %.2f ha = %.2f kg/ha\n", d, kg_nuevos[d], ha_orig_dias[d], kg_nuevos[d] / ha_orig_dias[d]))
}
