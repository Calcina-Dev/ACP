suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# Mock row
fila <- list(
  Sector = "9", Módulo = "8", Turno = "1", Variedad = "MAGICA", Tipo = "Convencional",
  Área = 6.45, `Ha Aj. 1ª Pas` = 6.45, `Ha Aj. 2ª Pas` = 0,
  `Kg/Ha Proy` = 859.70, `Kg/Ha Aj. 1ª Pas` = 930.95, `Kg/Ha Aj. 2ª Pas` = NA_real_,
  Lunes = NA_real_, Martes = NA_real_, Miércoles = NA_real_,
  Jueves = 3006.97, Viernes = 2997.66, Sábado = NA_real_,
  Lunes_tipo = "none", Martes_tipo = "none", Miércoles_tipo = "none",
  Jueves_tipo = "auto", Viernes_tipo = "auto", Sábado_tipo = "none",
  Lunes_bloqueado = FALSE, Martes_bloqueado = FALSE, Miércoles_bloqueado = FALSE,
  Jueves_bloqueado = FALSE, Viernes_bloqueado = FALSE, Sábado_bloqueado = FALSE
)

ed_actuales <- tibble(
  Campaña = "2026", Semana = 45, Sector = "9", Módulo = "8", Turno = "1", Variedad = "MAGICA",
  Campo = c("Jueves", "Viernes"), Valor = c(3006.97, 2997.66),
  Ha_Asignada = c(3.23, 3.22), Ha_Asignada_1ra = c(3.23, 3.22), Kg_Asignado_1ra = c(3006.97, 2997.66),
  Tipo_Edicion = "auto"
)

dias_validos <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
pasada_sel <- 1L
ha_aj_pasada <- 6.45

lista_dias <- list()
for (d in dias_validos) {
  val_d <- coalesce(as.numeric(fila[[d]]), 0)
  tipo_d <- coalesce(as.character(fila[[paste0(d, "_tipo")]]), "none")
  bloq_d <- isTRUE(fila[[paste0(d, "_bloqueado")]])
  if (bloq_d) next
  
  ed_d <- ed_actuales %>% filter(Campo == d)
  if (nrow(ed_d) > 0) {
    lista_dias[[length(lista_dias) + 1]] <- tibble(
      dia = d,
      kg_actual = ed_d$Valor[1],
      ha_actual = coalesce(ed_d$Ha_Asignada_1ra[1], ed_d$Ha_Asignada[1], 0),
      tipo = ed_d$Tipo_Edicion[1]
    )
  } else if (val_d > 0 && tipo_d %in% c("auto", "manual")) {
    lista_dias[[length(lista_dias) + 1]] <- tibble(
      dia = d, kg_actual = val_d, ha_actual = 0, tipo = tipo_d
    )
  }
}

dias_df <- bind_rows(lista_dias)
print(dias_df)

nuevo_rend <- 859.69
kg_tot_nuevo <- round(ha_aj_pasada * nuevo_rend, 2)
cat("kg_tot_nuevo:", kg_tot_nuevo, "\n")

sum_kg_act <- sum(dias_df$kg_actual, na.rm = TRUE)
props <- dias_df$kg_actual / sum_kg_act
dias_df$kg_nuevo <- round(kg_tot_nuevo * props, 2)

dif_cent <- round(kg_tot_nuevo - sum(dias_df$kg_nuevo), 2)
if (dif_cent != 0) {
  dias_df$kg_nuevo[which.max(dias_df$kg_nuevo)] <- round(dias_df$kg_nuevo[which.max(dias_df$kg_nuevo)] + dif_cent, 2)
}

print(dias_df)
cat("Sum kg_nuevo:", sum(dias_df$kg_nuevo), "Expected:", kg_tot_nuevo, "\n")
stopifnot(sum(dias_df$kg_nuevo) == kg_tot_nuevo)

# New yield on each day:
cat("Jueves rend:", round(dias_df$kg_nuevo[1] / dias_df$ha_actual[1], 2), "\n")
cat("Viernes rend:", round(dias_df$kg_nuevo[2] / dias_df$ha_actual[2], 2), "\n")
cat("Total rend:", round(sum(dias_df$kg_nuevo) / sum(dias_df$ha_actual), 2), "\n")
stopifnot(round(sum(dias_df$kg_nuevo) / sum(dias_df$ha_actual), 2) == 859.69)
cat("TEST PASSED!\n")
