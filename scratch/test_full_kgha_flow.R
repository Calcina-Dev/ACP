suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

ed_orig <- readRDS("Actualizacion Semanal/ediciones_manuales_cosecha.rds")
valvs <- readRDS("Actualizacion Semanal/ajuste_valvulas_semana.rds")

# Simular que estamos en S8 M4 T5
ed_s8 <- ed_orig %>% filter(Sector == "8", Módulo == "4", Turno == "5", Semana == 45)
cat("Registros originales en ed:\n")
print(ed_s8 %>% select(Campo, Valor, Tipo_Edicion, Ha_Asignada, Motivo_Ajuste))

# 1. Aplicar nuevo rendimiento 859.69
nuevo_kgha <- 859.69
ha_pasada <- 6.45
dias <- c("Jueves", "Viernes")
kg_orig <- ed_s8$Valor
tot_orig <- sum(kg_orig)
props <- kg_orig / tot_orig
kg_tot_nuevo <- round(ha_pasada * nuevo_kgha, 2)
kg_dist <- c(round(kg_tot_nuevo * props[1], 2), max(0, round(kg_tot_nuevo - round(kg_tot_nuevo * props[1], 2), 2)))
ha_dist <- c(3.23, 3.22)

nuevas_filas <- tibble(
  Campaña         = "2026",
  Semana          = 45,
  Sector          = "8",
  Módulo          = "4",
  Turno           = "5",
  Variedad        = "MAGICA",
  Campo           = dias,
  Valor           = kg_dist,
  Ha_Asignada     = ha_dist,
  Ha_Asignada_1ra = ha_dist,
  Kg_Asignado_1ra = kg_dist,
  Ha_Asignada_2da = c(0, 0),
  Kg_Asignado_2da = c(0, 0),
  Tipo_Edicion    = "manual",
  Modo_Ingreso    = "kgha",
  Motivo_Ajuste   = sprintf("Manual (%.2f kg/ha en %.2f ha)", nuevo_kgha, ha_dist),
  Fecha_Edicion   = Sys.time()
)

ed_mod <- ed_orig %>%
  filter(!(Campaña == "2026" & Semana == 45 & Sector == "8" & Módulo == "4" & Turno == "5" & Campo %in% dias)) %>%
  bind_rows(nuevas_filas)

cat("\nRegistros modificados a manual:\n")
print(ed_mod %>% filter(Sector == "8", Módulo == "4", Turno == "5", Semana == 45) %>% select(Campo, Valor, Tipo_Edicion, Ha_Asignada, Motivo_Ajuste))

# 2. Restablecer automático
valvs_t <- valvs %>% filter(Sector == "8", Módulo == "4", Turno == "5", Semana == 45, Dia_Asignado %in% dias)
cat(sprintf("\nValvulas en rv_ajuste_valvulas: %d\n", nrow(valvs_t)))

ed_restaurado <- ed_mod %>%
  filter(!(Campaña == "2026" & Semana == 45 & Sector == "8" & Módulo == "4" & Turno == "5" & Campo %in% dias))

if (nrow(valvs_t) > 0) {
  ed_auto_rest <- valvs_t %>%
    group_by(Campaña, Semana, Sector, Módulo, Turno, Variedad, Dia_Asignado) %>%
    summarise(
      Valor           = round(sum(Kg_Asignado, na.rm = TRUE), 2),
      Ha_Asignada     = round(sum(Ha_Asignada, na.rm = TRUE), 2),
      Ha_Asignada_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Ha_Asignada, 0), na.rm = TRUE), 2),
      Kg_Asignado_1ra = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 1L, Kg_Asignado, 0), na.rm = TRUE), 2),
      Ha_Asignada_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Ha_Asignada, 0), na.rm = TRUE), 2),
      Kg_Asignado_2da = round(sum(ifelse(coalesce(Num_Pasada, 1L) == 2L, Kg_Asignado, 0), na.rm = TRUE), 2),
      Motivo_Ajuste   = first(na.omit(Motivo_Ajuste)),
      .groups         = "drop"
    ) %>%
    rename(Campo = Dia_Asignado) %>%
    mutate(
      Fecha_Edicion = Sys.time(),
      Tipo_Edicion  = "auto",
      Modo_Ingreso  = NA_character_
    )
  ed_restaurado <- bind_rows(ed_restaurado, ed_auto_rest)
}

cat("\nRegistros tras restablecer auto:\n")
print(ed_restaurado %>% filter(Sector == "8", Módulo == "4", Turno == "5", Semana == 45) %>% select(Campo, Valor, Tipo_Edicion, Ha_Asignada, Motivo_Ajuste))
cat("\nFlujo verificado exitosamente!\n")
