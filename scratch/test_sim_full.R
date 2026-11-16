load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
library(dplyr)

limpiar_variedad <- function(x) toupper(trimws(as.character(x)))

desplazar_dia_2da_pasada <- function(dia_base_vec, frec_vec, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx = 6L, dia_fin_nom = "Sábado") {
  idx_base <- match(dia_base_vec, dias_nombres)
  frec_int <- as.integer(frec_vec)
  desp <- ifelse(idx_base == 1L, ifelse(!is.na(frec_int) & frec_int <= 3L, 3L, 4L), 3L)
  idx_target <- pmin(dia_fin_idx, pmax(dia_inicio_idx, idx_base + desp))
  dia_target <- dias_nombres[idx_target]
  dia_res <- ifelse(is.na(dia_target) | idx_target < dia_inicio_idx, dia_inicio_nom, dia_target)
  idx_res <- match(dia_res, dias_nombres)
  ifelse(!is.na(idx_res) & idx_res > dia_fin_idx, dia_fin_nom, dia_res)
}

dias_nombres <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
dia_inicio_idx <- 2L
dia_inicio_nom <- "Martes"
dia_fin_idx <- 5L
dia_fin_nom <- "Viernes"
dias_ajustables_nom <- dias_nombres[dia_inicio_idx:dia_fin_idx]

esc <- readRDS("Actualizacion Semanal/escenarios_ajuste_semanal.rds")
e4 <- esc[4, ]
vdata <- e4$Valvulas_Data[[1]]

reg_regular_e4 <- vdata %>% filter(Num_Pasada == 1)
reg_2da_lun_e4 <- vdata %>% filter(Num_Pasada == 2)

# Fix reg_2da_lun Dia_Asignado
reg_2da_lun_fixed <- reg_2da_lun_e4 %>%
  mutate(
    Dia_Asignado = as.character(desplazar_dia_2da_pasada("Lunes", 6, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom))
  )

# Now simulate reg_2da_mar with both real Tuesday + reg_regular Tuesday
valvs_mar_reg <- reg_regular_e4 %>% 
  filter(Dia_Asignado == "Martes", Ha_Asignada > 0) %>%
  select(id, codigo, Sector, Módulo, Turno, Variedad, Ha_Mar_Reg = Ha_Asignada, Rend_Mar_Reg = Kg_Asignado / Ha_Asignada)

# Already assigned 2da pasada from Monday
ha_ya_2da_df <- reg_2da_lun_fixed %>%
  group_by(Módulo, Turno, Variedad) %>%
  summarise(ha_ya_2da = sum(Ha_Asignada, na.rm = TRUE), .groups = "drop")

# Area of Turno 1 and 5
area_t1 <- 5.20
area_t5 <- 3.97

# For Turno 1:
t1_mar <- valvs_mar_reg %>% filter(as.character(Módulo) == "12", as.character(Turno) == "1")
pct_mar <- 0.60
t1_2da <- t1_mar %>%
  mutate(
    Ha_2da_Corta = Ha_Mar_Reg,
    Dia_Asignado = as.character(desplazar_dia_2da_pasada("Martes", 6, dias_nombres, dia_inicio_idx, dia_inicio_nom, dia_fin_idx, dia_fin_nom)),
    Rend_2da_Aplicar = round(Rend_Mar_Reg * pct_mar, 2),
    Kg_Asignado = round(Ha_2da_Corta * Rend_2da_Aplicar, 2),
    Ha_Asignada = Ha_2da_Corta,
    Num_Pasada = 2L,
    Motivo_Ajuste = sprintf("2da pasada Rocío (Rend Martes aj: %s kg/ha • %d%% sin floración)", format(round(Rend_Mar_Reg), big.mark = ","), round(pct_mar * 100))
  )

cat("Turno 1 summary:\n")
cat("1ra pasada (Martes): Ha =", sum(t1_mar$Ha_Mar_Reg), "Kg =", sum(t1_mar$Ha_Mar_Reg * t1_mar$Rend_Mar_Reg), "\n")
cat("2da pasada (Viernes): Ha =", sum(t1_2da$Ha_Asignada), "Kg =", sum(t1_2da$Kg_Asignado), "\n")

# For Turno 5:
t5_mar <- valvs_mar_reg %>% filter(as.character(Módulo) == "12", as.character(Turno) == "5")
t5_ya_2da <- ha_ya_2da_df %>% filter(as.character(Módulo) == "12", as.character(Turno) == "5") %>% pull(ha_ya_2da)
t5_cupo_2da <- area_t5 - coalesce(t5_ya_2da, 0)
cat("\nTurno 5 ha_ya_2da from Monday:", t5_ya_2da, "remaining cupo for 2da pasada:", t5_cupo_2da, "\n")
