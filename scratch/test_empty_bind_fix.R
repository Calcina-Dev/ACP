suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# 1. Test when df_valvs_ajuste_nueva has 0 rows
valvs_filtradas <- tibble(
  Campaña         = "2026",
  Semana          = 45,
  id              = "1",
  codigo          = "M01-T01-V01",
  Sector          = "1",
  Módulo          = "1",
  Turno           = "1",
  Variedad        = "ROCIO",
  Dia_Asignado    = "Jueves",
  Kg_Asignado     = 100,
  Ha_Asignada     = 1.5,
  Num_Pasada      = 1L,
  Tipo_Asignacion = "regular",
  Dia_Desde       = "Jueves",
  Dia_Hasta       = "Sábado",
  Motivo_Ajuste   = "Test",
  Fecha_Ajuste    = Sys.time()
)

# Simulate 0-row df_valvs_ajuste_nueva
registros_todos_0 <- tibble(
  id = character(), codigo = character(), Sector = character(), Módulo = character(), Turno = character(),
  Variedad = character(), Dia_Asignado = character(), Kg_Asignado = numeric(), Ha_Asignada = numeric(),
  Num_Pasada = integer(), Es_Ruta_Sin_Kg = logical(), Motivo_Ajuste = character()
)

df_valvs_ajuste_nueva_0 <- registros_todos_0 %>%
  filter(Kg_Asignado > 0) %>%
  mutate(
    Campaña         = as.character("2026"),
    Semana          = as.numeric(45),
    Dia_Desde       = as.character("Jueves"),
    Dia_Hasta       = as.character("Sábado"),
    Tipo_Asignacion = as.character(case_when(
      coalesce(Es_Ruta_Sin_Kg, FALSE) ~ "ruta_sin_kg",
      coalesce(Num_Pasada, 1L) == 2L  ~ "2da_pasada",
      TRUE                            ~ "regular"
    )),
    Motivo_Ajuste   = as.character(Motivo_Ajuste),
    Fecha_Ajuste    = Sys.time()
  ) %>%
  select(Campaña, Semana, id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Tipo_Asignacion, Dia_Desde, Dia_Hasta, Motivo_Ajuste, Fecha_Ajuste)

cat("df_valvs_ajuste_nueva_0 class of Tipo_Asignacion:", class(df_valvs_ajuste_nueva_0$Tipo_Asignacion), "\n")

valvs_guardar_0 <- if (nrow(df_valvs_ajuste_nueva_0) > 0 && nrow(valvs_filtradas) > 0) {
  bind_rows(valvs_filtradas, df_valvs_ajuste_nueva_0)
} else if (nrow(df_valvs_ajuste_nueva_0) > 0) {
  df_valvs_ajuste_nueva_0
} else {
  valvs_filtradas
}

cat("valvs_guardar_0 nrow:", nrow(valvs_guardar_0), "\n")
stopifnot(nrow(valvs_guardar_0) == 1)

# 2. Test when df_valvs_ajuste_nueva has rows
registros_todos_1 <- tibble(
  id = "2", codigo = "M01-T01-V02", Sector = "1", Módulo = "1", Turno = "1",
  Variedad = "ROCIO", Dia_Asignado = "Viernes", Kg_Asignado = 150, Ha_Asignada = 2,
  Num_Pasada = 1L, Es_Ruta_Sin_Kg = FALSE, Motivo_Ajuste = "Test 2"
)

df_valvs_ajuste_nueva_1 <- registros_todos_1 %>%
  filter(Kg_Asignado > 0) %>%
  mutate(
    Campaña         = as.character("2026"),
    Semana          = as.numeric(45),
    Dia_Desde       = as.character("Jueves"),
    Dia_Hasta       = as.character("Sábado"),
    Tipo_Asignacion = as.character(case_when(
      coalesce(Es_Ruta_Sin_Kg, FALSE) ~ "ruta_sin_kg",
      coalesce(Num_Pasada, 1L) == 2L  ~ "2da_pasada",
      TRUE                            ~ "regular"
    )),
    Motivo_Ajuste   = as.character(Motivo_Ajuste),
    Fecha_Ajuste    = Sys.time()
  ) %>%
  select(Campaña, Semana, id, codigo, Sector, Módulo, Turno, Variedad, Dia_Asignado, Kg_Asignado, Ha_Asignada, Num_Pasada, Tipo_Asignacion, Dia_Desde, Dia_Hasta, Motivo_Ajuste, Fecha_Ajuste)

valvs_guardar_1 <- if (nrow(df_valvs_ajuste_nueva_1) > 0 && nrow(valvs_filtradas) > 0) {
  bind_rows(valvs_filtradas, df_valvs_ajuste_nueva_1)
} else if (nrow(df_valvs_ajuste_nueva_1) > 0) {
  df_valvs_ajuste_nueva_1
} else {
  valvs_filtradas
}

cat("valvs_guardar_1 nrow:", nrow(valvs_guardar_1), "\n")
stopifnot(nrow(valvs_guardar_1) == 2)

cat("=== ALL EDGE CASE TESTS PASSED PERFECTLY! ===\n")
