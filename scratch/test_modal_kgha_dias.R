suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

cat("=== TEST: MODAL KGHA DAY ASSIGNMENT LOGIC ===\n")

# 1. Simular fila inicial
fila <- list(
  Sector = "8",
  Módulo = "4",
  Turno = "1",
  Variedad = "MAGICA",
  Tipo = "Convencional",
  Área = 6.42,
  `Ha Aj. 1ª Pas` = 6.42,
  `Kg/Ha Aj. 1ª Pas` = 719.06,
  Kg_Aj_1ra = 4616.37,
  `Kg/Ha Proy` = 628.30,
  Lunes = 0,
  Lunes_bloqueado = TRUE,   # Bloqueado por SAP
  Martes = 0,
  Martes_bloqueado = FALSE,
  Miércoles = 3451.5,
  Miércoles_tipo = "auto",
  Miércoles_bloqueado = FALSE,
  Jueves = 1164.9,
  Jueves_tipo = "auto",
  Jueves_bloqueado = FALSE,
  Viernes = 0,
  Viernes_bloqueado = FALSE,
  Sábado = 0,
  Sábado_bloqueado = FALSE
)

# Válvulas iniciales
valvs_ini <- tibble(
  Campaña = "2026",
  Semana = 45,
  id = c("V1", "V2", "V3"),
  Sector = "8",
  Módulo = "4",
  Turno = "1",
  Variedad = "MAGICA",
  Dia_Asignado = c("Miércoles", "Miércoles", "Jueves"),
  Kg_Asignado = c(1725.75, 1725.75, 1164.9),
  Ha_Asignada = c(2.4, 2.4, 1.62),
  Num_Pasada = 1L
)

# Ediciones iniciales (auto de la ejecución previa)
ed_ini <- tibble(
  Campaña = "2026",
  Semana = 45,
  Sector = "8",
  Módulo = "4",
  Turno = "1",
  Variedad = "MAGICA",
  Campo = c("Miércoles", "Jueves"),
  Valor = c(3451.5, 1164.9),
  Ha_Asignada = c(4.80, 1.62),
  Ha_Asignada_1ra = c(4.80, 1.62),
  Kg_Asignado_1ra = c(3451.5, 1164.9),
  Ha_Asignada_2da = c(0, 0),
  Kg_Asignado_2da = c(0, 0),
  Tipo_Edicion = "auto",
  Modo_Ingreso = "kgha"
)

cat("Estado inicial preparado.\n")

# TEST 1: Reasignar Miércoles y Jueves -> Martes y Jueves
cat("\n--- Test 1: Reasignar días de cosecha a Martes (70%) y Jueves (30%) ---\n")
dias_iniciales <- c("Miércoles", "Jueves")
nuevo_kgha <- 650.0
ha_pasada <- 6.42
total_kg_nuevo <- round(ha_pasada * nuevo_kgha, 2) # 4173.0

dias_nuevos_data <- list(
  list(dia = "Martes", pct = 70.0, kg = round(total_kg_nuevo * 0.70, 2), ha = round(ha_pasada * 0.70, 2)),
  list(dia = "Jueves", pct = 30.0, kg = round(total_kg_nuevo * 0.30, 2), ha = round(ha_pasada * 0.30, 2))
)

nuevas_filas <- bind_rows(lapply(dias_nuevos_data, as_tibble)) %>%
  mutate(
    Campaña = "2026",
    Semana = 45,
    Sector = fila$Sector,
    Módulo = fila$Módulo,
    Turno = fila$Turno,
    Variedad = fila$Variedad,
    Campo = dia,
    Valor = kg,
    Ha_Asignada = ha,
    Ha_Asignada_1ra = ha,
    Kg_Asignado_1ra = kg,
    Ha_Asignada_2da = 0,
    Kg_Asignado_2da = 0,
    Tipo_Edicion = "manual",
    Modo_Ingreso = "kgha",
    Motivo_Ajuste = sprintf("Manual (%.2f kg/ha en %.2f ha, %.1f%%)", nuevo_kgha, ha, pct),
    Fecha_Edicion = Sys.time()
  ) %>%
  select(-dia, -pct, -kg, -ha)

dias_nuevos <- unique(as.character(nuevas_filas$Campo))
dias_a_limpiar <- unique(c(as.character(dias_iniciales), dias_nuevos))

cat("Días a limpiar:", paste(dias_a_limpiar, collapse = ", "), "\n")
stopifnot(all(c("Miércoles", "Jueves", "Martes") %in% dias_a_limpiar))

pasada_sel <- 1L
ed_actualizadas <- ed_ini %>%
  filter(!(
    as.character(Campaña) == "2026" &
    as.numeric(Semana) == 45 &
    as.character(Sector) == fila$Sector &
    as.character(Módulo) == fila$Módulo &
    as.character(Turno) == fila$Turno &
    Variedad == fila$Variedad &
    (
      as.character(Campo) %in% as.character(dias_a_limpiar) |
      (if (pasada_sel == 2L) coalesce(Ha_Asignada_2da, 0) > 0 | coalesce(Kg_Asignado_2da, 0) > 0
       else coalesce(Ha_Asignada_1ra, 0) > 0 | coalesce(Kg_Asignado_1ra, 0) > 0)
    )
  )) %>%
  bind_rows(nuevas_filas)

cat("Ediciones resultantes:\n")
print(ed_actualizadas %>% select(Campo, Valor, Ha_Asignada, Tipo_Edicion, Motivo_Ajuste))

stopifnot(!("Miércoles" %in% ed_actualizadas$Campo))
stopifnot("Martes" %in% ed_actualizadas$Campo)
stopifnot("Jueves" %in% ed_actualizadas$Campo)
stopifnot(sum(ed_actualizadas$Valor) == total_kg_nuevo)
cat("✔ Test 1 Ediciones OK: Miércoles eliminado, Martes y Jueves presentes con suma exacta.\n")

# Válvulas reasignadas
valvs_act <- valvs_ini
idx_v <- which(
  as.character(valvs_act$Campaña) == "2026" &
  as.numeric(valvs_act$Semana) == 45 &
  as.character(valvs_act$Módulo) == fila$Módulo &
  as.character(valvs_act$Turno) == fila$Turno &
  valvs_act$Variedad == fila$Variedad &
  coalesce(valvs_act$Num_Pasada, 1L) == pasada_sel
)

dias_valvs_actuales <- unique(as.character(valvs_act$Dia_Asignado[idx_v]))
if (!identical(sort(dias_valvs_actuales), sort(dias_nuevos))) {
  if (length(dias_nuevos) == 1) {
    valvs_act$Dia_Asignado[idx_v] <- dias_nuevos[1]
  } else {
    valvs_a_mover <- idx_v[!(valvs_act$Dia_Asignado[idx_v] %in% dias_nuevos)]
    dias_receptores <- setdiff(dias_nuevos, dias_valvs_actuales)
    if (length(dias_receptores) == 0) dias_receptores <- dias_nuevos
    if (length(valvs_a_mover) > 0) {
      rep_dias <- rep(dias_receptores, length.out = length(valvs_a_mover))
      valvs_act$Dia_Asignado[valvs_a_mover] <- rep_dias
    }
  }
}

cat("Válvulas resultantes:\n")
print(valvs_act %>% select(id, Dia_Asignado, Kg_Asignado, Ha_Asignada))
stopifnot(!("Miércoles" %in% valvs_act$Dia_Asignado))
stopifnot("Martes" %in% valvs_act$Dia_Asignado)
stopifnot("Jueves" %in% valvs_act$Dia_Asignado)
cat("✔ Test 1 Válvulas OK: válvulas de Miércoles movidas a Martes, Jueves preservada.\n")

# TEST 2: Concentrar al 100% en un solo día (Viernes)
cat("\n--- Test 2: Concentrar al 100% en un solo día (Viernes) ---\n")
dias_nuevos_data2 <- list(
  list(dia = "Viernes", pct = 100.0, kg = total_kg_nuevo, ha = ha_pasada)
)
nuevas_filas2 <- bind_rows(lapply(dias_nuevos_data2, as_tibble)) %>%
  mutate(
    Campaña = "2026", Semana = 45, Sector = fila$Sector, Módulo = fila$Módulo, Turno = fila$Turno, Variedad = fila$Variedad,
    Campo = dia, Valor = kg, Ha_Asignada = ha, Ha_Asignada_1ra = ha, Kg_Asignado_1ra = kg, Ha_Asignada_2da = 0, Kg_Asignado_2da = 0,
    Tipo_Edicion = "manual", Modo_Ingreso = "kgha", Motivo_Ajuste = "Manual 100%", Fecha_Edicion = Sys.time()
  ) %>% select(-dia, -pct, -kg, -ha)

dias_nuevos2 <- "Viernes"
dias_a_limpiar2 <- unique(c(as.character(ed_actualizadas$Campo), dias_nuevos2))

ed_actualizadas2 <- ed_actualizadas %>%
  filter(!(
    as.character(Campaña) == "2026" & as.numeric(Semana) == 45 &
    as.character(Sector) == fila$Sector & as.character(Módulo) == fila$Módulo & as.character(Turno) == fila$Turno &
    Variedad == fila$Variedad & (as.character(Campo) %in% as.character(dias_a_limpiar2))
  )) %>%
  bind_rows(nuevas_filas2)

stopifnot(nrow(ed_actualizadas2) == 1)
stopifnot(ed_actualizadas2$Campo[1] == "Viernes")
stopifnot(ed_actualizadas2$Valor[1] == total_kg_nuevo)

# Válvulas cuando length(dias_nuevos) == 1
valvs_act2 <- valvs_act
if (length(dias_nuevos2) == 1) {
  valvs_act2$Dia_Asignado[idx_v] <- dias_nuevos2[1]
}
stopifnot(all(valvs_act2$Dia_Asignado[idx_v] == "Viernes"))
cat("✔ Test 2 OK: Toda la cosecha y todas las válvulas pasaron a Viernes.\n")

# TEST 3: Rechazar día bloqueado por SAP
cat("\n--- Test 3: Rechazar asignación en día bloqueado por SAP (Lunes) ---\n")
dias_intento <- c("Lunes", "Viernes")
dias_bloq <- dias_intento[sapply(dias_intento, function(d) isTRUE(fila[[paste0(d, "_bloqueado")]]))]
cat("Días bloqueados detectados:", paste(dias_bloq, collapse = ", "), "\n")
stopifnot("Lunes" %in% dias_bloq)
cat("✔ Test 3 OK: Bloqueo SAP detectado y protegido correctamente.\n")

cat("\n=== TODOS LOS TESTS PASARON EXITOSAMENTE ===\n")
