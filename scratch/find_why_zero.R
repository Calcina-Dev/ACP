lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8")
start_idx <- grep("ejecutar_ajuste_semana <- function", lines)[1]
end_idx   <- grep("No hay v.lvulas o d.as pendientes", lines)[1] + 5

# We will create an environment that simulates running this
# First extract lines up to UI to have all functions:
idx_ui <- grep("ui <- fluidPage", lines)[1]
helpers <- lines[1:(idx_ui - 1)]

# Create a clean mock runner
body_lines <- lines[(start_idx + 1):end_idx]

# Replace inputs and reactives cleanly:
body_clean <- body_lines
body_clean <- gsub("input\\$filtro_campana", '"2026"', body_clean)
body_clean <- gsub("input\\$filtro_semana", '45', body_clean)
body_clean <- gsub("input\\$ajustar_desde_dia", '"Martes"', body_clean)
body_clean <- gsub("input\\$ajustar_hasta_dia", '"Viernes"', body_clean)
body_clean <- gsub("input\\$switch_ajuste_cualquier_semana", 'TRUE', body_clean)
body_clean <- gsub("rv_modo_frecuencia\\(\\)", '"auto"', body_clean)
body_clean <- gsub("rv_frecuencias\\(\\)", 'tibble()', body_clean)
body_clean <- gsub("rv_pct_rocio\\(\\)", 'tibble()', body_clean)
body_clean <- gsub("rv_umbral_avance\\(\\)", '15', body_clean)
body_clean <- gsub("rv_cosecha\\(\\)", 'df_cosecha', body_clean)
body_clean <- gsub("rv_rutas_cosecha\\(\\)", 'if (exists("df_rutas_cosecha")) df_rutas_cosecha else tibble()', body_clean)
body_clean <- gsub("rv_ajuste_valvulas\\(\\)", 'tibble()', body_clean)
body_clean <- gsub("ediciones_manuales\\(\\)", 'readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")', body_clean)
body_clean <- gsub("universo_base\\(\\)", 'u_base_mock', body_clean)
body_clean <- gsub("turnos_activos_filtrados\\(\\)", 'turnos_filtro_mock', body_clean)
body_clean <- gsub("proyeccion_semanal\\(\\)", 'df_proyecciones %>% filter(Campaña == "2026", Semana == 45) %>% mutate(Módulo_join = Módulo, Turno_join = Turno, Variedad_join = Variedad)', body_clean)
body_clean <- gsub("withProgress\\(.*?, \\{", "", body_clean)
body_clean <- gsub("incProgress\\(.*?\\)", "# incProgress", body_clean)
body_clean <- gsub("showNotification\\(.*?\\)", "cat('NOTIFICATION TRIGGERED\\n')", body_clean)

# Add diagnostic prints
diag <- c(
  'cat("--- DIAGNOSTICS ---\\n")',
  'cat("valvulas_calc rows:", nrow(valvulas_calc), "\\n")',
  'cat("reg_regular rows:", nrow(reg_regular), "\\n")',
  'if (nrow(reg_regular) > 0) print(reg_regular %>% select(Módulo, Turno, Dia_Asignado, Kg_Asignado, Ha_Regular))',
  'cat("registros_todos rows:", nrow(registros_todos), "\\n")',
  'cat("auto_ediciones rows:", nrow(auto_ediciones), "\\n")',
  'if (nrow(auto_ediciones) > 0) print(auto_ediciones %>% select(Módulo, Turno, Campo, Valor, Ha_Asignada))',
  'cat("claves_auto_mtv:\\n")',
  'print(claves_auto_mtv)',
  'cat("claves_sap_bloqueadas_mtv:\\n")',
  'print(claves_sap_bloqueadas_mtv)',
  'cat("auto_validas rows:", nrow(auto_validas), "\\n")',
  'if (nrow(auto_validas) > 0) print(auto_validas)'
)

# Insert diag right before "if (nrow(auto_validas) == 0)"
idx_ins <- grep("if \\(nrow\\(auto_validas\\) == 0\\)", body_clean)[1]
body_with_diag <- c(body_clean[1:(idx_ins - 1)], diag, body_clean[idx_ins:length(body_clean)])

setup <- c(
  'library(dplyr)',
  'library(tidyr)',
  'load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")',
  'solo_filtrado <- TRUE',
  'turnos_filtro_mock <- tibble(Sector = "6", Módulo = "1", Turno = as.character(1:5), Variedad = "SEKOYA POP")',
  'u_base_mock <- df_proyecciones %>%',
  '  filter(Campaña == "2026", Semana == 45, as.character(Módulo) == "1") %>%',
  '  mutate(',
  '    Sector = "6", Módulo = "1", Turno = as.character(Turno), Variedad = "SEKOYA POP",',
  '    `Ha Real 2ª Pas` = 0,',
  '    Lunes = NA_real_, Lunes_bloqueado = FALSE,',
  '    Martes = 1431.81, Martes_bloqueado = TRUE,',
  '    Miércoles = 1684.68, Miércoles_bloqueado = TRUE,',
  '    Jueves = NA_real_, Jueves_bloqueado = FALSE,',
  '    Viernes = NA_real_, Viernes_bloqueado = FALSE,',
  '    Sábado = NA_real_, Sábado_bloqueado = FALSE',
  '  )'
)

test_file <- "D:/ACP/scratch/run_find_zero.R"
writeLines(c(helpers, setup, body_with_diag), test_file, useBytes = TRUE)
cat("Wrote run_find_zero.R\n")
