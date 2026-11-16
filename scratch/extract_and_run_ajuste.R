lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8")
idx_ui    <- grep("ui <- fluidPage", lines)[1]
start_idx <- grep("ejecutar_ajuste_semana <- function", lines)[1]
end_idx   <- grep("ejecutar_deshacer_ajuste <- function", lines)[1] - 1

helpers_code <- lines[1:(idx_ui - 1)]
fn_body      <- lines[(start_idx + 1):end_idx]

# Replace reactive calls with direct values/functions
fn_body <- gsub("input\\$filtro_campana", '"2026"', fn_body)
fn_body <- gsub("input\\$filtro_semana", '45', fn_body)
fn_body <- gsub("input\\$ajustar_desde_dia", '"Martes"', fn_body)
fn_body <- gsub("input\\$ajustar_hasta_dia", '"Viernes"', fn_body)
fn_body <- gsub("input\\$switch_ajuste_cualquier_semana", 'TRUE', fn_body)
fn_body <- gsub("rv_modo_frecuencia\\(\\)", '"auto"', fn_body)
fn_body <- gsub("rv_frecuencias\\(\\)", 'tibble()', fn_body)
fn_body <- gsub("rv_pct_rocio\\(\\)", 'tibble()', fn_body)
fn_body <- gsub("rv_umbral_avance\\(\\)", '15', fn_body)
fn_body <- gsub("rv_cosecha\\(\\)", 'df_cosecha', fn_body)
fn_body <- gsub("rv_rutas_cosecha\\(\\)", 'if (exists("df_rutas_cosecha")) df_rutas_cosecha else tibble()', fn_body)
fn_body <- gsub("rv_ajuste_valvulas\\(\\)", 'readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_historico.rds")', fn_body)
fn_body <- gsub("guardar_ajuste_valvulas_disco\\(valvs_guardar\\)", 'cat("Guardando valvs:", nrow(valvs_guardar), "\\n")', fn_body)
fn_body <- gsub("rv_ajuste_valvulas\\(valvs_guardar\\)", '#', fn_body)
fn_body <- gsub("ediciones_manuales\\(\\)", 'readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")', fn_body)
fn_body <- gsub("guardar_ediciones_disco\\(ed_guardar\\)", 'cat("Guardando ediciones:", nrow(ed_guardar), "\\n")', fn_body)
fn_body <- gsub("ediciones_manuales\\(ed_guardar\\)", '#', fn_body)
fn_body <- gsub("trigger_refresh\\(trigger_refresh\\(\\) \\+ 1\\)", '#', fn_body)
fn_body <- gsub("showNotification\\((.*?)\\)", 'cat("NOTIFICATION:", \\1, "\\n")', fn_body)
fn_body <- gsub("withProgress\\((.*?), \\{", '{', fn_body)
fn_body <- gsub("incProgress\\((.*?)\\)", '# incProgress', fn_body)
fn_body <- gsub("universo_base\\(\\)", 'u_base_sim', fn_body)
fn_body <- gsub("turnos_activos_filtrados\\(\\)", 'turnos_filtro_sim', fn_body)
fn_body <- gsub("proyeccion_semanal\\(\\)", 'proy_sem_sim', fn_body)

setup <- c(
  'solo_filtrado <- TRUE',
  'turnos_filtro_sim <- tibble(Sector = "6", Módulo = "1", Turno = as.character(1:5), Variedad = "SEKOYA POP")',
  'proy_sem_sim <- df_proyecciones %>% filter(Campaña == "2026", Semana == 45) %>% mutate(Módulo_join = Módulo, Turno_join = Turno, Variedad_join = Variedad)',
  '# Simulate u_base_sim for M1:',
  'u_base_sim <- df_proyecciones %>%',
  '  filter(Campaña == "2026", Semana == 45, Módulo == "1") %>%',
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

full_script <- c(helpers_code, setup, fn_body)
writeLines(full_script, "D:/ACP/scratch/run_test_ajuste_extracted.R", useBytes = TRUE)
cat("Wrote extracted script. Total lines:", length(full_script), "\n")
