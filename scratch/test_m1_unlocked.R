library(dplyr)
library(tidyr)
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")

lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8")
idx_ui    <- grep("ui <- fluidPage", lines)[1]
start_idx <- grep("ejecutar_ajuste_semana <- function", lines)[1]
end_idx   <- grep("ejecutar_deshacer_ajuste <- function", lines)[1] - 1

helpers_code <- lines[1:(idx_ui - 1)]
fn_body      <- lines[(start_idx + 1):end_idx]

# Modify fn_body:
# Replace the loop for claves_sap_bloqueadas to only check dias_previos_bloqueo:
target_loop <- 'for (dia_col in dias_nombres) {'
repl_loop   <- 'dias_previos_bloq <- if (dia_inicio_idx > 1) dias_nombres[1:(dia_inicio_idx - 1)] else character(0)\n        for (dia_col in dias_previos_bloq) {'

fn_body_mod <- gsub(target_loop, repl_loop, fn_body, fixed = TRUE)

# Replace other reactive calls
fn_body_mod <- gsub("input\\$filtro_campana", '"2026"', fn_body_mod)
fn_body_mod <- gsub("input\\$filtro_semana", '45', fn_body_mod)
fn_body_mod <- gsub("input\\$ajustar_desde_dia", '"Martes"', fn_body_mod)
fn_body_mod <- gsub("input\\$ajustar_hasta_dia", '"Viernes"', fn_body_mod)
fn_body_mod <- gsub("input\\$switch_ajuste_cualquier_semana", 'TRUE', fn_body_mod)
fn_body_mod <- gsub("rv_modo_frecuencia\\(\\)", '"auto"', fn_body_mod)
fn_body_mod <- gsub("rv_frecuencias\\(\\)", 'tibble()', fn_body_mod)
fn_body_mod <- gsub("rv_pct_rocio\\(\\)", 'tibble()', fn_body_mod)
fn_body_mod <- gsub("rv_umbral_avance\\(\\)", '15', fn_body_mod)
fn_body_mod <- gsub("rv_cosecha\\(\\)", 'df_cosecha', fn_body_mod)
fn_body_mod <- gsub("rv_rutas_cosecha\\(\\)", 'if (exists("df_rutas_cosecha")) df_rutas_cosecha else tibble()', fn_body_mod)
fn_body_mod <- gsub("rv_ajuste_valvulas\\(\\)", 'readRDS("D:/ACP/Actualizacion Semanal/ajuste_valvulas_historico.rds")', fn_body_mod)
fn_body_mod <- gsub("guardar_ajuste_valvulas_disco\\(valvs_guardar\\)", 'cat("Guardando valvs:", nrow(valvs_guardar), "\\n")', fn_body_mod)
fn_body_mod <- gsub("rv_ajuste_valvulas\\(valvs_guardar\\)", '#', fn_body_mod)
fn_body_mod <- gsub("ediciones_manuales\\(\\)", 'readRDS("D:/ACP/Actualizacion Semanal/ediciones_manuales_cosecha.rds")', fn_body_mod)
fn_body_mod <- gsub("guardar_ediciones_disco\\(ed_guardar\\)", 'cat("Guardando ediciones:", nrow(ed_guardar), "\\n")', fn_body_mod)
fn_body_mod <- gsub("ediciones_manuales\\(ed_guardar\\)", '#', fn_body_mod)
fn_body_mod <- gsub("trigger_refresh\\(trigger_refresh\\(\\) \\+ 1\\)", '#', fn_body_mod)
fn_body_mod <- gsub("withProgress\\((.*?), \\{", '{', fn_body_mod)
fn_body_mod <- gsub("incProgress\\((.*?)\\)", '# incProgress', fn_body_mod)
fn_body_mod <- gsub("universo_base\\(\\)", 'u_base_sim', fn_body_mod)
fn_body_mod <- gsub("turnos_activos_filtrados\\(\\)", 'turnos_filtro_sim', fn_body_mod)
fn_body_mod <- gsub("proyeccion_semanal\\(\\)", 'proy_sem_sim', fn_body_mod)

# Add print of auto_ediciones and auto_validas before return
idx_check <- grep("if \\(nrow\\(auto_validas\\) == 0\\)", fn_body_mod)[1]
debug_print <- c(
  'cat("=== DEBUG PRINTS ===", "\\n")',
  'cat("nrow auto_ediciones:", nrow(auto_ediciones), "\\n")',
  'if (nrow(auto_ediciones) > 0) print(auto_ediciones %>% select(Módulo, Turno, Campo, Valor, Ha_Asignada))',
  'cat("nrow auto_validas:", nrow(auto_validas), "\\n")',
  'if (nrow(auto_validas) > 0) print(auto_validas %>% select(Módulo, Turno, Campo, Valor, Ha_Asignada))'
)
fn_body_mod <- c(fn_body_mod[1:(idx_check - 1)], debug_print, fn_body_mod[idx_check:length(fn_body_mod)])

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

# Also fix the showNotification lines that had unescaped quotes if any
fn_body_mod <- gsub('showNotification\\("El ajuste automático está restringido.*?\\)', 'cat("Aviso semana\\n")', fn_body_mod)
fn_body_mod <- gsub('showNotification\\("El ajuste automático solo está habilitado.*?\\)', 'cat("Aviso campana\\n")', fn_body_mod)
fn_body_mod <- gsub('showNotification\\(msg_v, type = "warning", duration = 4\\)', 'cat("AVISO FINAL:", msg_v, "\\n")', fn_body_mod)
fn_body_mod <- gsub('showNotification\\(sprintf\\("Se autocompletaron.*?\\)', 'cat("EXITO AUTOCOMPLETADO\\n")', fn_body_mod)

full_script <- c(helpers_code, setup, fn_body_mod)
writeLines(full_script, "D:/ACP/scratch/run_test_m1_unlocked.R", useBytes = TRUE)
cat("Wrote unlocked test script.\n")
