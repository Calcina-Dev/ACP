# remove_dia_selector.R
# Script para remover el selector de Dia redundante en la pestaña Comparativo

app_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(app_path, encoding = "UTF-8", warn = FALSE)

# 1. Encontrar y remover el conditionalPanel con filtro_dia_comp en la UI
start_cp <- grep("conditionalPanel\\(\\s*condition = \"input\\.filtro_enfoque_comp == 'rango_escenario'\"", lines)[1]
if (is.na(start_cp)) {
  # intentar con regex mas flexible
  start_cp <- grep("filtro_dia_comp", lines)[1]
  while (start_cp > 1 && !grepl("conditionalPanel", lines[start_cp])) {
    start_cp <- start_cp - 1
  }
}

if (!is.na(start_cp)) {
  # Encontrar el cierre de este conditionalPanel
  # Debe cerrar con ),
  curr <- start_cp
  depth <- 0
  found_open <- FALSE
  end_cp <- NA
  for (i in start_cp:min(start_cp + 25, length(lines))) {
    line <- lines[i]
    opens <- (nchar(line) - nchar(gsub("\\(", "", line)))
    closes <- (nchar(line) - nchar(gsub("\\)", "", line)))
    if (opens > 0) found_open <- TRUE
    depth <- depth + opens - closes
    if (found_open && depth <= 0) {
      end_cp <- i
      break
    }
  }
  
  if (!is.na(end_cp)) {
    cat(sprintf("Removiendo conditionalPanel filtro_dia_comp desde linea %d a %d\n", start_cp, end_cp))
    lines <- lines[-(start_cp:end_cp)]
  } else {
    stop("No se pudo determinar el cierre de conditionalPanel filtro_dia_comp")
  }
} else {
  stop("No se encontro conditionalPanel con filtro_dia_comp")
}

# 2. En el server, simplificar dias_filtrar <- dias_escenario
# Buscar linea: dia_sel <- if (!is.null(input$filtro_dia_comp)) ...
dia_server_idx <- grep("dia_sel <- if \\(!is\\.null\\(input\\$filtro_dia_comp\\)\\)", lines)[1]
if (!is.na(dia_server_idx)) {
  cat(sprintf("Actualizando servidor en linea %d para evaluar directamente dias_escenario\n", dia_server_idx))
  # La siguiente linea es dias_filtrar <- if (dia_sel != "Todos" ...
  lines[dia_server_idx] <- "    # Auditoria acumulada del escenario en su rango completo"
  lines[dia_server_idx + 1] <- "    dias_filtrar <- dias_escenario"
} else {
  stop("No se encontro dia_sel en el servidor")
}

# 3. Pulir estilo de la etiqueta Escenario en Comparativo (color coherente con Emil Kowalski)
lines <- gsub('span\\(style = "font-size: 11px; font-weight: 700; color: #4F46E5; text-transform: uppercase; letter-spacing: 0.04em;", "Escenario:"\\)',
              'span(style = "font-size: 11px; font-weight: 700; color: #636366; text-transform: uppercase; letter-spacing: 0.04em;", icon("bookmark"), " Escenario:")',
              lines)

# Escribir archivo
writeLines(lines, app_path, useBytes = TRUE)
cat("Selector de Dia removido con exito y app.R actualizado.\n")
