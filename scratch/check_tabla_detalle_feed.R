lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

# Let's search for rv_ajuste_valvulas in tabla_detalle_turnos
idx <- grep("tabla_detalle_turnos <-", lines)
cat("Lines around tabla_detalle_turnos:\n")
cat(paste(lines[idx:(idx+60)], collapse = "\n"))
