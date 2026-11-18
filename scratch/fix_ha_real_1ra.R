archivo <- "Actualizacion Semanal/app.R"
lineas <- readLines(archivo, encoding = "UTF-8", warn = FALSE)

idx <- grep("# 3. Calcular hectáreas consumidas en días previos reales y otros días de la semana", lineas, fixed = TRUE)[1]
cat("idx:", idx, "\n")

# Insertar ha_real_1ra y ha_real_2da
insert_lines <- c(
  "    ha_real_1ra <- coalesce(as.numeric(fila[['Ha Real 1ª Pas']]), 0)",
  "    ha_real_2da <- coalesce(as.numeric(fila[['Ha Real 2ª Pas']]), 0)"
)

# Comprobar si ya están en las siguientes 3 líneas
if (!any(grepl("ha_real_1ra <- coalesce", lineas[idx:(idx + 3)]))) {
  lineas <- c(lineas[1:idx], insert_lines, lineas[(idx + 1):length(lineas)])
  cat("Insertado ha_real_1ra y ha_real_2da en linea", idx + 1, "\n")
}

# También mejorar la condición en pasada_sugerida
idx_pas <- grep("} else if (limpiar_variedad(fila$Variedad) == \"ROCIO\" && ha_real_1ra >= (area_turno - 0.05)) {", lineas, fixed = TRUE)[1]
if (!is.na(idx_pas)) {
  lineas[idx_pas] <- "    } else if (limpiar_variedad(fila$Variedad) == \"ROCIO\" && (ha_consumida_otros_1ra >= (area_turno - 0.05) || ha_real_1ra >= (area_turno - 0.05))) {"
  cat("Actualizada condicion de pasada_sugerida\n")
}

writeLines(lineas, archivo, useBytes = TRUE)
cat("Actualizado con exito.\n")
