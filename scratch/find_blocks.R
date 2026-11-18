# Script to apply decoupling of Ha Real from DESDE and refine VISTA Real vs Ajustado

app_file <- "Actualizacion Semanal/app.R"
lines <- readLines(app_file, warn = FALSE, encoding = "UTF-8")

cat("Total lineas leidas:", length(lines), "\n")

# Localizar inicio de 4B
idx_4b <- grep("# 4B. Calcular Ha Recorrida desde Rutas de Campo", lines)[1]
cat("idx_4b:", idx_4b, "\n")

# Localizar 5. Flags iniciales de bloqueo
idx_5flags <- grep("# 5. Flags iniciales de bloqueo", lines)[1]
cat("idx_5flags:", idx_5flags, "\n")

cat("Lineas entre 4B y 5:\n")
cat(lines[idx_4b:(idx_4b + 5)], sep = "\n")
cat("...\n")
cat(lines[(idx_5flags - 5):idx_5flags], sep = "\n")
