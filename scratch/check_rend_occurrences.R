lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

cat("Occurrences of Rend_Base_Con_Floral:\n")
for (i in grep("Rend_Base_Con_Floral", lines)) cat(sprintf("%d: %s\n", i, trimws(lines[i])))

cat("\nOccurrences of Rend_Turno_Aplicar:\n")
for (i in grep("Rend_Turno_Aplicar", lines)) cat(sprintf("%d: %s\n", i, trimws(lines[i])))

cat("\nOccurrences of Rend_Valv_Regular:\n")
for (i in grep("Rend_Valv_Regular", lines)) cat(sprintf("%d: %s\n", i, trimws(lines[i])))
