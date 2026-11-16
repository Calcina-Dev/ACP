lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

cat("--- Search 1: pasada_sugerida ---\n")
idx <- grep("pasada_sugerida <- if", lines)
for (i in idx) cat(i, ":", lines[i], "\n")

cat("\n--- Search 2: ui_modal_calculo_vivo pasada_activa ---\n")
idx <- grep("pasada_activa <- if", lines)
for (i in idx) cat(i, ":", lines[i], "\n")

cat("\n--- Search 3: num_pasada_sel ---\n")
idx <- grep("num_pasada_sel <-", lines)
for (i in idx) cat(i, ":", lines[i], "\n")

cat("\n--- Search 4: ha_ajustadas_ed ---\n")
idx <- grep("ha_ajustadas_ed <-", lines)
for (i in idx) cat(i, ":", lines[i], "\n")

cat("\n--- Search 5: 1ª Pasada Ajustada ---\n")
idx <- grep("1ª Pasada Ajustada", lines)
for (i in idx) cat(i, ":", lines[i], "\n")
