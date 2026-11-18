app_file <- "Actualizacion Semanal/app.R"
lines <- readLines(app_file, warn = FALSE, encoding = "UTF-8")

idx_uc <- grep("universo_completo <- reactive\\(\\{", lines)[1]
cat("idx_uc:", idx_uc, "\n")

idx_uc_end <- grep("# Observador 1: Módulos \\(Escucha a universo_base", lines)[1]
cat("idx_uc_end:", idx_uc_end, "\n")

idx_ed_filt <- grep("if \\(nrow\\(ed_filtradas\\) > 0\\) \\{", lines)
idx_ed_filt_uc <- idx_ed_filt[idx_ed_filt > idx_uc & idx_ed_filt < idx_uc_end][1]
cat("idx_ed_filt_uc:", idx_ed_filt_uc, "\n")

cat("Lines around idx_ed_filt_uc:\n")
cat(lines[idx_ed_filt_uc:(idx_ed_filt_uc + 15)], sep = "\n")
