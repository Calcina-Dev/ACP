# Patch script for app.R to implement:
# 1. Pure Vista Ajustado (prior days = Real SAP, scenario days = ONLY scenario edits, no Real SAP leaks)
# 2. Redesigned Comparativo Tab with 3 perspectives: Rango Escenario, Semana Completa (Proy vs Ajus vs Real), and Entre Escenarios
# 3. Full module/sector filter alignment and normalized modules (13, 14, etc.)

file_path <- "D:/ACP/Actualizacion Semanal/app.R"
lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)
cat("Lines before patch:", length(lines), "\n")

# -----------------------------------------------------------------------------
# 1. Update universo_completo in app.R
# -----------------------------------------------------------------------------
idx_uc <- grep("universo_completo <- reactive", lines)[1]
idx_claves <- grep("claves_u <- paste\\(trimws\\(as.character\\(universo\\$Sector\\)\\)", lines)[1]
cat(sprintf("universo_completo at %d, claves_u at %d\n", idx_uc, idx_claves))

# Let's inspect lines around idx_claves
# We want to insert the cleaning logic right before the for (i in seq_len(nrow(ed_filtradas))) loop!
idx_for <- grep("for \\(i in seq_len\\(nrow\\(ed_filtradas\\)\\)\\)", lines)[1]
cat("for (i in seq_len(nrow(ed_filtradas))) at line:", idx_for, "\n")

# Let's check lines from idx_claves - 5 to idx_for + 5
for (i in (idx_claves - 2):(idx_for + 2)) {
  cat(sprintf("%4d: %s\n", i, lines[i]))
}
