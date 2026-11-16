lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)

cat("Total lines in app.R:", length(lines), "\n")

# Find line for kpi_cumplimiento_badge
l_cump <- grep("kpi_cumplimiento_badge", lines)
cat("kpi_cumplimiento_badge at line:", l_cump, "\n")
print(lines[(l_cump - 5):(l_cump + 5)])

# Find line for tabPanel Evolución
l_evol <- grep('tabPanel\\("Evoluci', lines)
cat("tabPanel Evolución at line:", l_evol, "\n")

# Find line for ui_kpi_ajuste_detalle
l_ajus <- grep('output\\$ui_kpi_ajuste_detalle', lines)
cat("ui_kpi_ajuste_detalle at line:", l_ajus, "\n")

# Find line for kpi_cumplimiento_badge in server
l_cump_srv <- grep('output\\$kpi_cumplimiento_badge', lines)
cat("output$kpi_cumplimiento_badge at line:", l_cump_srv, "\n")
