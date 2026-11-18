options(warn = 1)
app_path <- "D:/ACP/Actualizacion Semanal/app.R"

lines <- readLines(app_path, encoding = "UTF-8", warn = FALSE)
full_text <- paste(lines, collapse = "\n")

target <- '    df_mad_mod <- if (exists("rv_maduracion") && is.function(rv_maduracion)) rv_maduracion() else tibble()
    df_mad_tur <- if (exists("rv_maduracion_turno") && is.function(rv_maduracion_turno)) rv_maduracion_turno() else tibble()
    sens_pct   <- if (!is.null(input$sensibilidad_floral_pct)) input$sensibilidad_floral_pct else 50'

replacement <- '    df_mad_mod <- if (exists("rv_maduracion") && is.function(rv_maduracion)) rv_maduracion() else tibble()
    df_mad_tur <- if (exists("rv_maduracion_turno") && is.function(rv_maduracion_turno)) rv_maduracion_turno() else tibble()
    sens_pct   <- tryCatch(rv_sensibilidad_floral(), error = function(e) 50)'

if (!grepl(target, full_text, fixed = TRUE)) {
  stop("Target not found!")
}

full_text <- sub(target, replacement, full_text, fixed = TRUE)
writeLines(full_text, app_path, useBytes = TRUE)
cat("Line 17183 updated successfully.\n")

parse(app_path)
cat("Syntax check: 100% OK!\n")
