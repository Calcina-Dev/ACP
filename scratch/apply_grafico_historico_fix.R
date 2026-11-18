options(warn = 1)
app_path <- "D:/ACP/Actualizacion Semanal/app.R"

# Read file lines
lines <- readLines(app_path, encoding = "UTF-8", warn = FALSE)
full_text <- paste(lines, collapse = "\n")

target <- '        mod_f <- input$filtro_modulo
        tur_f <- input$filtro_turno
        
        d_res <- NA_real_
        es_c  <- FALSE
        
        if (!is.null(mod_f) && length(mod_f) == 1 && !is.null(tur_f) && length(tur_f) == 1 && nrow(df_mad_t) > 0) {
          sub_t <- df_mad_t %>% filter(trimws(as.character(Módulo)) == trimws(as.character(mod_f)),
                                       trimws(as.character(Turno)) == trimws(as.character(tur_f)))
          if (nrow(sub_t) > 0 && !is.na(sub_t$Dias_Maduracion[1])) {
            d_res <- as.numeric(sub_t$Dias_Maduracion[1])
            es_c  <- TRUE
          }
        }
        if (is.na(d_res) && !is.null(mod_f) && length(mod_f) == 1 && nrow(df_mad_m) > 0) {
          sub_m <- df_mad_m %>% filter(trimws(as.character(Módulo)) == trimws(as.character(mod_f)))
          if (nrow(sub_m) > 0 && !is.na(sub_m$Dias_Maduracion[1])) {
            d_res <- as.numeric(sub_m$Dias_Maduracion[1])
          }
        }
        if (is.na(d_res) && nrow(df_mad_t) > 0) {
          vals <- na.omit(suppressWarnings(as.numeric(df_mad_t$Dias_Maduracion)))
          if (length(vals) > 0) d_res <- round(mean(vals))
        }
        if (is.na(d_res) && nrow(df_mad_m) > 0) {
          vals <- na.omit(suppressWarnings(as.numeric(df_mad_m$Dias_Maduracion)))
          if (length(vals) > 0) d_res <- round(mean(vals))
        }'

replacement <- '        mod_f <- input$filtro_modulo
        tur_f <- input$filtro_turno
        var_f <- input$filtro_variedad
        
        d_res <- NA_real_
        es_c  <- FALSE
        
        if (!is.null(mod_f) && length(mod_f) == 1 && !is.null(tur_f) && length(tur_f) == 1 && nrow(df_mad_t) > 0) {
          sub_t <- df_mad_t %>% filter(trimws(as.character(Módulo)) == trimws(as.character(mod_f)),
                                       trimws(as.character(Turno)) == trimws(as.character(tur_f)))
          if (!is.null(var_f) && length(var_f) > 0) {
            sub_t_v <- sub_t %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(var_f))
            if (nrow(sub_t_v) > 0) sub_t <- sub_t_v
          }
          if (nrow(sub_t) > 0 && !is.na(sub_t$Dias_Maduracion[1])) {
            d_res <- as.numeric(sub_t$Dias_Maduracion[1])
            es_c  <- TRUE
          }
        }
        if (is.na(d_res) && !is.null(mod_f) && length(mod_f) == 1 && nrow(df_mad_m) > 0) {
          sub_m <- df_mad_m %>% filter(trimws(as.character(Módulo)) == trimws(as.character(mod_f)))
          if (!is.null(var_f) && length(var_f) > 0) {
            sub_m_v <- sub_m %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(var_f))
            if (nrow(sub_m_v) > 0) sub_m <- sub_m_v
          }
          if (nrow(sub_m) > 0 && !is.na(sub_m$Dias_Maduracion[1])) {
            d_res <- as.numeric(sub_m$Dias_Maduracion[1])
          }
        }
        if (is.na(d_res) && !is.null(var_f) && length(var_f) > 0 && nrow(df_mad_t) > 0) {
          vals_t <- df_mad_t %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(var_f)) %>% pull(Dias_Maduracion) %>% na.omit() %>% as.numeric()
          if (length(vals_t) > 0) d_res <- round(mean(vals_t))
        }
        if (is.na(d_res) && !is.null(var_f) && length(var_f) > 0 && nrow(df_mad_m) > 0) {
          vals_m <- df_mad_m %>% filter(limpiar_variedad(Variedad) %in% limpiar_variedad(var_f)) %>% pull(Dias_Maduracion) %>% na.omit() %>% as.numeric()
          if (length(vals_m) > 0) d_res <- round(mean(vals_m))
        }
        if (is.na(d_res) && nrow(df_mad_t) > 0) {
          vals <- na.omit(suppressWarnings(as.numeric(df_mad_t$Dias_Maduracion)))
          if (length(vals) > 0) d_res <- round(mean(vals))
        }
        if (is.na(d_res) && nrow(df_mad_m) > 0) {
          vals <- na.omit(suppressWarnings(as.numeric(df_mad_m$Dias_Maduracion)))
          if (length(vals) > 0) d_res <- round(mean(vals))
        }'

if (!grepl(target, full_text, fixed = TRUE)) {
  stop("TARGET not found in app.R!")
}
cat("TARGET found! Replacing...\n")
full_text <- sub(target, replacement, full_text, fixed = TRUE)

writeLines(full_text, app_path, useBytes = TRUE)
cat("File written successfully.\n")

parse(app_path)
cat("Syntax validation: 100% OK!\n")
