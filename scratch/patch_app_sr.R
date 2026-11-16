lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)

# 1. Patch df_rec columns (around line 1817)
idx_desfase <- grep("Kg_Desfase       = 0,", lines, fixed = TRUE)
cat("Found idx_desfase at:", idx_desfase, "\n")

# 2. Patch obtener_cosecha_conciliada_semana (around lines 1825 - 1960)
idx_loop_start <- grep("for (i in seq_len(nrow(df_rec))) {", lines, fixed = TRUE)
cat("Found idx_loop_start at:", idx_loop_start, "\n")

# 3. Patch ui_kpi_real_detalle (around line 6551)
idx_kpi_sin_r <- grep("sin_r_mask <- !df_concil$Tiene_Campo & df_concil$Tiene_SAP", lines, fixed = TRUE)
cat("Found idx_kpi_sin_r at:", idx_kpi_sin_r, "\n")

# 4. Patch datos_resumen_base (around line 9818)
idx_resumen_sr <- grep("sr_mask <- (!coalesce(df_concil$Tiene_Campo, FALSE) & coalesce(df_concil$Tiene_SAP, FALSE)) |", lines, fixed = TRUE)
cat("Found idx_resumen_sr at:", idx_resumen_sr, "\n")

# 5. Patch kpi_concil_sin_ruta_sub (around line 15151)
idx_kpi_sub <- grep("kg_tot <- round(sum(sub_df$Kg_Total_sap, na.rm = TRUE), 0)", lines, fixed = TRUE)
cat("Found idx_kpi_sub at:", idx_kpi_sub, "\n")
