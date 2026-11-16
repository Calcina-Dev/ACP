suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"

f_det_v1 <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44).xlsx")
f_det_v2 <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")
f_form_v1 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51.xlsx")
f_form_v2 <- file.path(base_dir, "Proyección Semanal Formalizada", "26.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 46 al 51_V2.xlsx")

cat("=== EXCEL SHEETS & SIZES ===\n")
for (f in c(f_det_v1, f_det_v2, f_form_v1, f_form_v2)) {
  cat("\nFile:", basename(f), "\n")
  if (file.exists(f)) {
    sheets <- excel_sheets(f)
    cat("  Sheets:", paste(sheets, collapse = ", "), "\n")
  } else {
    cat("  NOT FOUND\n")
  }
}

# Inspect Detallada V1
cat("\n=== DETALLADA V1 ===\n")
if (file.exists(f_det_v1)) {
  for (sh in excel_sheets(f_det_v1)) {
    df <- tryCatch(read_excel(f_det_v1, sheet = sh, n_max = 50), error = function(e) NULL)
    if (!is.null(df)) {
      # find any cell or col with 405865 or near
      # also check full sheet if needed
    }
  }
  # Read with skip = 3
  df_calc_v1 <- tryCatch(read_excel(f_det_v1, sheet = "Cálculo", skip = 3), error = function(e) NULL)
  if (!is.null(df_calc_v1)) {
    cat("V1 Cálculo cols:", names(df_calc_v1)[1:15], "\n")
    col_p <- names(df_calc_v1)[grep("proy", names(df_calc_v1), ignore.case = TRUE)]
    cat("V1 Proy cols:", col_p, "\n")
    for (cp in col_p) {
      val <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(df_calc_v1[[cp]]))))
      cat("  V1", cp, "sum:", sum(val, na.rm = TRUE), "\n")
    }
  }
}

# Inspect Detallada V2
cat("\n=== DETALLADA V2 ===\n")
if (file.exists(f_det_v2)) {
  for (sh in excel_sheets(f_det_v2)) {
    cat("Checking sheet:", sh, "\n")
    df_raw <- tryCatch(read_excel(f_det_v2, sheet = sh), error = function(e) NULL)
    if (!is.null(df_raw)) {
      # search for 405865 anywhere
      match_405 <- which(df_raw == 405865 | df_raw == "405865" | df_raw == 405865.2, arr.ind = TRUE)
      if (nrow(match_405) > 0) {
        cat("  FOUND 405865 in sheet", sh, "at:\n")
        print(match_405)
      }
    }
  }
  
  df_calc_v2 <- tryCatch(read_excel(f_det_v2, sheet = "Cálculo", skip = 3), error = function(e) NULL)
  if (!is.null(df_calc_v2)) {
    col_p <- names(df_calc_v2)[grep("proy", names(df_calc_v2), ignore.case = TRUE)]
    cat("V2 Proy cols in Cálculo:\n")
    for (cp in col_p) {
      val <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(df_calc_v2[[cp]]))))
      cat("  V2", cp, "sum:", sum(val, na.rm = TRUE), "\n")
    }
    
    # Also look at rows: are any rows filtered out?
    # Like NA sector, NA turno, or Proyección <= 0?
    cat("Total rows in df_calc_v2:", nrow(df_calc_v2), "\n")
    # Col 13 is Proyección (Kg) usually
    if (ncol(df_calc_v2) >= 13) {
      col13 <- names(df_calc_v2)[13]
      cat("Col 13 name:", col13, "\n")
      v13 <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(df_calc_v2[[col13]]))))
      cat("Col 13 raw sum:", sum(v13, na.rm = TRUE), "\n")
    }
  }
}

# Inspect Formalizada V1 and V2
cat("\n=== FORMALIZADA V1 & V2 ===\n")
for (ff in c(f_form_v1, f_form_v2)) {
  cat("\nChecking", basename(ff), "\n")
  if (file.exists(ff)) {
    for (sh in excel_sheets(ff)) {
      df <- tryCatch(read_excel(ff, sheet = sh), error = function(e) NULL)
      if (!is.null(df)) {
        # Check for 405865 or 383565
        m_405 <- which(abs(suppressWarnings(as.numeric(as.matrix(df))) - 405865) < 1, arr.ind = TRUE)
        if (nrow(m_405) > 0) cat("  FOUND 405865 in sheet", sh, "\n")
        m_383 <- which(abs(suppressWarnings(as.numeric(as.matrix(df))) - 383565) < 1, arr.ind = TRUE)
        if (nrow(m_383) > 0) cat("  FOUND 383565 in sheet", sh, "\n")
      }
    }
  }
}
