suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
dirs_form <- file.path(base_dir, "Proyección Semanal Formalizada")
files_form <- list.files(dirs_form, pattern = "\\.xlsx$", full.names = TRUE)

e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
df_p <- e$df_proyecciones

cat("=== COMPARISON ALL WEEKS: RData (Calculo) vs Formalizada ===\n")
res_comp <- list()

for (w in 21:47) {
  rdata_w <- sum((df_p %>% filter(Semana == w))$Proyección, na.rm = TRUE)
  
  # Find formalizada file for week w
  pat <- sprintf("Sem\\s*0*%d\\b", w)
  f_w <- files_form[grepl(pat, basename(files_form), ignore.case = TRUE)]
  # if multiple, sort descending (V2, etc.)
  f_w <- sort(f_w, decreasing = TRUE)
  f_sel <- f_w[!grepl("QALI|Ajuste", f_w)][1]
  
  form_tot <- NA_real_
  if (!is.na(f_sel) && file.exists(f_sel)) {
    tryCatch({
      r_df <- read_excel(f_sel, sheet = "Resumen")
      row_n <- which(grepl("Total Escenario Neutral", r_df[[1]], ignore.case = TRUE))
      if (length(row_n) > 0) {
        form_tot <- suppressWarnings(as.numeric(r_df[[2]][row_n[1]]))
      }
    }, error = function(e) NULL)
  }
  
  cat(sprintf("Semana %02d | RData: %10.1f | Formalizada: %10.1f | Dif: %+10.1f\n",
              w, rdata_w, coalesce(form_tot, 0), rdata_w - coalesce(form_tot, 0)))
}
