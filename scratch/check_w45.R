suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_form_45 <- file.path(base_dir, "Proyección Semanal Formalizada", "25.- Proyección Cosecha Arándano ACP _ 2026 _ Sem 45 al 50_V2.xlsx")
f_det_45  <- file.path(base_dir, "Proyección Semanal Detallada", "25. Proyecciones Semana 45 + 5 semanas ACP (Conteo S43)_V2.xlsx")

if (file.exists(f_form_45)) {
  r45 <- read_excel(f_form_45, sheet = "Resumen")
  # find Total Escenario Neutral
  row_tot <- which(grepl("Total Escenario Neutral", r45[[1]], ignore.case = TRUE))
  cat("Week 45 Formalizada Total:", r45[[2]][row_tot], "\n")
}

if (file.exists(f_det_45)) {
  c45 <- read_excel(f_det_45, sheet = "Calculo", skip = 3)
  col_p <- names(c45)[grep("proy", names(c45), ignore.case = TRUE)]
  cat("Week 45 Calculo Detallada:", sum(suppressWarnings(as.numeric(c45[[col_p[1]]])), na.rm = TRUE), "\n")
}

e <- new.env()
load("Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
cat("Week 45 in RData:", sum((e$df_proyecciones %>% filter(Semana == 45))$Proyección, na.rm = TRUE), "\n")
