suppressMessages({
  library(readxl)
  library(dplyr)
})

base_dir <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal"
f_det_v2  <- file.path(base_dir, "Proyección Semanal Detallada", "26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx")

# Check sheet Calculo headers
calc_top <- read_excel(f_det_v2, sheet = "Calculo", n_max = 5)
print(as.data.frame(calc_top))

# Check skip = 3
calc_all <- read_excel(f_det_v2, sheet = "Calculo", skip = 3)
cat("All column names in Calculo:\n")
print(names(calc_all))

# Are there other projection columns in Calculo?
for (i in seq_along(names(calc_all))) {
  nm <- names(calc_all)[i]
  # check numeric sum
  v <- suppressWarnings(as.numeric(calc_all[[i]]))
  if (sum(!is.na(v)) > 50) {
    s <- sum(v, na.rm = TRUE)
    if (s > 10000) {
      cat(sprintf("Col %d: %-30s | Sum: %12.2f\n", i, nm, s))
    }
  }
}
