suppressPackageStartupMessages({
  library(dplyr)
})

# Let's inspect size of main objects in data maestra
e <- new.env()
load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData", envir = e)
cat("Objects in datos_maestros_cosecha_2026.RData:\n")
for (obj in ls(e)) {
  val <- get(obj, envir = e)
  size_mb <- round(as.numeric(object.size(val)) / (1024 * 1024), 2)
  nrows <- if (is.data.frame(val)) nrow(val) else length(val)
  cat(sprintf("  %-30s : %8.2f MB (%d rows/elements)\n", obj, size_mb, nrows))
}
