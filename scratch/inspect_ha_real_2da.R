lines <- readLines("Actualizacion Semanal RNA/app.R", warn = FALSE)
idx <- grep("Ha Real 2ª Pas", lines)[1]
cat("Around line", idx, ":\n")
cat(lines[(idx - 120):(idx + 20)], sep = "\n")
