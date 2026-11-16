# Check how u_base has Ha Real 2ª Pas
lines <- readLines("Actualizacion Semanal RNA/app.R", warn = FALSE)
cat(grep("u_base <- universo_base", lines, value = TRUE), "\n")
idx <- grep("u_base <- universo_base", lines)
cat("Lines around u_base (L", idx, "):\n")
cat(lines[(idx-5):(idx+25)], sep = "\n")
