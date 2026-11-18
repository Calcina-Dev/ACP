app_file <- "Actualizacion Semanal/app.R"
lines <- readLines(app_file, warn = FALSE, encoding = "UTF-8")

cat(lines[6705:6735], sep = "\n")
cat("...\n")
cat(lines[6780:6840], sep = "\n")
cat("...\n")
cat(lines[6890:6912], sep = "\n")
