lines <- readLines("Actualizacion Semanal/app.R", encoding = "UTF-8")
idx_start <- grep('tabPanel\\("Frecuencia"', lines)
cat("Found tabPanel Frecuencia at line:", idx_start, "\n")
idx_prev <- idx_start - 1
cat("Line prev:", lines[idx_prev], "\n")
# find where tabPanel ends
idx_end <- idx_start + 38
cat("Line end:", lines[idx_end], "\n")
cat("Line after end:", lines[idx_end + 1], "\n")

# Update prev line to not have trailing comma
lines[idx_prev] <- "            )"
# Remove tabPanel("Frecuencia", ...)
lines <- lines[-c(idx_start:idx_end)]
writeLines(lines, "Actualizacion Semanal/app.R", useBytes = TRUE)
cat("Successfully removed tabPanel Frecuencia!\n")
