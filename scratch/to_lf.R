content <- readBin("Actualizacion Semanal/app.R", "raw", file.info("Actualizacion Semanal/app.R")$size)
# Replace \r\n with \n
# as.raw(0x0d) is \r, as.raw(0x0a) is \n
text <- rawToChar(content)
text_lf <- gsub("\r\n", "\n", text, fixed = TRUE)
writeChar(text_lf, "Actualizacion Semanal/app.R", eos = NULL)
cat("Normalized to LF. File size:", file.info("Actualizacion Semanal/app.R")$size, "\n")
