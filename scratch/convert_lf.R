f <- "Actualizacion Semanal/app.R"
txt <- readChar(f, file.info(f)$size, useBytes = TRUE)
txt_lf <- gsub("\r\n", "\n", txt, fixed = TRUE)
writeChar(txt_lf, f, eos = NULL, useBytes = TRUE)
cat("Converted to LF. Has CRLF now:", grepl("\r\n", readChar(f, 10000)), "\n")
