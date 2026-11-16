con <- file("Actualizacion Semanal/app.R", "rb")
raw_bytes <- readBin(con, "raw", 1000)
close(con)
cat("Has CRLF:", any(raw_bytes[-length(raw_bytes)] == as.raw(0x0d) & raw_bytes[-1] == as.raw(0x0a)), "\n")
