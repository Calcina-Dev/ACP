con <- file('Actualizacion Semanal/app.R', 'rb')
bin <- readBin(con, 'raw', 1000)
close(con)
cat('Has CRLF:', any(bin[-length(bin)] == 0x0d & bin[-1] == 0x0a), '\n')
