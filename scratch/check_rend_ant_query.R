suppressPackageStartupMessages({
  library(dplyr)
})

# Load the valvulas calculation or check the turns in S46
# Let's inspect data from Actualizacion Semanal
app_lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "UTF-8")

# Let's see how proy_turno and rend_ant_turno are loaded in app.R
cat("Searching for rend_ant_turno query in app.R:\n")
idx_ant <- grep("rend_ant_turno <-", app_lines)
for (i in idx_ant) {
  cat(paste(app_lines[i:(i+25)], collapse = "\n"), "\n---\n")
}
