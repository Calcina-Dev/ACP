lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8")
idx_ub <- grep("universo_base <- reactive", lines)[1]
idx_end <- grep("universo_completo <- reactive", lines)[1] - 1
cat("universo_base is lines:", idx_ub, "to", idx_end, "\n")
