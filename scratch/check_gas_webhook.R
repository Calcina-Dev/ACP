lines_gas <- readLines("D:/ACP/Rutas de Cosecha/google_apps_script_webhook.js", warn = FALSE, encoding = "UTF-8")
cat("Lines in google_apps_script_webhook.js:", length(lines_gas), "\n")
cat(paste(head(lines_gas, 35), collapse = "\n"))
