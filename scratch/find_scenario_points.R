lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8", warn = FALSE)

# 1. Location of guardar_ajuste_valvulas_disco
l_valvs <- grep("guardar_ajuste_valvulas_disco", lines)
cat("guardar_ajuste_valvulas_disco around line:", l_valvs, "\n")

# 2. Location of btn_borrar_manuales
l_borrar <- grep("btn_borrar_manuales", lines)
cat("btn_borrar_manuales at line:", l_borrar, "\n")

# 3. Location of Tab Comparativo
l_comp <- grep('tabPanel\\("Comparativo"', lines)
cat("tabPanel Comparativo at line:", l_comp, "\n")

# 4. Location of PESTAÑA: COMPARATIVO in server
l_srv_comp <- grep("PESTAÑA: COMPARATIVO", lines)
cat("PESTAÑA: COMPARATIVO in server at line:", l_srv_comp, "\n")
