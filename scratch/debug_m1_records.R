lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", encoding = "UTF-8")
start_idx <- grep("ejecutar_ajuste_semana <- function", lines)[1]
end_idx   <- grep("ejecutar_deshacer_ajuste <- function", lines)[1] - 1

fn_lines <- lines[start_idx:end_idx]

# Let's inspect how valvulas_calc is formed
idx_vc <- grep("valvulas_calc <-", fn_lines)
cat("Lines with valvulas_calc:", idx_vc, "\n")
for(i in idx_vc) cat(fn_lines[i], "\n")
