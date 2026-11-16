tryCatch({
  load("D:/ACP/Actualizacion Semanal/datos_maestros_cosecha_2026.RData")
  cat("Loaded OK!\n")
  cat("Objects in RData:\n")
  print(ls())
}, error = function(e) {
  cat("Error loading:", conditionMessage(e), "\n")
})
