tryCatch({
  p <- parse('D:/ACP/Actualizacion data maestra/actualizar_data_maestra.R', encoding = 'UTF-8')
  cat("PARSED SUCCESSFULLY! Expressions:", length(p), "\n")
}, error = function(e) {
  cat("PARSE ERROR:", conditionMessage(e), "\n")
})
