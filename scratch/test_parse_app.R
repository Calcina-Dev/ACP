tryCatch({
  p <- parse('D:/ACP/Actualizacion Semanal/app.R', encoding = 'UTF-8')
  cat("APP.R PARSES PERFECTLY! Expressions:", length(p), "\n")
}, error = function(e) {
  cat("PARSE ERROR:", conditionMessage(e), "\n")
})
