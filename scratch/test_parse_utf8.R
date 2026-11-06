tryCatch({
  lines <- readLines('D:/ACP/Actualizacion Semanal/app.R', encoding = 'UTF-8', warn = FALSE)
  p <- parse(text = lines)
  cat("PARSED SUCCESSFULLY! Expressions:", length(p), "\n")
}, error = function(e) {
  cat("PARSE ERROR:", conditionMessage(e), "\n")
})
