tryCatch({
  p <- parse('D:/ACP/Actualizacion Semanal/app.R.bak_sector_m12', encoding = 'UTF-8')
  cat("BAK FILE PARSES PERFECTLY! Expressions:", length(p), "\n")
}, error = function(e) {
  cat("PARSE ERROR:", conditionMessage(e), "\n")
})
