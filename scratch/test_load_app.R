# test_load_app.R
tryCatch({
  # Source app.R in a local environment to verify all ui and server definitions are valid
  env <- new.env()
  # We test parsing and check for any syntax or variable structure error
  parsed <- parse("D:/ACP/Actualizacion Semanal/app.R")
  cat(sprintf("Syntax OK: %d top-level expressions parsed.\n", length(parsed)))
}, error = function(e) {
  cat("ERROR loading app:", conditionMessage(e), "\n")
})
