lines <- readLines("D:/ACP/Actualizacion Semanal/app.R", warn = FALSE, encoding = "latin1")
cat("Total lines:", length(lines), "\n")
res <- tryCatch({
  parse(text = lines)
  cat("PARSED FULL TEXT SUCCESSFULLY!\n")
}, error = function(e) {
  cat("Error parsing full text:", conditionMessage(e), "\n")
  # find which chunk fails
  step <- 500
  for (i in seq(1, length(lines), by = step)) {
    end_i <- min(i + step - 1, length(lines))
    chunk <- lines[i:end_i]
    # wrap in dummy function to test syntax
    dummy <- paste(c("foo <- function() {", chunk, "}"), collapse = "\n")
    tryCatch(parse(text = dummy), error = function(err) {
      cat(sprintf("Chunk %d..%d failed: %s\n", i, end_i, conditionMessage(err)))
    })
  }
})
