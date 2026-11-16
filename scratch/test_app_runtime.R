suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})

# Test scenario loading function from app.R context
source("D:/ACP/Actualizacion Semanal/app.R", local = new.env())
cat("app.R sourced into environment with 0 errors!\n")
