library(readxl)
library(dplyr)
library(lubridate)

path_meteo <- "d:/ACP/Variables_Filtrados_7_9.xlsx"
df7_all <- read_excel(path_meteo, sheet = "Variables_Meteo_Filtrado7")
cat("Variables in Filtrado 7:\n")
print(table(as.character(df7_all[["Variable"]])))

cat("\nAños in Filtrado 7:\n")
print(table(as.character(df7_all[["Año"]])))
