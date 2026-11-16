# Investigate projection difference for Semana 46
suppressMessages(library(dplyr))
rdata_path <- "Actualizacion Semanal/datos_maestros_cosecha_2026.RData"
e <- new.env()
load(rdata_path, envir = e)

df_p <- e$df_proyecciones
cat("Names of df_proyecciones:\n")
print(names(df_p))

p46 <- df_p %>% filter(Semana == 46)
cat("Rows in Semana 46:", nrow(p46), "\n")
print(head(p46))

# Let's see the sum of the projection column
col_proy <- names(p46)[grep("proy", names(p46), ignore.case = TRUE)]
cat("Projection columns:", col_proy, "\n")
for (cp in col_proy) {
  cat(cp, "total sum:", sum(p46[[cp]], na.rm = TRUE), "\n")
}

# Also let's check by Sector, Modulo, Tipo
cat("\nBreakdown by Sector:\n")
p46_sec <- p46 %>% group_by(Sector_join) %>% summarise(Kg = sum(.data[[col_proy[1]]], na.rm = TRUE), n = n())
print(p46_sec)

# Check if there is Tipo or Testblock
if ("Tipo" %in% names(p46)) {
  cat("\nBreakdown by Tipo:\n")
  print(p46 %>% group_by(Tipo) %>% summarise(Kg = sum(.data[[col_proy[1]]], na.rm = TRUE)))
}

# Check if there is Sector 4
cat("\nSector 4 rows:\n")
print(p46 %>% filter(Sector_join %in% c("4", "04", "S04", "S4")))

cat("\nCheck other weeks sums:\n")
print(df_p %>% group_by(Semana) %>% summarise(Total_Kg = sum(.data[[col_proy[1]]], na.rm = TRUE)) %>% print(n = 30))
