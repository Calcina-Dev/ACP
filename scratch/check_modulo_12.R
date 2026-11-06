# Script to check where Módulo 12 is in different data files
files <- list.files("D:/ACP/Actualizacion Semanal", pattern = "\\.(rds|RData|xlsx|csv)$", full.names = TRUE)
cat("Checking Módulo 12 across data files in D:/ACP/Actualizacion Semanal...\n")

# Check if there is an .RData or .rds workspace or data files
for (f in files) {
  if (grepl("valvulas|areas|proyeccion|cosecha|ediciones", f, ignore.case = TRUE)) {
    cat("File:", basename(f), "\n")
  }
}
