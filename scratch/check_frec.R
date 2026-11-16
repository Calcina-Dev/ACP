frec_file <- "Actualizacion Semanal/frecuencias_modulo_variedad_cosecha.rds"
df <- readRDS(frec_file)
print(head(df))
print(df[grepl("ROCIO", df$Variedad, ignore.case=TRUE), ])
