# inspect_rocio_mod12.R
source("D:/ACP/Actualizacion Semanal/app.R", local = TRUE)

# Check frequencies
combos <- cargar_frecuencias_disco()
print(combos %>% filter(as.character(Módulo) == "12"))

# Check pct_rocio
pcts <- cargar_pct_rocio_disco()
print(pcts %>% filter(as.character(Módulo) == "12"))

# Check valvulas_calc for Modulo 12
sem_sel <- 46
camp_sel <- "2026"
df_valvs <- df_valvulas %>% filter(as.character(Módulo) == "12")
cat(sprintf("Valvulas en M12: %d\n", nrow(df_valvs)))
print(df_valvs %>% group_by(Turno, Variedad) %>% summarise(Area = sum(Area_ha), n = n()))
