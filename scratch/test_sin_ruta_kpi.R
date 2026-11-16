# test_sin_ruta_kpi.R
source("D:/ACP/Actualizacion Semanal/app.R", local = TRUE)

sem_sel <- 46
camp_sel <- "2026"

# Test df_concil via obtener_cosecha_conciliada_semana
df_c <- obtener_cosecha_conciliada_semana(sem_sel, camp_sel)
cat(sprintf("Filas df_c: %d\n", nrow(df_c)))

# Check Estado_Concil
sr_rows <- df_c %>% filter(Estado_Concil == "Kilos SAP sin Ruta")
cat(sprintf("Filas Kilos SAP sin Ruta: %d\n", nrow(sr_rows)))
kg_sr <- round(sum(coalesce(sr_rows$Kg_Sin_Ruta, sr_rows$Kg_Total_sap), na.rm = TRUE), 1)
cat(sprintf("Kg SAP sin Ruta: %f\n", kg_sr))

# Check df_cosecha_testblock
df_tb <- df_cosecha_testblock_ini %>% filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel)
kg_tb <- sum(coalesce(df_tb$`Kg Total`, 0), na.rm = TRUE)
cat(sprintf("Kg Testblock: %f\n", kg_tb))

# Check comercial
df_com <- df_cosecha_inicial %>% filter(as.character(Campaña) == camp_sel, as.numeric(Semana) == sem_sel)
kg_com <- sum(coalesce(df_com$`Kg Total`, 0), na.rm = TRUE)
cat(sprintf("Kg Comercial: %f\n", kg_com))

kg_tot <- kg_com + kg_tb + kg_sr
cat(sprintf("Total Cosecha Real (Comercial + TB + Sin Ruta): %f (formateado: %s)\n", kg_tot, format(round(kg_tot, 1), big.mark = ",")))
