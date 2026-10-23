# ==============================================================================
# SCRIPT: LEER EXCEL Y GUARDAR EN UN SOLO ARCHIVO RDATA
# ==============================================================================

# 1. Instalar y cargar librería para leer Excel
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

# 2. Definir las carpetas y los nombres de los archivos
ruta_conteo  <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Conteo"
ruta_etapas  <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Etapas Fenologicas"
ruta_cosecha <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Histórico de Cosecha"
ruta_censo   <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Data Histórica/Censo"

# 3. Función auxiliar para encontrar y leer el archivo (sea .xlsx o .xls)
cargar_excel <- function(directorio, nombre_base) {
  # Busca el archivo que coincida con el nombre y tenga extensión .xlsx o .xls
  archivo <- list.files(
    path = directorio, 
    pattern = paste0("^", nombre_base, ".*\\.(xlsx|xls)$"), 
    full.names = TRUE, 
    ignore.case = TRUE
  )
  
  if (length(archivo) == 0) {
    stop(paste("No se encontró el archivo:", nombre_base, "en la ruta:", directorio))
  }
  
  message("Leyendo archivo: ", basename(archivo[1]))
  return(read_excel(archivo[1]))
}

# 4. Lectura de las cuatro bases de datos
df_conteo       <- cargar_excel(ruta_conteo, "historico_conteo")
df_etapas       <- cargar_excel(ruta_etapas, "Historico_Etapas_Fenologicas")
df_cosecha      <- cargar_excel(ruta_cosecha, "BI_Cosecha2")
df_tratamientos <- cargar_excel(ruta_censo, "Historico_Tratamientos")



# 5. Guardar en un único archivo .RData
# (Puedes cambiar la ruta y nombre de salida donde prefieras guardarlo)
ruta_salida_rdata <- "D:/datos_historicos_arandano.RData"

save(df_conteo, df_etapas, df_cosecha, df_tratamientos, file = ruta_salida_rdata)
message("✓ Archivo guardado exitosamente en: ", ruta_salida_rdata)