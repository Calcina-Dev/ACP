# Unzip xlsx to check formulas of Resumen Proyección
zip_path <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal/Proyección Semanal Detallada/26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx"
tmp_dir <- file.path(tempdir(), "xlsx_inspect")
dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

unzip(zip_path, exdir = tmp_dir)

# Read workbook.xml to see sheet names and ids
wb_xml <- readLines(file.path(tmp_dir, "xl", "workbook.xml"), warn = FALSE)
cat("workbook.xml:\n")
cat(paste(wb_xml, collapse = "\n"), "\n")

# Read sheet relationships
rels_xml <- readLines(file.path(tmp_dir, "xl", "_rels", "workbook.xml.rels"), warn = FALSE)
cat("workbook.xml.rels:\n")
cat(paste(rels_xml, collapse = "\n"), "\n")
