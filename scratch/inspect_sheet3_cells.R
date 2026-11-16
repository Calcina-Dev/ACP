# Unzip to scratch/xlsx_unzip
zip_path <- "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano/Campaña 26-27/Proyección Semanal/Proyección Semanal Detallada/26. Proyecciones Semana 46 + 5 semanas ACP (Conteo S44)_V2.xlsx"
out_dir <- "scratch/xlsx_unzip"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

unzip(zip_path, exdir = out_dir)

sh3_file <- file.path(out_dir, "xl", "worksheets", "sheet3.xml")
cat("sh3_file exists:", file.exists(sh3_file), "size:", file.size(sh3_file), "\n")

sh3_str <- paste(readLines(sh3_file, warn = FALSE), collapse = "")

for (r in c(5, 6, 7, 9, 10, 11, 13, 14, 15, 18, 20, 21, 22, 23, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 76)) {
  pat <- sprintf('<c r="AF%d"[^>]*>(.*?)</c>', r)
  m <- regmatches(sh3_str, regexec(pat, sh3_str))[[1]]
  if (length(m) >= 2) {
    cat(sprintf("Cell AF%02d: %s\n", r, m[2]))
  } else {
    pat2 <- sprintf('<c r="AF%d"[^>]*/>', r)
    m2 <- regmatches(sh3_str, regexec(pat2, sh3_str))[[1]]
    if (length(m2) > 0) cat(sprintf("Cell AF%02d: [EMPTY / SELF-CLOSING]\n", r))
    else cat(sprintf("Cell AF%02d: NOT FOUND\n", r))
  }
}
