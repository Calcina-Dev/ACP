# Find projection Excel files
paths_to_check <- c(
  "G:/Unidades compartidas/Compartido Proyecciones/3.- Arándano",
  "G:/.shortcut-targets-by-id/16xQsmkQ1cbL8vd_Xf_o619r0T7zpPcv-/02_OPERACIONES/08_PROGRAMA SEMANAL COSECHA/ARÁNDANO",
  "G:/My Drive",
  "G:/Mi unidad"
)

for (p in paths_to_check) {
  cat("Checking path:", p, "Exists:", dir.exists(p), "\n")
}

# Also let's check local drives or workspace
cat("\nSearching local files for Semana 46 Excel:\n")
local_files <- list.files(".", pattern = "46.*\\.(xlsx|xls)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
print(local_files)

# Search all xlsx in D:/ACP
cat("\nSearching D:/ACP for Semana 46 files:\n")
all_files <- list.files("D:/ACP", pattern = "46.*\\.(xlsx|xls)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
print(all_files)
