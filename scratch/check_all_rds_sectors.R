suppressPackageStartupMessages(library(dplyr))

rds_files <- list.files("D:/ACP", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)

for (rf in rds_files) {
  if (grepl("renv|\\.git", rf)) next
  res <- tryCatch({
    obj <- readRDS(rf)
    if (is.data.frame(obj) && "Módulo" %in% names(obj) && "Sector" %in% names(obj)) {
      sub_obj <- obj %>% filter(as.character(Módulo) %in% c("9", "11", "12"))
      if (nrow(sub_obj) > 0) {
        tb <- table(as.character(sub_obj$Módulo), as.character(sub_obj$Sector))
        cat("File:", rf, "\n")
        print(tb)
        cat("----------------------------------\n")
      }
    }
  }, error = function(e) NULL)
}
