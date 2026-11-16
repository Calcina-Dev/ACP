suppressPackageStartupMessages({
  library(dplyr)
})

# Let's inspect some turns without harvest in S46
# We can load the projection file or the simulation
ruta_base <- "D:/ACP"
# Let's see what rds or data files exist
rds_files <- list.files("D:/ACP", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
cat("RDS files found:\n")
for (f in head(rds_files, 15)) cat(f, "\n")
