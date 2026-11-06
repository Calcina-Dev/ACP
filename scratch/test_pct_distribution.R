suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

ha_pasada <- 6.40
nuevo_kgha <- 801.74
tot_kg <- round(ha_pasada * nuevo_kgha, 2)

dias <- c("Jueves", "Viernes")
pcts <- c(70, 30)

props <- pcts / sum(pcts)

kg_dist <- c()
ha_dist <- c()
kg_acum <- 0
ha_acum <- 0

for (i in seq_along(dias)) {
  if (i == length(dias)) {
    kg_dist[i] <- max(0, round(tot_kg - kg_acum, 2))
    ha_dist[i] <- max(0, round(ha_pasada - ha_acum, 2))
  } else {
    kg_dist[i] <- round(tot_kg * props[i], 2)
    ha_dist[i] <- round(ha_pasada * props[i], 2)
    kg_acum <- kg_acum + kg_dist[i]
    ha_acum <- ha_acum + ha_dist[i]
  }
}

cat(sprintf("Total Kg: %.2f (esperado: %.2f)\n", sum(kg_dist), tot_kg))
cat(sprintf("Total Ha: %.2f (esperado: %.2f)\n", sum(ha_dist), ha_pasada))
for (i in seq_along(dias)) {
  cat(sprintf("  %s: %.1f%% => %.2f kg en %.2f ha (Rend: %.2f kg/ha)\n", 
              dias[i], pcts[i], kg_dist[i], ha_dist[i], kg_dist[i] / ha_dist[i]))
}
