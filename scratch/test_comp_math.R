tot_aj <- 12200
tot_real <- 21392

dif_neta <- round(tot_real - tot_aj, 0)
pct_desvio <- if (tot_aj > 0) round((dif_neta / tot_aj) * 100, 1) else 0

cat("tot_aj (X):", tot_aj, "\n")
cat("tot_real (Y):", tot_real, "\n")
cat("dif_neta (Y - X):", dif_neta, "\n")
cat("pct_desvio:", pct_desvio, "%\n")

stopifnot(dif_neta == 9192)
stopifnot(pct_desvio == 75.3)
cat("\n>>> COMPARATIVO TEST PASSED! <<<\n")
