lines_new <- readLines('D:/ACP/Actualizacion Semanal/app.R', warn=FALSE, encoding='UTF-8')
lines_bak <- readLines('D:/ACP/Actualizacion Semanal/app.R.bak_mover_20261111', warn=FALSE, encoding='UTF-8')

cat('New lines count:', length(lines_new), '\n')
cat('Bak lines count:', length(lines_bak), '\n')
cat('Diff line count:', length(lines_new) - length(lines_bak), '\n')

# Check which sections differ
diff_idx <- which(lines_new[1:min(length(lines_new), length(lines_bak))] != lines_bak[1:min(length(lines_new), length(lines_bak))])
cat('First difference at line:', if(length(diff_idx) > 0) diff_idx[1] else 'None', '\n')
