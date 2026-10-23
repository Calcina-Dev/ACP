@echo off
title Sistema Gerencial de Control y Seguimiento de Proyectos - ACP
echo =====================================================================
echo  Iniciando Aplicacion Shiny: Control y Seguimiento de Proyectos ACP
echo  Lineamientos de diseno: macOS / iOS Human Interface Guidelines
echo =====================================================================
echo.
"C:\Users\cmendozac\AppData\Local\Programs\R\R-4.6.1\bin\Rscript.exe" -e "shiny::runApp('D:/ACP/Seguimiento de Proyectos', launch.browser = TRUE)"
pause
