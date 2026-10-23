# Rutas de Cosecha - Mapa 2D Interactivo (ACP)

Aplicación web desarrollada en **R Shiny** para la planificación, distribución y seguimiento de rutas de cosecha en el fundo de arándano de Cerro Prieto.

---

## 🚀 Características Principales

1. **Lienzo Gráfico Canvas 2D**:
   - Geometría precisa de cada válvula (incluyendo parcelas diagonales y polígonos complejos).
   - Pintado interactivo por días de la semana y por porcentaje (10%, 25%, 50%, 75%, 100%).
   - Soporte para válvulas multivariedad (particiones proporcionales de hectáreas).

2. **Control Semanal y KPIs en Tiempo Real**:
   - Resumen dinámico de avance físico (ha) y kilos proyectados vs cosechados.
   - Filtros dinámicos por Grupo de Módulos, Módulo, Turno, Válvula y Variedad.
   - Comparación entre modos: **Proyección**, **Cosecha Campo** y **Cosecha Oficial** (Balanza Packing).

3. **Arquitectura Cloud y Alta Disponibilidad**:
   - Descarga dinámica en tiempo real de `datos_maestros_cosecha_2026.RData` desde **Google Drive**.
   - Persistencia de rutas programadas y control de concurrencia multi-usuario mediante **Google Sheets Webhook (Apps Script)**.
   - Despliegue automatizado en **shinyapps.io**.

---

## 🛠️ Tecnologías

- **Lenguaje**: R (>= 4.3)
- **Framework Web**: Shiny, Shinyjs, HTML5 Canvas
- **Librerías R**: `dplyr`, `tidyr`, `lubridate`, `DT`, `jsonlite`, `httr`, `rsconnect`
- **Integraciones**: Google Drive API, Google Apps Script Webhooks

---

## 📦 Despliegue en shinyapps.io

Para publicar actualizaciones a producción:
```r
source("desplegar_shinyapps.R")
```
URL en vivo: [https://acp-proyecciones.shinyapps.io/ruta_cosecha/](https://acp-proyecciones.shinyapps.io/ruta_cosecha/)
