# Proyectos ACP (Agroberries / Cerro Prieto)

Repositorio centralizado que alberga los proyectos, aplicaciones y herramientas analíticas desarrolladas para la operación agrícola y logística de **ACP**.

---

## 📁 Catálogo de Proyectos

### 🗺️ [Rutas de Cosecha](./Rutas%20de%20Cosecha)
Plataforma web interactiva desarrollada en **R Shiny** para la gestión visual de rutas de cosecha:
- **Trazado 2D sobre Lienzo Canvas**: Visualización y pintura interactiva de válvulas por día (Lunes a Sábado), turno y módulo.
- **Control de Avance Físico**: Pintado porcentual (10%, 25%, 50%, 75%, 100%), cálculo dinámico de hectáreas cosechadas y balance semanal.
- **Sincronización en la Nube**:
  - Descarga y actualización en tiempo real de datos maestros desde **Google Drive**.
  - Persistencia multi-usuario de rutas mediante Webhook en **Google Sheets (Google Apps Script)**.
  - Sincronización inmutable con cosechas oficiales de balanza de packing.
- **Despliegue**: Publicado en [shinyapps.io](https://acp-proyecciones.shinyapps.io/ruta_cosecha/).

### 📊 [Seguimiento de Proyectos](./Seguimiento%20de%20Proyectos)
Plataforma ejecutiva en **R Shiny** con diseño inspirado en **macOS y Apple iOS**:
- **Carga Multi-Formato**: Procesa exportaciones de Linear / Jira y cronogramas tradicionales de Excel Gantt.
- **Cronograma Gantt Interactivo**: Visualización temporal con Plotly, línea de hoy y mapeo de dependencias bloqueantes.
- **Métricas Gerenciales & Carga**: Donut de estados, distribución por módulo y carga de trabajo por colaborador.
- **Matriz Operativa DT**: Tabla interactiva con insignias (*badges*) de estado y exportación directa a Excel.

### 🧠 [Actualización Semanal RNA](./Actualizacion%20Semanal%20RNA)
Plataforma de ajuste semanal de cosecha basada en **Machine Learning & Redes Neuronales Artificiales (Deep Bagged MLP)**:
- **Ensemble de Redes Neuronales**: 10 redes neuronales MLP entrenadas sobre 21,400+ observaciones históricas multianuales (2021–2026).
- **Inferencia Multivariada en Tiempo Real**: Predice rendimiento continuo (kg/ha) y desvío de incertidumbre combinando avance Lunes/Martes, lags temporales ($S-1$, $S-2$), conteos florales desfasados, meteorología de Filtrado 7/9 y atributos de lote.
- **Precisión Validada en Test 2026**: $R^2 = 83.6\%$, correlación $0.9175$ y error relativo de $17.7\%$.
- **Conmutador Híbrido**: Permite alternar en tiempo real entre el motor predictivo de RNA y el árbol de decisión heurístico tradicional.

### 📈 [Análisis de Productividad](./Analisis%20de%20Productividad)
Seguimiento del calendario fenológico y curvas de productividad multiescenario (Emil Kowalski / iOS craft).


