# 📊 Sistema Gerencial de Control y Seguimiento de Proyectos (ACP)

Plataforma analítica e interactiva desarrollada en **R Shiny** para la gestión ejecutiva, seguimiento de tareas, cronogramas Gantt y control de dependencias para los proyectos de **Agrícola Cerro Prieto (ACP)**.

---

## 🎨 Lineamientos de Diseño (macOS & Apple iOS)

La aplicación sigue fielmente la filosofía de diseño **Human Interface Guidelines** de Apple (macOS e iOS) y los estándares visuales corporativos implementados en los demás proyectos de ACP:
- **Fondo del Sistema**: `#F2F2F7` (Secondary System Background).
- **Header Frosted Glass**: Barra superior con efecto traslúcido y desenfoque (`backdrop-filter: blur(20px)`), isotipo de ACP y tipografía de alta jerarquía.
- **Tarjetas y Contenedores**: Tarjetas blancas flotantes con bordes sutiles (`border-radius: 18px`, `border: 1px solid rgba(0, 0, 0, 0.06)`).
- **Segmented Control Tabs**: Pestañas estilo píldora con transiciones suaves para alternar entre vistas.
- **Paleta de Color Semántica iOS**:
  - `Done / Completado`: `#34C759` (Verde Apple)
  - `En Progreso`: `#007AFF` (Azul Sistema)
  - `Revisión / Validación`: `#FF9500` (Naranja)
  - `Backlog / Pendiente`: `#8E8E93` (Gris Sistema)
  - `Bloqueos / Riesgos`: `#FF3B30` (Rojo)
- **Elegancia Gerencial sin excesos**: Tipografía `-apple-system / Inter`, indicadores numéricos tabulares y visualizaciones Plotly optimizadas sin saturación de elementos.

---

## 🚀 Funcionalidades Principales

1. **Subida y Procesamiento Inteligente de Archivos**:
   - Soporte para archivos `.xlsx`, `.xls` y `.csv`.
   - **Detección automática de esquemas**:
     - Exportaciones ágiles modernas (Linear, Plane, Jira): columnas `Identifier`, `Name`, `State Name`, `Start Date`, `Due Date`, `Assignees`, `Module Name`, `Relations`.
     - Matrices tradicionales de Cronograma Excel (Gantt): columnas `ID`, `Tarea`, `Fecha Inicio`, `Fecha Final`, `Responsable`.
   - Incluye archivo de muestra precargado (`datos_muestra.xlsx`).

2. **Indicadores Clave de Desempeño (KPIs)**:
   - Total de tareas registradas.
   - % de avance global ponderado con barra de progreso interactiva.
   - Conteo de tareas completadas (Done) y pendientes.
   - Alerta temprana de tareas con dependencias bloqueantes (`blocked_by`).
   - Número de colaboradores / responsables activos.

3. **Cronograma Gantt Interactivo (Plotly)**:
   - Barras temporales continuas por tarea con escala cronológica real.
   - Selector dinámico para colorear por **Estado**, **Módulo** o **Responsable**.
   - Línea indicadora del día de hoy en tiempo real.
   - Tarjetas emergentes (*tooltips*) con detalle de fechas, duración y dependencias.

4. **Métricas de Gestión y Carga de Trabajo**:
   - Donut chart con distribución porcentual de estados.
   - Gráfico de barras apiladas con la carga de tareas por responsable.
   - Distribución de tareas por módulo o iniciativa.
   - Cronograma de hitos y entregas acumuladas en el tiempo.

5. **Matriz Detallada de Tareas (DT)**:
   - Píldoras de estado (*badges*) de estilo iOS.
   - Búsqueda en vivo, ordenamiento por columnas y paginación rápida.
   - Botón directo para **Exportar a Excel (.xlsx)** el subconjunto de datos filtrado.

---

## 📂 Estructura del Directorio

```text
D:\ACP\Seguimiento de Proyectos\
├── app.R                       # Código fuente unificado Shiny (UI + Servidor)
├── datos_muestra.xlsx          # Archivo de datos de proyectos precargado
├── gantt_formalizado_muestra.xlsx # Muestra en formato cronograma tradicional
├── ejecutar_app.bat            # Script de lanzamiento con 1 clic en navegador
├── README.md                   # Documentación técnica y manual de uso
└── www\
    └── logo_cerro_prieto.png   # Isotipo oficial Agrícola Cerro Prieto
```

---

## 💻 Instrucciones de Ejecución

### Opción 1: Lanzador rápido (Recomendado)
Haz doble clic sobre el archivo:
```text
D:\ACP\Seguimiento de Proyectos\ejecutar_app.bat
```

### Opción 2: Desde RStudio o Consola R
```r
shiny::runApp("D:/ACP/Seguimiento de Proyectos", launch.browser = TRUE)
```
