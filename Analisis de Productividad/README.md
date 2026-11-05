# Análisis de Productividad — Agrícola Cerro Prieto (ACP)

Aplicación interactiva y reactiva en tiempo real para el seguimiento del calendario fenológico y curvas de productividad en cultivo de arándanos, diseñada bajo los principios y lineamientos de **Emil Kowalski** y diseño tipo **iOS** (diseño artesanal, controles táctiles, micro-interacciones, tipografía precisa, bordes sutiles y usabilidad sin fricción).

---

## 🎯 Estructura y Flujo de la Aplicación

1. **Identidad Visual & Header**:
   - Incorpora el logo oficial de **Agrícola Cerro Prieto**.
   - Barra superior limpia con acciones de rendimiento (*Deshacer*, *Restablecer Originales*, *Exportar Excel*).

2. **Filtros en Formato iOS & Alineación Precisa**:
   - Selectores (`Módulo`, `Breeder`, `Variedad`) rediseñados con estética **iOS**: esquinas redondeadas (`border-radius: 11px`), fondo suave `#F8FAFC`, chevron minimalista en SVG, borde sutil y foco sin resplandor invasivo.
   - Alineación horizontal y vertical perfecta: todos los selectores, los botones **Buscar** y **Limpiar**, y el indicador de volumen comparten la misma altura exacta (`40px`) y descansan sobre la misma línea base.
   - **Reactividad en cascada**: Al cambiar el Breeder, el menú de variedades se actualiza de inmediato para mostrar solo las variedades de ese breeder.

3. **Carga Inicial Ultrarrápida (Inicio en Blanco)**:
   - La aplicación inicia en blanco (sin ejecutar el renderizado pesado de matrices ni curvas en frío), mostrando una tarjeta de espera elegante con atajos rápidos de un clic.
   - Al hacer clic en **Buscar**, se procesan y despliegan los resultados al instante.

4. **Calendario Fenológico Histórico (PRIMERO)**:
   - Visualización de las 56 semanas calendario (Año 0 Dic 49-52 y Año 1 Ene a Dic).
   - Selector de tamaño de celda: Compacto, Estándar o Amplio en control segmentado iOS.
   - Barra de leyenda interactiva con contadores por etapa.

5. **Curvas de Productividad & Tabla de Campañas (SEGUNDO)**:
   - **Banner**: Notificación limpia con la distribución semanal de kilos (sin la palabra *"Excel"*).
   - **Toolbar iOS**:
     - Control segmentado tipo iOS para el **Tipo de Año**: `Año neutral` | `Año caliente` (sin emojis de IA).
     - Control segmentado tipo iOS para la **Métrica**: `Kg Semanales` | `Kg Acumulados`.
     - **Menú de 3 Puntos (`⋯ Comparar`) con Switches iOS**:
       - Abre un popover flotante con efecto *glassmorphism* (blur) y switches tipo iOS.
       - **Inicialmente solo está cargado `ESC. CAMPO`**.
       - El usuario puede activar/desactivar `Presupuesto`, `ESC. PROY.`, `ESC. CAMPO MODIF`, `Wish List 27/28` y `Mostrar Picos` mediante switches individuales.
       - La píldora del botón muestra el número de escenarios activos en tiempo real.
   - **Tabla de Campañas**:
     - Módulo y Variedad en filas.
     - Columnas: Campañas históricas (`22/23`, `23/24`, `24/25`, `25/26`, `26/27`) y campaña proyectada **`27/28`**.
     - **Solo editable la campaña 7 (`27/28`)**: Al modificar el rendimiento, los kilos totales se recalculan automáticamente con las plantas fijas y la curva se actualiza en tiempo real.

---

## 🚀 Cómo Iniciar la Aplicación

### Opción 1: Doble clic en Windows
Hacer doble clic en:
[`iniciar_analisis_productividad.bat`](file:///d:/ACP/Analisis%20de%20Productividad/iniciar_analisis_productividad.bat)

### Opción 2: Desde R / PowerShell
```powershell
& "C:\Users\cmendozac\AppData\Local\Programs\R\R-4.6.1\bin\x64\Rscript.exe" "d:\ACP\Analisis de Productividad\run_app.R"
```
