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
