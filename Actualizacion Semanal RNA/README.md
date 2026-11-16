# Control Semanal de Cosecha — Modelo RNA (Agrícola Cerro Prieto)

Plataforma analítica e interactiva en **R Shiny** para el **Ajuste Semanal de Cosecha de Arándanos**, potenciada por un **Ensemble de Redes Neuronales Artificiales (Deep Bagged Multi-Layer Perceptron)** que reemplaza los árboles de decisión heurísticos rígidos por inferencia predictiva continua multivariada.

---

## 🎯 Propósito del Proyecto

Cada semana de cosecha (típicamente entre martes en la noche y miércoles temprano), el equipo de operaciones y proyecciones de ACP necesita ajustar la estimación semanal de cosecha:
- **Versión Tradicional (`Actualizacion Semanal`):** Utiliza un árbol de decisión heurístico basado en reglas condicionales rígidas (`avance > 30%`, `60% fijo para 2da pasada de Rocío`, `multiplicador lineal de tendencia floral`).
- **Versión con Red Neuronal (`Actualizacion Semanal RNA`):** Utiliza un **Ensemble de 10 Redes Neuronales Artificiales (MLP)** entrenadas con más de **21,400 observaciones históricas (2021–2026)** para aprender las interacciones no lineales entre avance real, clima, fenología desfasada y memoria temporal.

---

## 🧠 Arquitectura de la Red Neuronal (RNA)

### 1. Modelo: Ensemble Bagged Multi-Layer Perceptron (MLP)
- **10 Redes Neuronales Feedforward** entrenadas con submuestreo aleatorio (bagging del 85%) y regularización por decaimiento de pesos (*weight decay* `0.002 - 0.005`).
- **Capas Ocultas:** Alternancia de 8 y 10 neuronas por red.
- **Predicción Final:** Promedio ponderado del ensamble para el rendimiento esperado $\hat{Y}_{RNA}$ (kg/ha) y desviación estándar como métrica de **incertidumbre / confianza operativa**.

### 2. Variables de Entrada (Features Vector - 27 variables)
1. **Señales intra-semanales tempranas (Lunes y Martes):**
   - `Rend_Lun_Mar`: Rendimiento observado en los dos primeros días ($kg/ha$).
   - `Tiene_Lun_Mar`: Indicador binario de cosecha activa al inicio de la semana.
   - `Prop_Avance_Lun_Mar`: Proporción de la superficie cosechada del turno.
2. **Inercia temporal y memoria (Lags):**
   - `Rend_S_1`: Rendimiento registrado en la semana anterior ($S-1$).
   - `Rend_S_2`: Rendimiento de hace 2 semanas ($S-2$).
   - `Ratio_S1_S2`: Tasa de aceleración/desaceleración de la curva.
   - `Semana_Ant_No_Finalizo`: Flag binario de cierre incompleto en fin de semana previo.
3. **Fenología floral sincronizada (Conteo floral histórico ACP):**
   - `Carga_Flor_Lag`: Órganos florales por planta (Botón + Flor) de hace ~6 semanas (desfasado según curva de maduración).
   - `Fruto_Verde_Lag`: Conteo de frutos verdes en desarrollo de hace ~3 semanas.
4. **Meteorología (Estaciones Filtrado 7 y Filtrado 9):**
   - `Temp_Prom`: Temperatura promedio semanal (°C).
   - `Temp_Max`: Temperatura máxima semanal (°C).
   - `Radiacion`: Radiación solar media ($W/m^2$).
   - `Anomalia_Temp`: Desviación térmica vs promedio histórico semanal.
5. **Atributos agronómicos del lote:**
   - `Variedad`: One-hot encoding de las variedades principales (`ROCIO`, `VENTURA`, `BILOXI`, `MAGICA`, `SEKOYA POP`, `SEKOYA BEAUTY`, `IMPERIAL`, `MADEIRA`, `OTROS`).
   - `Modulo_Num`: Módulo físico (1 a 14).
   - `Area_Ha`: Superficie neta del turno.
   - `Plantas_Ha`: Densidad de plantas por hectárea.
   - `Es_Organico`: Indicador de manejo orgánico vs convencional.
   - `Semana`: Semana ISO (captura estacionalidad en la campaña).

---

## 📈 Desempeño y Validación en Test (Campaña 2026 Fuera de Muestra)

El modelo fue entrenado con las campañas **2021 a 2025** (18,571 observaciones) y evaluado exclusivamente sobre la campaña **2026** (2,888 observaciones independientes):

| Métrica de Evaluación | Resultado Campaña 2026 | Interpretación Operativa |
| :--- | :---: | :--- |
| **Correlación ($r$)** | **`0.9175`** | Altísima consistencia direccional entre proyección RNA y cosecha real. |
| **Coeficiente $R^2$** | **`0.8364`** | La RNA explica el **`83.6%` de la variabilidad** del rendimiento real. |
| **MAE (Error Absoluto Medio)** | **`369.76 kg/ha`** | Desviación promedio por hectárea cosechada. |
| **RMSE (Error Cuadrático)** | **`850.05 kg/ha`** | Penalización acotada en picos extremos. |
| **Error Relativo (MAE / Media)** | **`17.7%`** | Precisión operativa de nivel productivo comercial. |

---

## 🚀 Cómo Iniciar la Aplicación

### Opción 1: Doble clic en Windows
Hacer doble clic en el ejecutable por lotes:
[`iniciar_ajuste_semanal_rna.bat`](file:///d:/ACP/Actualizacion%20Semanal%20RNA/iniciar_ajuste_semanal_rna.bat)

### Opción 2: Desde R o PowerShell
```powershell
& "C:\Users\cmendozac\AppData\Local\Programs\R\R-4.6.1\bin\x64\Rscript.exe" "d:\ACP\Actualizacion Semanal RNA\run_app.R"
```
La aplicación abrirá automáticamente en tu navegador en `http://127.0.0.1:4242`.

---

## 🎛️ Modos de Ajuste en Tiempo Real

En la barra de herramientas de la pestaña **Detalle Semanal**, se cuenta con un control segmentado interactivo:
- **`🧠 Red Neuronal (RNA)` (Por defecto):** Calcula el rendimiento semanal utilizando la inferencia multivariada de las 10 redes neuronales. Muestra en la columna de motivo la insignia púrpura: `🤖 RNA: X kg/ha (Confianza: ±Y kg/ha)`.
- **`📐 Heurístico`:** Permite conmutar al árbol de reglas tradicional para contrastar y auditar la diferencia de criterio agronómico en tiempo real.

---

## 🔄 Re-entrenamiento Periódico del Modelo

Para re-entrenar la RNA cuando ingresen nuevas semanas de SAP o nuevas campañas:
```powershell
& "C:\Users\cmendozac\AppData\Local\Programs\R\R-4.6.1\bin\x64\Rscript.exe" "d:\ACP\Actualizacion Semanal RNA\entrenar_modelo_rna.R"
```
El script generará y sobrescribirá automáticamente `modelo_rna_ajuste.rds` sin necesidad de reiniciar la máquina.
