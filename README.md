# Impacto del Cambio Climático y Moderación por Infraestructura sobre el Rendimiento Académico en Cundinamarca (2014-2019)

Este repositorio contiene la pipeline de datos, scripts de procesamiento y modelos econométricos en datos panel para analizar el efecto causal de los choques térmicos y pluviométricos sobre el desempeño en la prueba censal Saber 11 (ICFES) en las instituciones educativas del departamento de Cundinamarca, Colombia.

---

## 📂 Estructura del Proyecto

El código está estructurado en scripts ordenados de forma secuencial y reproducible dentro de la carpeta `Scripts_bases/`:

```text
├── Scripts_bases/
│   ├── 01_procesar_infraestructura_dane.py        # Procesamiento de variables de infraestructura C600 del DANE
│   ├── 02_descargar_microdatos_historicos_dane.py # Descarga e ingesta de microdatos del catálogo ANDA-DANE (2014-2019)
│   ├── 03_construir_panel_maestro.R               # Consolidación del panel maestro (ICFES, Clima, DANE SISE)
│   ├── 04_modelo_econometrico_panel_v1.R          # Estimaciones TWFE, heterogeneidad, mecanismos y pruebas de robustez
│   └── DatosEarth.py                              # Consulta de datos satelitales mediante Google Earth Engine API
├── Bases_Procesadas/
│   └── coeficientes_modelo.png                    # Gráfico de coeficientes e intervalos de confianza (95%)
└── README.md                                      # Documentación del repositorio
```

---

## 📊 Metodología Econométrica

Se estiman especificaciones de **Efectos Fijos de Dos Vías (Two-Way Fixed Effects - TWFE)** a nivel de colegio ($i$) y año ($t$):

$$\text{Puntaje}_{it} = \beta_1 \, \text{TempEfectiva}_{it} + \beta_2 \, \text{PrecipEfectiva}_{it} + \beta_3 \, (\text{TempEfectiva}_{it} \times \text{Infraestructura}_i) + \boldsymbol{X}_{it}'\boldsymbol{\delta} + \alpha_i + \lambda_t + \varepsilon_{it}$$

### Modelos y Pruebas de Robustez Incluidas:
* **Especificación Principal:** TWFE (Colegio + Año) con controles socioeconómicos del hogar e interacciones de infraestructura física (energía) y apoyo nutricional (PAE).
* **Robustez 1:** Restricción a muestra de panel balanceado estricto (6 años continuos).
* **Robustez 2:** Heterogeneidad por naturaleza institucional (Oficial / Público vs. No Oficial / Privado).
* **Robustez 3:** Tendencias temporales específicas por colegio ($\alpha_i \times t$).
* **Robustez 4:** Efectos fijos de Municipio $\times$ Año ($\lambda_{m,t}$).
* **Robustez 5:** Prueba de Placebo / Falsación con temperatura futura ($\text{Temp}_{t+1}$).
* **Robustez 6:** Inferencia mediante *Two-Way Clustering* (Municipio y Colegio).
* **Robustez 7:** Regresión Ponderada WLS por tamaño de cohorte (`total_estudiantes`).

---

## 🤖 Declaración sobre el Uso de Inteligencia Artificial

Para fines de transparencia académica e integridad científica:

* **Formulación Teórica y Econométrica:** Toda la fundamentación teórica, la estrategia de identificación causal, las especificaciones de modelos, los supuestos de exogeneidad y la selección de pruebas de robustez fueron **formulados e ideados propiamente por los autores/investigadores del proyecto**.
* **Asistencia de IA y Supervisión:** Se utilizó asistencia de Inteligencia Artificial (AI-assisted coding) como herramienta de soporte técnico para la optimización de código, estructuración de scripts, homologación de encodados de texto y automatización de comandos. Todo el código generado por IA fue **supervisado, revisado, probado y auditado exhaustivamente por los investigadores** previo a su inclusión en el repositorio.
