# Análisis de la ocupación por sexo — EPH (2022–2026)

Este proyecto analiza la evolución de la ocupación de mujeres y varones en cinco aglomerados de Buenos Aires, utilizando los microdatos de la **Encuesta Permanente de Hogares (EPH)** del INDEC.

Se consideran los **primeros trimestres de cada año, entre 2022 y 2026**, para los siguientes aglomerados: Gran La Plata, Bahía Blanca-Cerri, Ciudad de Buenos Aires, Partidos del GBA y Mar del Plata-Batán.

## ¿Qué analizamos?

Se clasifica a la población ocupada en tres categorías:

- **Una ocupación:** personas que declaran tener un solo empleo.
- **Pluriempleo:** personas que declaran tener más de una ocupación.
- **Sin clasificar:** personas ocupadas cuya cantidad de ocupaciones no puede determinarse.

Se comparan los porcentajes por sexo, su evolución anual y el aporte de cada aglomerado.

## ¿Cómo se calculan los indicadores?

Se utiliza el ponderador `PONDERA` de la EPH para estimar la población representada por las personas encuestadas.

**Tasa por categoría (%):**

Población estimada de la categoría / Total de población ocupada estimada del mismo sexo × 100.

**Variación anual (puntos porcentuales):**

Tasa del año actual − Tasa del año anterior.

**Aporte de un aglomerado (puntos porcentuales):**

Población estimada de una categoría en el aglomerado / Total de población ocupada estimada del mismo sexo en los cinco aglomerados × 100.

La suma de los aportes territoriales equivale a la tasa general de esa categoría para el sexo y año correspondientes.

## ¿Cómo ejecutar el proyecto?

1. Descargar o clonar este repositorio y abrir su carpeta en RStudio o VS Code.
2. Ejecutar los scripts en este orden:

```r
source("01_preparacion_datos.R")
source("02_visualizaciones.R")
```

El primer script instala los paquetes necesarios, descarga los microdatos de la EPH, procesa la información y genera las tablas.

El segundo utiliza esas tablas para crear los gráficos de composición ocupacional, evolución por sexo y aportes territoriales.

Los resultados se guardan automáticamente en la carpeta `salidas/`, incluyendo tablas Excel, archivos RDS y visualizaciones.

Para volver a descargar los microdatos, configurar `actualizar_microdatos <- TRUE` en el primer script.

## Fuente

**INDEC — Encuesta Permanente de Hogares (EPH).** Microdatos individuales correspondientes al primer trimestre de 2022, 2023, 2024, 2025 y 2026.

Los resultados son estimaciones basadas en una encuesta y no corresponden a un censo poblacional.
