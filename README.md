# Pluriempleo EPH — Buenos Aires (2022–2026)

Análisis reproducible del pluriempleo de mujeres y varones en cinco aglomerados urbanos: Gran La Plata, Bahía Blanca-Cerri, CABA, Partidos del GBA y Mar del Plata-Batán.

## Requisitos
- R instalado (recomendado R 4.4 o superior).
- Conexión a Internet en la primera ejecución, para descargar paquetes y microdatos del INDEC.
- Windows, macOS o Linux. Puede ejecutarse desde VS Code, RStudio o terminal.

## Ejecutar
1. Descargar o clonar el repositorio.
2. Abrir `01_preparacion_datos.R` en R o VS Code y ejecutarlo con `source("01_preparacion_datos.R")` desde la carpeta del repositorio, **o** ejecutar en una terminal situada en la carpeta del repositorio:
   ```bash
   Rscript 01_preparacion_datos.R
   ```
3. El script instala paquetes CRAN necesarios si faltan, descarga los primeros trimestres de 2022–2026 con `eph::get_microdata()` y crea `salidas/`.
4. Abrir `salidas/analisis_muestra_poblacion_pluriempleo_2022_2026.xlsx` para ver muestra y población estimada.

> Si la descarga de algún trimestre falla porque la fuente no está disponible, la ejecución se detiene con un mensaje. No se reemplazan datos por valores inventados.

## Salidas
- `salidas/eph_general.rds`: microdatos originales descargados (no se publican en GitHub).
- `salidas/tabla_encuestados.rds`, `tabla_resumen.rds`, `tabla_muestra_total.rds`: tamaños muestrales.
- `salidas/poblacion_sexo.rds`, `tasa_general_sexo.rds`: población expandida y tasas.
- `salidas/datos_areas.rds`, `datos_areas_varones.rds`, `datos_areas_sexo.rds`: aportes territoriales.
- `salidas/datos_tablero.rds`: datos agregados para visualizaciones y tablero.
- `salidas/analisis_muestra_poblacion_pluriempleo_2022_2026.xlsx`: tablas en hojas separadas.
- Otras tablas históricas `.rds` y `.xlsx`.

Para recalcular descargando nuevamente, cambiar `actualizar_microdatos <- TRUE` en el script.

## Cartografía (opcional)
Para generar `salidas/aglomerados_buenos_aires.geojson`, colocar `aglomerados_eph_json.zip` en la raíz del repositorio antes de ejecutar. El script instalará `sf` si hace falta. Sin ese ZIP, **el análisis tabular se ejecuta igualmente**, pero el GeoJSON no se genera.

## Definiciones
- Ocupados: `ESTADO == 1`.
- Una ocupación: `PP03C == 1`.
- Pluriempleo: `PP03C == 2`.
- Sin clasificar: otros valores de `PP03C` entre ocupados.
- Sexo según `CH04`: 1 varones, 2 mujeres.
- `encuestados`: registros de la muestra, sin expandir.
- `poblacion_estimada`: suma del ponderador `PONDERA`.
- Tasa de pluriempleo: pluriempleados estimados / ocupados estimados del mismo sexo y territorio.
- Aporte territorial: pluriempleados estimados del territorio / ocupados estimados del mismo sexo en los cinco aglomerados.

Fuente: INDEC, Encuesta Permanente de Hogares, primeros trimestres 2022–2026.

## Publicación
Se excluye `salidas/` mediante `.gitignore` porque contiene archivos descargados y resultados reproducibles. Los scripts y documentación sí se publican.
