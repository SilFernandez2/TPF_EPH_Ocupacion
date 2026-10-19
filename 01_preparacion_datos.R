# ================================================================
# 01_preparacion_datos.R | EPH: PLURIEMPLEO BUENOS AIRES
# Primer trimestre de 2022 a 2026 | 5 aglomerados
# Descarga microdatos, controles, muestra, expansión, tasas,
# brechas, aportes territoriales, salidas RDS, Excel y GeoJSON.
#
# USO: abrir la carpeta del proyecto en VS Code y ejecutar
# source("01_preparacion_datos.R")
# Instalar paquetes una sola vez si hace falta.
# ================================================================

paquetes <- c('eph', 'dplyr', 'tidyr', 'writexl')
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) {
  message("Instalando paquetes necesarios: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}
suppressPackageStartupMessages({
  library(eph); library(dplyr); library(tidyr); library(writexl)
})

# Ejecutar desde la carpeta raíz del proyecto.
# En VS Code: abrir la carpeta del proyecto antes de ejecutar.
# Localiza el script incluso si la carpeta de trabajo de R es distinta.
argumentos <- commandArgs(trailingOnly = FALSE)
archivo_arg <- grep("^--file=", argumentos, value = TRUE)
ruta_script <- if (length(archivo_arg)) {
  sub("^--file=", "", archivo_arg[1])
} else {
  marcos <- sys.frames()
  archivos_source <- lapply(marcos, function(x) x$ofile)
  archivos_source <- Filter(function(x) !is.null(x), archivos_source)
  if (length(archivos_source)) tail(archivos_source, 1)[[1]] else "01_preparacion_datos.R"
}
if (!file.exists(ruta_script)) {
  stop("No se pudo localizar 01_preparacion_datos.R. Abrí la carpeta del proyecto en VS Code.")
}
carpeta <- dirname(normalizePath(ruta_script, winslash = "/", mustWork = TRUE))
dir.create(file.path(carpeta, "salidas"), showWarnings = FALSE)
salidas <- file.path(carpeta, "salidas")

# 1. DESCARGA / RECUPERACION DE MICRODATOS
# Si ya existe el RDS, no vuelve a descargar. Para actualizar, poné TRUE.
actualizar_microdatos <- FALSE
ruta_eph <- file.path(salidas, 'eph_general.rds')
if (!actualizar_microdatos && file.exists(ruta_eph)) {
  eph_general <- readRDS(ruta_eph)
} else {
  eph_general <- dplyr::bind_rows(lapply(2022:2026, function(anio) {
    message('Descargando EPH: primer trimestre ', anio)
    tryCatch(
      eph::get_microdata(year = anio, trimester = 1, type = 'individual'),
      error = function(e) stop(
        "No se pudo descargar la EPH del primer trimestre de ", anio,
        ". Verificá la conexión y disponibilidad en INDEC. Detalle: ",
        conditionMessage(e), call. = FALSE
      )
    )
  }))
  saveRDS(eph_general, ruta_eph)
}
stopifnot(all(c('ANO4', 'TRIMESTRE', 'AGLOMERADO', 'ESTADO', 'PP03C', 'PP03D', 'CH04', 'PONDERA') %in% names(eph_general)))
stopifnot(all(2022:2026 %in% unique(eph_general$ANO4)))
print(table(eph_general$ANO4, eph_general$TRIMESTRE))

# 2. AGLOMERADOS Y CLASIFICACION
nombres_aglomerados <- c(
  '2' = 'Gran La Plata', '3' = 'Bahía Blanca-Cerri',
  '32' = 'CABA', '33' = 'Partidos del GBA',
  '34' = 'Mar del Plata-Batán'
)
base_ba <- eph_general %>%
  filter(ANO4 %in% 2022:2026, TRIMESTRE == 1,
         as.character(AGLOMERADO) %in% names(nombres_aglomerados),
         ESTADO %in% c(1, 2, 3)) %>%
  mutate(
    nombre = unname(nombres_aglomerados[as.character(AGLOMERADO)]),
    anio = as.integer(ANO4),
    trimestre = paste0(TRIMESTRE, '_', ANO4),
    categoria = case_when(
      PP03C == 1 & ESTADO == 1 ~ 'Una ocupación',
      PP03C == 2 & ESTADO == 1 ~ 'Pluriempleo',
      ESTADO == 1 ~ 'Sin clasificar',
      TRUE ~ NA_character_
    )
  )

# 3. TABLA HISTORICA DE CONTEOS (SIN PONDERAR, COMO EN EL ANALISIS ORIGINAL)
tabla_final_historica <- base_ba %>%
  group_by(AGLOMERADO, nombre, trimestre, anio) %>%
  summarise(
    total_personas = n(),
    estado_1 = sum(ESTADO == 1, na.rm = TRUE),
    estado_2 = sum(ESTADO == 2, na.rm = TRUE),
    estado_3 = sum(ESTADO == 3, na.rm = TRUE),
    ocupacion_0 = sum(PP03D == 0, na.rm = TRUE),
    ocupacion_2 = sum(PP03D == 2, na.rm = TRUE),
    ocupacion_3 = sum(PP03D == 3, na.rm = TRUE),
    ocupacion_4 = sum(PP03D == 4, na.rm = TRUE),
    ocupacion_5 = sum(PP03D == 5, na.rm = TRUE),
    ocupacion_6 = sum(PP03D == 6, na.rm = TRUE),
    ocupacion_7 = sum(PP03D == 7, na.rm = TRUE),
    ocupacion_8 = sum(PP03D == 8, na.rm = TRUE),
    ocupacion_9 = sum(PP03D == 9, na.rm = TRUE),
    ocupacion_NA = sum(is.na(PP03D)),
    una_ocupacion = sum(ESTADO == 1 & PP03C == 1, na.rm = TRUE),
    pluriempleo = sum(ESTADO == 1 & PP03C == 2, na.rm = TRUE),
    ocupado_sin_clasificar = sum(ESTADO == 1 & (is.na(PP03C) | !PP03C %in% c(1, 2)), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    suma_estados = estado_1 + estado_2 + estado_3,
    suma_ocupaciones = rowSums(across(starts_with('ocupacion_'))),
    suma_clasificacion = una_ocupacion + pluriempleo + ocupado_sin_clasificar,
    control_estados = total_personas == suma_estados,
    control_ocupaciones = total_personas == suma_ocupaciones,
    control_clasificacion = estado_1 == suma_clasificacion,
    control_NA = ocupacion_NA == estado_2 + estado_3
  ) %>%
  arrange(AGLOMERADO, anio)

# 4. COMPOSICION PONDERADA DE OCUPADOS
ocupados_ponderados <- base_ba %>%
  filter(ESTADO == 1, !is.na(PONDERA), PONDERA > 0) %>%
  group_by(AGLOMERADO, nombre, trimestre, anio, categoria) %>%
  summarise(personas = sum(PONDERA, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = categoria, values_from = personas, values_fill = 0) %>%
  rename(una_ocupacion = `Una ocupación`, pluriempleo = Pluriempleo,
         sin_clasificar = `Sin clasificar`) %>%
  mutate(total_ocupados = una_ocupacion + pluriempleo + sin_clasificar) %>%
  arrange(AGLOMERADO, anio)

base_2022 <- ocupados_ponderados %>%
  filter(anio == 2022) %>%
  transmute(AGLOMERADO, base_ocupados_2022 = total_ocupados,
            base_una_2022 = una_ocupacion, base_pluri_2022 = pluriempleo,
            base_sin_clasificar_2022 = sin_clasificar)

tabla_evolucion_ocupacion <- ocupados_ponderados %>%
  left_join(base_2022, by = 'AGLOMERADO') %>%
  mutate(
    indice_base_100 = total_ocupados / base_ocupados_2022 * 100,
    variacion_total = indice_base_100 - 100,
    aporte_una_pp = (una_ocupacion - base_una_2022) / base_ocupados_2022 * 100,
    aporte_pluri_pp = (pluriempleo - base_pluri_2022) / base_ocupados_2022 * 100,
    aporte_sin_clasificar_pp = (sin_clasificar - base_sin_clasificar_2022) / base_ocupados_2022 * 100,
    suma_aportes_pp = aporte_una_pp + aporte_pluri_pp + aporte_sin_clasificar_pp,
    control_aportes = abs(suma_aportes_pp - variacion_total) < 1e-6
  )

tabla_historica_completa <- tabla_final_historica %>%
  left_join(tabla_evolucion_ocupacion %>%
              select(AGLOMERADO, trimestre, indice_base_100, variacion_total,
                     aporte_una_pp, aporte_pluri_pp, aporte_sin_clasificar_pp,
                     suma_aportes_pp, control_aportes),
            by = c('AGLOMERADO', 'trimestre'))

# 5. COMPOSICION POR SEXO (PONDERADA)
tabla_sexo <- base_ba %>%
  filter(ESTADO == 1, CH04 %in% c(1, 2), !is.na(PONDERA), PONDERA > 0) %>%
  mutate(sexo = case_when(CH04 == 1 ~ 'Varones', CH04 == 2 ~ 'Mujeres')) %>%
  group_by(AGLOMERADO, nombre, trimestre, sexo, categoria) %>%
  summarise(personas = sum(PONDERA, na.rm = TRUE), .groups = 'drop') %>%
  group_by(AGLOMERADO, trimestre, sexo) %>%
  mutate(total_ocupados = sum(personas), porcentaje = personas / total_ocupados * 100) %>%
  ungroup()

# Brecha = participacion dentro de mujeres - participacion dentro de varones
brecha_sexo_historica <- tabla_sexo %>%
  select(nombre, trimestre, categoria, sexo, porcentaje) %>%
  pivot_wider(names_from = sexo, values_from = porcentaje) %>%
  mutate(anio = as.integer(sub('1_', '', trimestre)), brecha_pp = Mujeres - Varones) %>%
  arrange(nombre, categoria, anio) %>%
  group_by(nombre, categoria) %>%
  mutate(
    cambio_anual_pp = brecha_pp - lag(brecha_pp),
    etiqueta = ifelse(is.na(cambio_anual_pp), '', sprintf('%+.1f', cambio_anual_pp)),
    direccion = case_when(cambio_anual_pp > 0 ~ 'Aumentó',
                          cambio_anual_pp < 0 ~ 'Disminuyó', TRUE ~ 'Sin cambio')
  ) %>% ungroup()

# 6. VERIFICACIONES Y EXPORTACIONES
stopifnot(nrow(brecha_sexo_historica) == 75,
          all(tabla_final_historica$control_estados),
          all(tabla_final_historica$control_clasificacion),
          all(tabla_evolucion_ocupacion$control_aportes))
if (!all(tabla_final_historica$control_ocupaciones)) {
  warning('Hay filas con control_ocupaciones FALSE. Revisar codigos PP03D.')
}
if (!all(tabla_final_historica$control_NA)) {
  warning('Hay filas con control_NA FALSE. Revisar PP03D y ESTADO.')
}

saveRDS(tabla_sexo, file.path(salidas, 'tabla_sexo.rds'))
saveRDS(brecha_sexo_historica, file.path(salidas, 'brecha_sexo_historica.rds'))
saveRDS(tabla_historica_completa, file.path(salidas, 'tabla_historica_completa.rds'))
writexl::write_xlsx(tabla_historica_completa,
                    file.path(salidas, 'tabla_historica_buenos_aires_2022_2026.xlsx'))
writexl::write_xlsx(tabla_sexo,
                    file.path(salidas, 'tabla_ocupacion_por_sexo_2022_2026.xlsx'))


# 7. TAMAÑOS MUESTRALES Y POBLACION EXPANDIDA POR SEXO Y CATEGORIA
# 'encuestados' = registros de la muestra; 'poblacion_estimada' = suma de PONDERA.
# No confundir la población expandida con la cantidad de encuestados.
tabla_encuestados <- base_ba %>%
  filter(ESTADO == 1, CH04 %in% c(1, 2)) %>%
  mutate(sexo = if_else(CH04 == 1, "Varones", "Mujeres")) %>%
  group_by(anio, AGLOMERADO, nombre, sexo, categoria) %>%
  summarise(
    encuestados = n(),
    poblacion_estimada = sum(PONDERA, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(anio, AGLOMERADO, sexo, categoria)

tabla_resumen <- tabla_encuestados %>%
  select(anio, AGLOMERADO, nombre, sexo, categoria, encuestados) %>%
  pivot_wider(names_from = categoria, values_from = encuestados, values_fill = 0) %>%
  mutate(total_ocupados = `Una ocupación` + Pluriempleo + `Sin clasificar`) %>%
  select(anio, AGLOMERADO, nombre, sexo, total_ocupados,
         `Una ocupación`, Pluriempleo, `Sin clasificar`) %>%
  arrange(anio, AGLOMERADO, sexo)

tabla_muestra_total <- eph_general %>%
  filter(ANO4 %in% 2022:2026, TRIMESTRE == 1,
         AGLOMERADO %in% c(2, 3, 32, 33, 34)) %>%
  mutate(nombre = unname(nombres_aglomerados[as.character(AGLOMERADO)])) %>%
  group_by(anio = ANO4, AGLOMERADO, nombre) %>%
  summarise(
    personas_encuestadas = n(),
    ocupados = sum(ESTADO == 1, na.rm = TRUE),
    .groups = "drop"
  )

# 8. TASAS PONDERADAS Y PARTICIPACION DE CADA SEXO EN CADA AGLOMERADO
poblacion_sexo <- tabla_encuestados %>%
  group_by(anio, AGLOMERADO, nombre, sexo) %>%
  summarise(
    muestra_ocupados = sum(encuestados),
    poblacion_ocupada_estimada = sum(poblacion_estimada),
    poblacion_pluriempleada_estimada =
      sum(poblacion_estimada[categoria == "Pluriempleo"]),
    muestra_pluriempleo = sum(encuestados[categoria == "Pluriempleo"]),
    .groups = "drop"
  ) %>%
  mutate(tasa_pluriempleo_pct =
           100 * poblacion_pluriempleada_estimada / poblacion_ocupada_estimada) %>%
  group_by(anio, AGLOMERADO) %>%
  mutate(
    poblacion_ocupada_ambos_sexos = sum(poblacion_ocupada_estimada),
    participacion_sexo_en_ocupados_pct =
      100 * poblacion_ocupada_estimada / poblacion_ocupada_ambos_sexos
  ) %>%
  ungroup()

# Tasa de pluriempleo total por sexo (cinco aglomerados combinados).
# No se promedian tasas por aglomerado: se suman numeradores y denominadores.
tasa_general_sexo <- poblacion_sexo %>%
  group_by(anio, sexo) %>%
  summarise(
    ocupados_estimados = sum(poblacion_ocupada_estimada),
    pluriempleados_estimados = sum(poblacion_pluriempleada_estimada),
    encuestados_ocupados = sum(muestra_ocupados),
    encuestados_pluriempleo = sum(muestra_pluriempleo),
    .groups = "drop"
  ) %>%
  mutate(tasa_pluriempleo_pct =
           100 * pluriempleados_estimados / ocupados_estimados) %>%
  arrange(anio, sexo)

# 9. APORTE TERRITORIAL AL PLURIEMPLEO DE CADA SEXO
# Aporte_pp: pluriempleados estimados del aglomerado / ocupados estimados
# del mismo sexo en TODOS los cinco aglomerados * 100.
# La suma de aportes coincide con la tasa general del mismo sexo y año.
datos_areas_sexo <- poblacion_sexo %>%
  left_join(
    tasa_general_sexo %>%
      select(anio, sexo, denominador_cinco_aglomerados = ocupados_estimados),
    by = c("anio", "sexo")
  ) %>%
  mutate(aporte_pp = 100 * poblacion_pluriempleada_estimada /
           denominador_cinco_aglomerados) %>%
  arrange(anio, sexo, AGLOMERADO)

control_aportes_territoriales <- datos_areas_sexo %>%
  group_by(anio, sexo) %>%
  summarise(suma_aportes_pp = sum(aporte_pp), .groups = "drop") %>%
  left_join(tasa_general_sexo %>%
              select(anio, sexo, tasa_pluriempleo_pct),
            by = c("anio", "sexo")) %>%
  mutate(control = abs(suma_aportes_pp - tasa_pluriempleo_pct) < 1e-8)
stopifnot(all(control_aportes_territoriales$control))

datos_areas <- datos_areas_sexo %>% filter(sexo == "Mujeres")
datos_areas_varones <- datos_areas_sexo %>% filter(sexo == "Varones")
datos_total_mujeres <- tasa_general_sexo %>%
  filter(sexo == "Mujeres") %>%
  transmute(anio, total_pct = tasa_pluriempleo_pct,
            etiqueta = sprintf("%.1f%%", total_pct))
datos_total_varones <- tasa_general_sexo %>%
  filter(sexo == "Varones") %>%
  transmute(anio, total_pct = tasa_pluriempleo_pct,
            etiqueta = sprintf("%.1f%%", total_pct))

# 10. TABLA PARA TABLERO SHINY: 5 AGLOMERADOS x 5 AÑOS x 3 CATEGORIAS
composicion_mapa <- base_ba %>%
  filter(ESTADO == 1, !is.na(PONDERA), PONDERA > 0) %>%
  group_by(nombre, anio, categoria) %>%
  summarise(ocupados = sum(PONDERA), .groups = "drop") %>%
  group_by(nombre, anio) %>%
  mutate(porcentaje_total = 100 * ocupados / sum(ocupados)) %>%
  ungroup()

sexo_mapa <- tabla_sexo %>%
  mutate(anio = as.integer(sub("1_", "", trimestre))) %>%
  select(nombre, anio, categoria, sexo, porcentaje) %>%
  pivot_wider(names_from = sexo, values_from = porcentaje) %>%
  rename(pct_varones = Varones, pct_mujeres = Mujeres)

datos_tablero <- composicion_mapa %>%
  left_join(sexo_mapa, by = c("nombre", "anio", "categoria")) %>%
  left_join(
    brecha_sexo_historica %>%
      select(nombre, anio, categoria, brecha_pp, cambio_anual_pp),
    by = c("nombre", "anio", "categoria")
  )
stopifnot(nrow(datos_tablero) == 75,
          !anyNA(datos_tablero$pct_varones),
          !anyNA(datos_tablero$pct_mujeres),
          !anyNA(datos_tablero$brecha_pp))

# 11. DESCARGAS: ARCHIVOS RDS Y EXCEL
# Los RDS permiten que el tablero y los gráficos lean datos sin volver a descargar.
objetos_rds <- list(
  tabla_final_historica = tabla_final_historica,
  tabla_evolucion_ocupacion = tabla_evolucion_ocupacion,
  tabla_encuestados = tabla_encuestados,
  tabla_resumen = tabla_resumen,
  tabla_muestra_total = tabla_muestra_total,
  poblacion_sexo = poblacion_sexo,
  tasa_general_sexo = tasa_general_sexo,
  datos_areas_sexo = datos_areas_sexo,
  datos_areas = datos_areas,
  datos_areas_varones = datos_areas_varones,
  datos_total_mujeres = datos_total_mujeres,
  datos_total_varones = datos_total_varones,
  datos_tablero = datos_tablero,
  control_aportes_territoriales = control_aportes_territoriales
)
for (nm in names(objetos_rds)) {
  saveRDS(objetos_rds[[nm]], file.path(salidas, paste0(nm, ".rds")))
}

writexl::write_xlsx(
  list(
    "Muestra por categoria" = tabla_encuestados,
    "Resumen muestra" = tabla_resumen,
    "Muestra total" = tabla_muestra_total,
    "Poblacion por sexo" = poblacion_sexo,
    "Tasa general sexo" = tasa_general_sexo,
    "Aporte territorial" = datos_areas_sexo,
    "Control aportes" = control_aportes_territoriales
  ),
  file.path(salidas, "analisis_muestra_poblacion_pluriempleo_2022_2026.xlsx")
)

# 12. CARTOGRAFIA: SI ESTA EL ZIP ORIGINAL, GENERAR GEOJSON DE AGLOMERADOS
# Requiere sf y el archivo aglomerados_eph_json.zip en la carpeta del proyecto.
archivo_zip <- file.path(carpeta, "aglomerados_eph_json.zip")
if (file.exists(archivo_zip)) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    message("Instalando sf para procesar la cartografía...")
    install.packages("sf", repos = "https://cloud.r-project.org")
  }
  if (!requireNamespace("sf", quietly = TRUE)) stop("Se necesita el paquete sf para procesar el ZIP geográfico.")
  carpeta_geo <- file.path(salidas, "poligonos_eph")
  dir.create(carpeta_geo, recursive = TRUE, showWarnings = FALSE)
  unzip(archivo_zip, exdir = carpeta_geo)
  archivos_geo <- list.files(carpeta_geo, pattern = "\\.(geojson|json)$",
                            full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (length(archivos_geo) > 0) {
    mapa_eph <- sf::st_read(archivos_geo[1], quiet = TRUE)
    poligonos_ba <- mapa_eph %>%
      mutate(AGLOMERADO = as.integer(eph_codagl)) %>%
      filter(AGLOMERADO %in% c(2, 3, 32, 33, 34)) %>%
      group_by(AGLOMERADO) %>%
      summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
      mutate(nombre = unname(nombres_aglomerados[as.character(AGLOMERADO)])) %>%
      sf::st_transform(4326) %>%
      select(AGLOMERADO, nombre, geometry)
    sf::st_write(poligonos_ba,
                 file.path(salidas, "aglomerados_buenos_aires.geojson"),
                 delete_dsn = TRUE, quiet = TRUE)
  }
} else {
  message("Cartografía: no se encontró aglomerados_eph_json.zip; se omite GeoJSON.")
}

# 13. COMPROBACIONES Y LISTADO DE SALIDAS
stopifnot(nrow(tabla_encuestados) == 150,
          nrow(tabla_resumen) == 50,
          nrow(poblacion_sexo) == 50,
          nrow(tasa_general_sexo) == 10,
          all(control_aportes_territoriales$control))
message("\nPROCESAMIENTO FINALIZADO. Archivos generados en: ", salidas)
print(list.files(salidas, pattern = "\\.(rds|xlsx|geojson)$"))
