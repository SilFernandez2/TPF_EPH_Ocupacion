# =====================================================================
# 02_visualizaciones.R | EPH Buenos Aires, primeros trimestres 2022-2026
# Requiere haber ejecutado 01_preparacion_datos.R en este repositorio.
# Ejecutar: source('02_visualizaciones.R') o Rscript 02_visualizaciones.R
# =====================================================================

paquetes <- c('dplyr', 'tidyr', 'ggplot2', 'plotly', 'htmlwidgets', 'scales')
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) install.packages(faltantes, repos = 'https://cloud.r-project.org')
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2)
  library(plotly); library(htmlwidgets); library(scales)
})

# Identificar la carpeta del script, independientemente del directorio activo.
args <- grep('^--file=', commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(args)) {
  ruta_script <- sub('^--file=', '', args[1])
} else {
  archivos_source <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
  ruta_script <- if (length(archivos_source)) tail(archivos_source, 1)[[1]] else '02_visualizaciones.R'
}
if (!file.exists(ruta_script)) stop('Abrí el proyecto en VS Code y ejecutá 02_visualizaciones.R.')
raiz <- dirname(normalizePath(ruta_script, winslash = '/', mustWork = TRUE))
salidas <- file.path(raiz, 'salidas')
graficos <- file.path(salidas, 'graficos')
dir.create(graficos, recursive = TRUE, showWarnings = FALSE)

leer <- function(nombre) {
  ruta <- file.path(salidas, paste0(nombre, '.rds'))
  if (!file.exists(ruta)) {
    stop('Falta ', ruta, '. Primero ejecutá 01_preparacion_datos.R.', call. = FALSE)
  }
  readRDS(ruta)
}

# Fuentes agregadas generadas por 01_preparacion_datos.R
poblacion_sexo <- leer('poblacion_sexo')
tabla_encuestados <- leer('tabla_encuestados')

colores_sexo <- c('Mujeres' = '#008C68', 'Varones' = '#E66A19')
colores_categoria <- c('Una ocupación' = '#7356A6',
                       'Pluriempleo' = '#E8BA45',
                       'Sin clasificar' = '#B8BEC8')
colores_aglomerados <- c('Partidos del GBA' = '#E76BF3',
                         'CABA' = '#A3A500',
                         'Gran La Plata' = '#00BF7D',
                         'Mar del Plata-Batán' = '#00B0F6',
                         'Bahía Blanca-Cerri' = '#F8766D')
orden_aglomerados <- names(colores_aglomerados)
orden_categoria <- names(colores_categoria)

# La muestra y la expansión se calculan para cada categoría y sexo.
# Tasas: numerador categoría / denominador ocupados del mismo sexo y territorio.
# Las tasas generales usan sumas de población, NO promedios simples de tasas.
base <- tabla_encuestados %>%
  group_by(anio, sexo, categoria) %>%
  summarise(muestra_categoria = sum(encuestados),
            poblacion_categoria = sum(poblacion_estimada), .groups = 'drop') %>%
  group_by(anio, sexo) %>%
  mutate(muestra_ocupados = sum(muestra_categoria),
         poblacion_ocupada = sum(poblacion_categoria),
         porcentaje = 100 * poblacion_categoria / poblacion_ocupada) %>%
  ungroup() %>%
  arrange(categoria, sexo, anio) %>%
  group_by(sexo, categoria) %>%
  mutate(variacion_anual_pp = porcentaje - lag(porcentaje)) %>%
  ungroup()
stopifnot(nrow(base) == 30)
control_composicion <- base %>%
  group_by(anio, sexo) %>%
  summarise(total_pct = sum(porcentaje), .groups = 'drop')
stopifnot(all(abs(control_composicion$total_pct - 100) < 1e-8))

# 1. BARRAS APILADAS: composición por sexo, un PNG por año.
composicion <- base %>%
  mutate(sexo = factor(sexo, levels = c('Varones', 'Mujeres')),
         categoria = factor(categoria, levels = orden_categoria),
         etiqueta = ifelse(porcentaje >= 3, sprintf('%.1f%%', porcentaje), ''))

grafico_composicion_anio <- function(anio_elegido) {
  ggplot(filter(composicion, anio == anio_elegido),
         aes(x = sexo, y = porcentaje, fill = categoria)) +
    geom_col(width = .62, color = 'white') +
    geom_text(aes(label = etiqueta), position = position_stack(vjust = .5),
              size = 4, color = '#252525') +
    coord_flip() +
    scale_fill_manual(values = colores_categoria, drop = FALSE) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                       labels = function(x) paste0(x, '%'),
                       expand = expansion(mult = c(0, 0))) +
    labs(title = paste('Composición ocupacional por sexo |', anio_elegido),
         subtitle = 'Cinco aglomerados de Buenos Aires | Primer trimestre',
         x = NULL, y = 'Porcentaje de personas ocupadas',
         fill = 'Categoría ocupacional',
         caption = 'Fuente: INDEC - EPH | Estimaciones ponderadas con PONDERA') +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = 'bold'), legend.position = 'bottom',
          panel.grid.major.y = element_blank())
}
for (a in 2022:2026) {
  ggsave(file.path(graficos, paste0('composicion_ocupacional_', a, '.png')),
         grafico_composicion_anio(a), width = 11, height = 6, dpi = 160)
}

# GIF opcional: se puede omitir sin afectar el resto de los gráficos.
# Para generarlo: instalar gganimate y gifski y cambiar a TRUE.
generar_gif <- FALSE
if (generar_gif) {
  extra <- c('gganimate', 'gifski')
  faltan <- extra[!vapply(extra, requireNamespace, logical(1), quietly = TRUE)]
  if (length(faltan)) install.packages(faltan, repos = 'https://cloud.r-project.org')
  g_anim <- ggplot(composicion, aes(sexo, porcentaje, fill = categoria)) +
    geom_col(width = .62, color = 'white') +
    geom_text(aes(label = etiqueta), position = position_stack(vjust = .5), size = 4) +
    coord_flip() +
    scale_fill_manual(values = colores_categoria) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, '%')) +
    labs(title = 'Composición ocupacional por sexo | {current_frame}',
         subtitle = 'Cinco aglomerados | Primer trimestre', x = NULL,
         y = 'Porcentaje de ocupados', fill = 'Categoría',
         caption = 'INDEC - EPH | Datos ponderados') +
    theme_minimal(base_size = 13) +
    theme(legend.position = 'bottom') +
    gganimate::transition_manual(anio)
  anim <- gganimate::animate(g_anim, duration = 10, fps = 20,
                             width = 1100, height = 600,
                             renderer = gganimate::gifski_renderer())
  gganimate::anim_save(file.path(graficos, 'composicion_ocupacional_sexo.gif'), anim)
}

# 2. LINEAS INTERACTIVAS CON TOOLTIP: una por categoría.
# El tooltip incluye tasa, muestra, población expandida y cambio anual.
formatear_entero <- function(x) format(round(x), big.mark = '.', decimal.mark = ',', trim = TRUE, scientific = FALSE)
formatear_pct <- function(x) formatC(x, format = 'f', digits = 1, decimal.mark = ',')

lineas_interactivas <- function(cat) {
  df <- base %>% filter(categoria == cat) %>%
    mutate(sexo = factor(sexo, levels = c('Mujeres', 'Varones')),
           tooltip = paste0('<b>Año:</b> ', anio,
                            '<br><b>Sexo:</b> ', sexo,
                            '<br><b>Categoría:</b> ', categoria,
                            '<br><b>Porcentaje:</b> ', formatear_pct(porcentaje), '%',
                            '<br><b>Variación interanual:</b> ',
                            ifelse(is.na(variacion_anual_pp), 'Año base',
                                   paste0(ifelse(variacion_anual_pp > 0, '+', ''),
                                          formatear_pct(variacion_anual_pp), ' pp')),
                            '<br><b>Encuestados en categoría:</b> ', formatear_entero(muestra_categoria),
                            '<br><b>Población estimada en categoría:</b> ', formatear_entero(poblacion_categoria),
                            '<br><b>Población ocupada estimada del sexo:</b> ', formatear_entero(poblacion_ocupada),
                            '<br><b>Período:</b> Primer trimestre',
                            '<br><b>Fuente:</b> INDEC - EPH'))
  gg <- ggplot(df, aes(anio, porcentaje, color = sexo, group = sexo)) +
    geom_line(linewidth = 1.4) +
    geom_point(aes(text = tooltip), size = 4.5) +
    scale_color_manual(values = colores_sexo) +
    scale_x_continuous(breaks = 2022:2026) +
    scale_y_continuous(labels = function(x) paste0(x, '%')) +
    expand_limits(y = 0) +
    labs(title = paste('Evolución de', tolower(cat), 'por sexo'),
         subtitle = 'Cinco aglomerados de Buenos Aires | Primer trimestre 2022-2026',
         x = NULL, y = 'Porcentaje de ocupados del mismo sexo', color = NULL,
         caption = 'Fuente: INDEC - EPH | Datos ponderados') +
    theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = 'bold'), legend.position = 'top',
          panel.grid.minor = element_blank())
  ggplotly(gg, tooltip = 'text') %>%
    layout(hovermode = 'closest', legend = list(orientation = 'h', x = .4, y = 1.12))
}

grafico_linea_pluriempleo <- lineas_interactivas('Pluriempleo')
grafico_linea_una_ocupacion <- lineas_interactivas('Una ocupación')
htmlwidgets::saveWidget(grafico_linea_pluriempleo,
                        file.path(graficos, 'evolucion_pluriempleo_sexo.html'),
                        selfcontained = FALSE)
htmlwidgets::saveWidget(grafico_linea_una_ocupacion,
                        file.path(graficos, 'evolucion_una_ocupacion_sexo.html'),
                        selfcontained = FALSE)

# 3. APORTES TERRITORIALES POR SEXO Y CATEGORÍA.
# Denominador: todos los ocupados estimados del mismo sexo en los 5 aglomerados.
# Suma de los cinco aportes = porcentaje general de la categoría en ese sexo/año.
datos_aportes <- tabla_encuestados %>%
  filter(categoria %in% c('Pluriempleo', 'Una ocupación')) %>%
  left_join(base %>% select(anio, sexo, categoria, poblacion_ocupada, porcentaje),
            by = c('anio', 'sexo', 'categoria')) %>%
  mutate(aporte_pp = 100 * poblacion_estimada / poblacion_ocupada,
         nombre = factor(nombre, levels = orden_aglomerados))
control_aportes <- datos_aportes %>%
  group_by(anio, sexo, categoria) %>%
  summarise(suma_aportes_pp = sum(aporte_pp), tasa_general_pct = first(porcentaje),
            .groups = 'drop') %>%
  mutate(control = abs(suma_aportes_pp - tasa_general_pct) < 1e-8)
stopifnot(all(control_aportes$control))

# Misma escala para mujeres y varones de una misma categoría, para comparar.
area_territorial <- function(cat, sexo_elegido) {
  df <- datos_aportes %>% filter(categoria == cat, sexo == sexo_elegido)
  totales <- control_aportes %>% filter(categoria == cat, sexo == sexo_elegido) %>%
    mutate(etiqueta = paste0(formatear_pct(tasa_general_pct), '%'))
  ymax <- ceiling(max(filter(control_aportes, categoria == cat)$tasa_general_pct) * 1.13)
  ggplot(df, aes(x = anio, y = aporte_pp, fill = nombre, group = nombre)) +
    geom_area(color = 'white', linewidth = .45) +
    geom_line(data = totales, aes(x = anio, y = tasa_general_pct, group = 1),
              inherit.aes = FALSE, color = unname(colores_sexo[sexo_elegido]), linewidth = 1.3) +
    geom_point(data = totales, aes(x = anio, y = tasa_general_pct),
               inherit.aes = FALSE, color = unname(colores_sexo[sexo_elegido]),
               fill = 'white', shape = 21, stroke = 1.3, size = 3.6) +
    geom_text(data = totales, aes(x = anio, y = tasa_general_pct, label = etiqueta),
              inherit.aes = FALSE, vjust = -1.2, size = 3.8, fontface = 'bold',
              color = unname(colores_sexo[sexo_elegido])) +
    scale_fill_manual(values = colores_aglomerados, drop = FALSE) +
    scale_x_continuous(breaks = 2022:2026, expand = expansion(mult = c(.01, .01))) +
    scale_y_continuous(limits = c(0, ymax), labels = function(x) paste0(x, '%')) +
    labs(title = paste('Aporte territorial a', tolower(cat), '|', tolower(sexo_elegido)),
         subtitle = 'Cinco aglomerados | Primer trimestre 2022-2026',
         x = NULL, y = 'Aporte al total del mismo sexo (pp)', fill = NULL,
         caption = 'INDEC - EPH | Aporte territorial: población de la categoría en el aglomerado / ocupados del mismo sexo en los 5 aglomerados') +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = 'bold'), legend.position = 'bottom',
          panel.grid.minor = element_blank(), plot.caption = element_text(size = 8))
}
for (cat in c('Pluriempleo', 'Una ocupación')) {
  for (s in c('Mujeres', 'Varones')) {
    slug <- if (cat == 'Pluriempleo') 'pluriempleo' else 'una_ocupacion'
    sexo_slug <- if (s == 'Mujeres') 'mujeres' else 'varones'
    g <- area_territorial(cat, s)
    ggsave(file.path(graficos, paste0('aporte_', slug, '_', sexo_slug, '.png')),
           g, width = 12, height = 7, dpi = 160)
  }
}

# 4. TABLAS DE APOYO PARA REVISAR Y ANALIZAR LOS GRÁFICOS.
saveRDS(base, file.path(salidas, 'datos_composicion_sexo.rds'))
saveRDS(datos_aportes, file.path(salidas, 'datos_aportes_territoriales.rds'))
writexl_disponible <- requireNamespace('writexl', quietly = TRUE)
if (writexl_disponible) {
  writexl::write_xlsx(list('Composicion por sexo' = base,
                           'Aportes territoriales' = datos_aportes,
                           'Control aportes' = control_aportes),
                     file.path(salidas, 'tablas_para_visualizaciones.xlsx'))
}
message('Gráficos creados en: ', graficos)
print(list.files(graficos))
