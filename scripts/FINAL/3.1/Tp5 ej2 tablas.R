#===============================================================================#
# TP5 - Economías de escala - EJERCICIO 2: TABLAS COMO IMAGEN
#===============================================================================#
# OBJETIVO
#   Armar, con la misma estética de los gráficos del ejercicio, las tablas de los
#   incisos (a) y (b) para pegarlas en el documento como imagen:
#     (a) GL agregado de Argentina con Brasil y con China, 2024.
#     (b) Los 10 capítulos de mayor comercio total (X+M) con cada socio.
#
# ENTRADAS
#   output/tablas/3.1/GL_por_capitulo.csv : lo genera TP5_ej2_grubel_lloyd_WITS.R
#                                           (correr ese script primero).
#
# SALIDAS (carpeta output/tablas/3.1/)
#   t_GL_agregado.png, t_top10_BRA.png, t_top10_CHN.png
#
# NOTA: las tablas se dibujan con ggplot2 (sin paquetes extra ni navegador), así que
#   la tipografía, los colores y el formato de números son los mismos que en los gráficos.
#
# CONVENCIONES PROPIAS DE ESTE SCRIPT (no vienen del enunciado del TP):
#   - Corte GL_i >= 0,5 para "predominantemente intraindustrial".
#===============================================================================#

for (p in c("tidyverse", "scales", "showtext", "stringr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(scales)

# Formato de números en todas las tablas: coma decimal y punto de miles.
number <- function(x, ..., big.mark = ".", decimal.mark = ",") scales::number(x, ..., big.mark = big.mark, decimal.mark = decimal.mark)
library(showtext)

# Directorio de trabajo = carpeta del proyecto (la que contiene "bases de datos/").
if (!dir.exists("bases de datos")) stop("No encuentro la carpeta 'bases de datos/' en el directorio de trabajo (", getwd(),
                                        "). Abrí el proyecto o usá setwd() hacia su carpeta.")
ruta_datos    <- "bases de datos/"
ruta_graficos <- "output/graficos/3.1/"
ruta_tablas   <- "output/tablas/3.1/"
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas,   recursive = TRUE, showWarnings = FALSE)

archivo_gl <- paste0(ruta_tablas, "GL_por_capitulo.csv")
if (!file.exists(archivo_gl)) {
  cat("Buscado:", archivo_gl, "| getwd():", getwd(), "\nContenido de la carpeta:\n")
  print(list.files(ruta_tablas))
  stop("Falta GL_por_capitulo.csv. Correr primero TP5_ej2_grubel_lloyd_WITS.R")
}

#===============================================================================#
# BLOQUE 0: PARÁMETROS Y ESTILO (los mismos que en TP5_ej2_graficos.R)
#===============================================================================#

corte_intra <- 0.5
n_top       <- 10

font_add_google("Rajdhani", "rajdhani")
showtext_auto()
showtext_opts(dpi = 300)

color_fondo_tp2  <- "#FAFAF8"
color_texto_tp2  <- "#262421"
color_texto_sec  <- "#6B6660"
color_grilla_tp2 <- "#E4E1DB"
color_titulo_tp2 <- "#7A1128"
color_rojo       <- "#C1272D"
color_verde      <- "#006233"
color_zebra      <- "#F2F0EB"

env_wrap <- function(x, width = 50) paste(stringr::str_wrap(x, width = width), collapse = "\n")

theme_tp2 <- function() {
  theme_minimal(base_family = "rajdhani") +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.background  = element_rect(fill = color_fondo_tp2, color = NA),
      panel.background = element_rect(fill = color_fondo_tp2, color = NA),
      panel.grid       = element_blank(),
      plot.title    = element_text(color = color_titulo_tp2, face = "bold", size = 22, hjust = 0,
                                   lineheight = 1.02, margin = margin(b = 3)),
      plot.subtitle = element_text(color = color_texto_sec, size = 13, lineheight = 1.05,
                                   margin = margin(b = 10)),
      plot.caption  = element_text(color = color_texto_sec, size = 9.6, hjust = 0,
                                   lineheight = 1.1, margin = margin(t = 8)),
      axis.title    = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
      legend.position = "none",
      plot.margin   = margin(18, 34, 14, 18)
    )
}

# Nombres en español de los capítulos del SA (2 dígitos). Traducción propia de las
# descripciones en inglés de WITS; si falta alguno se deja el nombre en inglés y se avisa.
nombres_cap_es <- c(
  "02" = "Carne y despojos comestibles",      "04" = "Lácteos, huevos y miel",
  "10" = "Cereales",                          "12" = "Semillas y frutos oleaginosos",
  "15" = "Grasas y aceites animales o vegetales", "26" = "Minerales, escorias y cenizas",
  "27" = "Combustibles y aceites minerales",  "28" = "Productos químicos inorgánicos",
  "29" = "Productos químicos orgánicos",      "39" = "Plásticos y sus manufacturas",
  "40" = "Caucho y sus manufacturas",         "72" = "Fundición, hierro y acero",
  "84" = "Máquinas y aparatos mecánicos",     "85" = "Máquinas y aparatos eléctricos",
  "87" = "Vehículos automóviles y sus partes"
)
nombre_cap_es <- function(codigo, desc_en) {
  out <- unname(nombres_cap_es[as.character(codigo)])
  ifelse(is.na(out), desc_en, out)
}

#===============================================================================#
# BLOQUE 1: FUNCIÓN QUE DIBUJA UNA TABLA
#===============================================================================#
# df       : data.frame (se convierte todo a texto; formatear los números antes).
# anchos   : ancho relativo de cada columna (la suma es el ancho total del panel).
# alin     : "l", "c" o "r" por columna.
# negrita  : índices de columnas en negrita.
# col_tipo : índice de la columna cuyo texto se pinta según "Intra"/"Inter".
tabla_png <- function(df, anchos, alin, titulo, subtitulo, caption, archivo,
                      negrita = integer(0), col_tipo = NA_integer_, alto_cab = 1.7) {
  df <- as.data.frame(lapply(df, as.character), check.names = FALSE, stringsAsFactors = FALSE)
  n <- nrow(df); k <- ncol(df)
  x1 <- cumsum(anchos); x0 <- x1 - anchos; xm <- (x0 + x1) / 2
  pad <- 1.2
  pos_x <- function(col) case_when(alin[col] == "l" ~ x0[col] + pad,
                                   alin[col] == "r" ~ x1[col] - pad,
                                   TRUE ~ xm[col])
  
  celdas <- tibble(fila = rep(seq_len(n), each = k),
                   col  = rep(seq_len(k), times = n),
                   txt  = as.vector(t(as.matrix(df)))) %>%
    mutate(a = alin[col], x = pos_x(col), y = alto_cab + fila - 0.5,
           ff = if_else(col %in% negrita, "bold", "plain"),
           color_txt = case_when(col %in% col_tipo & grepl("^Intra", txt) ~ color_verde,
                                 col %in% col_tipo & grepl("^Inter", txt) ~ color_rojo,
                                 TRUE ~ color_texto_tp2),
           ff = if_else(col %in% col_tipo, "bold", ff))
  cab <- tibble(col = seq_len(k), txt = names(df)) %>%
    mutate(a = alin[col], x = pos_x(col), y = alto_cab / 2)
  filas <- tibble(fila = seq_len(n)) %>%
    mutate(ymin = alto_cab + fila - 1, ymax = alto_cab + fila, zebra = fila %% 2 == 0)
  total_x <- sum(anchos)
  
  g <- ggplot() +
    geom_rect(data = filas %>% filter(zebra), aes(xmin = 0, xmax = total_x, ymin = ymin, ymax = ymax),
              fill = color_zebra) +
    geom_rect(aes(xmin = 0, xmax = total_x, ymin = 0, ymax = alto_cab), fill = color_titulo_tp2) +
    geom_segment(data = filas, aes(x = 0, xend = total_x, y = ymax, yend = ymax),
                 color = color_grilla_tp2, linewidth = 0.4)
  for (al in c("l", "c", "r")) {
    hj <- c(l = 0, c = 0.5, r = 1)[[al]]
    g <- g +
      geom_text(data = cab %>% filter(a == al), aes(x, y, label = txt), hjust = hj,
                family = "rajdhani", fontface = "bold", size = 4.4, color = "white", lineheight = 0.9) +
      geom_text(data = celdas %>% filter(a == al), aes(x, y, label = txt, color = color_txt, fontface = ff),
                hjust = hj, family = "rajdhani", size = 4.6)
  }
  g <- g +
    scale_color_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reverse(expand = expansion(add = 0.05)) +
    coord_cartesian(clip = "off") +
    labs(title = env_wrap(titulo, 75), subtitle = env_wrap(subtitulo, 85), caption = env_wrap(caption, 125)) +
    theme_tp2()
  
  ggsave(paste0(ruta_tablas, archivo), g, width = 10, height = 1.9 + (alto_cab + n) * 0.37,
         dpi = 300, bg = color_fondo_tp2)
  invisible(g)
}

#===============================================================================#
# BLOQUE 2: DATOS
#===============================================================================#

# ProductCode se lee como texto para no perder el cero de "02", "04", etc.
gl <- read_csv(archivo_gl, col_types = cols(ProductCode = col_character(),
                                            PartnerISO3 = col_character()))
nombre_socio <- c(BRA = "Brasil", CHN = "China")

fuente <- "Fuente: elaboración propia con datos de WITS (UN Comtrade), Argentina como declarante, SA 2022, 2024. Se excluye el capítulo 99."

#===============================================================================#
# BLOQUE 3: TABLA (a): GL AGREGADO
#===============================================================================#

agr <- gl %>%
  group_by(PartnerISO3) %>%
  summarise(X = sum(Export), M = sum(Import), T = sum(comercio),
            num = sum(comercio - abs(Export - Import)), .groups = "drop") %>%
  mutate(GL = num / T,
         GL_ajust = num / (T - abs(X - M)),            # ajuste de Grubel y Lloyd (1975)
         saldo = 100 * (X - M) / T,
         Socio = nombre_socio[PartnerISO3]) %>%
  arrange(factor(PartnerISO3, levels = names(nombre_socio)))
print(as.data.frame(agr), digits = 5)

tab_a <- agr %>%
  transmute(Socio,
            `X\n(MUSD)` = number(X / 1e3, accuracy = 0.1),
            `M\n(MUSD)` = number(M / 1e3, accuracy = 0.1),
            `Saldo X-M\n(% del comercio)` = number(saldo, accuracy = 0.1, suffix = "%"),
            `GL\nagregado` = number(GL, accuracy = 0.001),
            `GL ajustado\n(Grubel-Lloyd)` = number(GL_ajust, accuracy = 0.001))

tabla_png(tab_a, anchos = c(16, 17, 17, 20, 15, 15), alin = c("l", "r", "r", "r", "r", "r"),
          titulo = "Argentina: índice de Grubel-Lloyd agregado con Brasil y China, 2024",
          subtitulo = "Promedio del GL_i de los capítulos (SA, 2 dígitos) ponderado por el comercio total de cada capítulo.",
          caption = paste0(fuente, " GL ajustado: descuenta el saldo comercial total del denominador."),
          archivo = "t_GL_agregado.png", negrita = c(1, 5), alto_cab = 2)

#===============================================================================#
# BLOQUE 4: TABLAS (b): TOP 10 POR COMERCIO TOTAL
#===============================================================================#

tabla_top10 <- function(socio) {
  d <- gl %>% filter(PartnerISO3 == socio)
  total <- sum(d$comercio)
  t10 <- d %>%
    slice_max(comercio, n = n_top, with_ties = FALSE) %>%
    mutate(desc_es = nombre_cap_es(ProductCode, ProductDescription),
           # el tipo se define con el GL_i numérico, antes de pasarlo a texto formateado
           tipo = if_else(GL_i >= corte_intra, "Intraindustrial", "Interindustrial"))
  if (any(t10$desc_es == t10$ProductDescription))
    message("AVISO: capítulos sin traducción en ", nombre_socio[[socio]], ": ",
            paste(t10$ProductCode[t10$desc_es == t10$ProductDescription], collapse = ", "))
  cobertura <- 100 * sum(t10$comercio) / total
  
  tab <- t10 %>%
    transmute(`Cap.` = ProductCode,
              `Descripción` = str_trunc(desc_es, 38),
              `X\n(MUSD)` = number(Export / 1e3, accuracy = 0.1),
              `M\n(MUSD)` = number(Import / 1e3, accuracy = 0.1),
              `X + M\n(MUSD)` = number(comercio / 1e3, accuracy = 0.1),
              `% del\ncomercio` = number(100 * comercio / total, accuracy = 0.1, suffix = "%"),
              `GL_i` = number(GL_i, accuracy = 0.001),
              `Tipo` = tipo)
  
  tabla_png(tab, anchos = c(7, 31, 11, 11, 12, 10, 8, 16),
            alin = c("c", "l", "r", "r", "r", "r", "r", "l"),
            titulo = paste0("Argentina - ", nombre_socio[[socio]], ": los 10 capítulos de mayor comercio, 2024"),
            subtitulo = paste0("Ordenados por comercio total (X+M); cubren el ", number(cobertura, accuracy = 0.1),
                               "% del comercio bilateral. Intraindustrial: GL_i >= 0,5."),
            caption = paste0(fuente, " El corte de 0,5 es una convención."),
            archivo = paste0("t_top10_", socio, ".png"), negrita = c(1, 7), col_tipo = 8L)
}

for (s in names(nombre_socio)) tabla_top10(s)

cat("\nListo. Tablas (PNG) en:", ruta_tablas, "\n")

#===============================================================================#
# FIN. Para correr: abrir el proyecto y ejecutar entero (después del script de GL).
#===============================================================================#