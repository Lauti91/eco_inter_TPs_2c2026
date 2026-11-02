#===============================================================================#
# TP5 - Economías de escala - EJERCICIO 2: GRÁFICOS
#===============================================================================#
# OBJETIVO
#   (1) Inciso (b): Grubel-Lloyd (GL_i) de los 10 capítulos de mayor comercio
#       (X+M) de Argentina con cada socio (Brasil, China), con el corte de 0,5.
#   (2) Inciso (d): diagrama de dumping con dos mercados segmentados (local y
#       externo), a partir de los datos numéricos del enunciado.
#
# ENTRADAS
#   output/tablas/3.1/GL_por_capitulo.csv : lo genera TP5_ej2_grubel_lloyd_WITS.R
#                                           (correr ese script primero).
#
# SALIDAS
#   output/graficos/3.1/: g_GL_top10_BRA.png, g_GL_top10_CHN.png, g_dumping_diagrama.png
#   output/tablas/3.1/  : dumping_calculos.csv
#
# CONVENCIONES PROPIAS DE ESTE SCRIPT (no vienen del enunciado del TP):
#   - Corte GL_i >= 0,5 para "predominantemente intraindustrial".
#   - Dumping: el enunciado no da costos, por eso el diagrama NO dibuja ningún CMg;
#     solo compara los ingresos marginales de los dos mercados en el punto actual.
#===============================================================================#

for (p in c("tidyverse", "scales", "showtext", "stringr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(scales)

# Formato de números en todos los gráficos: coma decimal y punto de miles.
number <- function(x, ..., big.mark = ".", decimal.mark = ",") scales::number(x, ..., big.mark = big.mark, decimal.mark = decimal.mark)
label_number <- function(..., big.mark = ".", decimal.mark = ",") scales::label_number(..., big.mark = big.mark, decimal.mark = decimal.mark)
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
# BLOQUE 0: PARÁMETROS Y ESTILO
#===============================================================================#

corte_intra <- 0.5        # GL_i >= corte -> predominantemente intraindustrial
n_top       <- 10

font_add_google("Rajdhani", "rajdhani")
showtext_auto()
showtext_opts(dpi = 300)

color_fondo_tp2  <- "#FAFAF8"
color_texto_tp2  <- "#262421"
color_texto_sec  <- "#6B6660"
color_grilla_tp2 <- "#E4E1DB"
color_titulo_tp2 <- "#7A1128"
color_rojo       <- "#C1272D"   # destaque puntual
color_verde      <- "#006233"
color_gris_dato  <- "#B8B3AA"

paleta_tipo <- c("intraindustrial" = color_verde, "interindustrial" = color_rojo)

env_wrap <- function(x, width = 50) paste(stringr::str_wrap(x, width = width), collapse = "\n")

theme_tp2 <- function() {
  theme_minimal(base_family = "rajdhani") +
    theme(
      plot.title.position   = "plot",     # título, subtítulo y fuente alineados al borde del gráfico
      plot.caption.position = "plot",     # (no al panel): con etiquetas largas en el eje y no se cortan
      plot.background  = element_rect(fill = color_fondo_tp2, color = NA),
      panel.background = element_rect(fill = color_fondo_tp2, color = NA),
      panel.grid.major = element_line(color = color_grilla_tp2, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title    = element_text(color = color_titulo_tp2, face = "bold", size = 22, hjust = 0,
                                   lineheight = 1.02, margin = margin(b = 3)),
      plot.subtitle = element_text(color = color_texto_sec, size = 13, lineheight = 1.05,
                                   margin = margin(b = 10)),
      plot.caption  = element_text(color = color_texto_sec, size = 9.6, hjust = 0,
                                   lineheight = 1.1, margin = margin(t = 8)),
      axis.title    = element_text(color = color_texto_sec, size = 12.5, face = "bold"),
      axis.text     = element_text(color = color_texto_tp2, size = 12, face = "bold"),
      axis.ticks    = element_blank(),
      legend.text   = element_text(color = color_texto_sec, size = 11),
      legend.title  = element_text(color = color_texto_tp2, size = 12, face = "bold"),
      legend.position = "bottom",
      strip.text    = element_text(color = color_texto_tp2, face = "bold", size = 12),
      plot.margin   = margin(18, 34, 14, 18)
    )
}

# Nombres en español de los capítulos del SA (2 dígitos) que aparecen en los top 10
# de Brasil y China. Traducción propia de las descripciones en inglés de WITS.
# Si un capítulo no está acá, se deja el nombre en inglés de la base y se avisa.
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
# BLOQUE 1: GL_i DE LOS 10 CAPÍTULOS PRINCIPALES (inciso b)
#===============================================================================#

# ProductCode se lee como texto para no perder el cero de "02", "04", etc.
gl <- read_csv(archivo_gl, col_types = cols(ProductCode = col_character(),
                                            PartnerISO3 = col_character()))

graficar_gl <- function(socio, nombre_socio) {
  d <- gl %>%
    filter(PartnerISO3 == socio) %>%
    slice_max(comercio, n = n_top, with_ties = FALSE) %>%
    mutate(desc_es  = nombre_cap_es(ProductCode, ProductDescription),
           etiqueta = paste0(ProductCode, " ", str_trunc(desc_es, 40)),
           etiqueta = fct_reorder(etiqueta, comercio),            # mayor comercio arriba
           peso     = 100 * comercio / sum(gl$comercio[gl$PartnerISO3 == socio]),
           tipo     = factor(if_else(GL_i >= corte_intra, "intraindustrial", "interindustrial"),
                             levels = c("intraindustrial", "interindustrial")))
  if (any(d$desc_es == d$ProductDescription)) message("AVISO: capítulos sin traducción en ", nombre_socio, ": ",
                                                      paste(d$ProductCode[d$desc_es == d$ProductDescription], collapse = ", "))
  cobertura <- sum(d$peso)
  
  ggplot(d, aes(GL_i, etiqueta, fill = tipo)) +
    geom_col(width = 0.72) +
    # Capa auxiliar sin barras visibles: fuerza que la leyenda muestre las dos categorías
    # aunque un socio no tenga ningún capítulo en una de ellas (ej.: China sin intraindustriales).
    geom_col(data = tibble(etiqueta = d$etiqueta[1], GL_i = 0,
                           tipo = factor(c("intraindustrial", "interindustrial"),
                                         levels = c("intraindustrial", "interindustrial"))),
             aes(GL_i, etiqueta, fill = tipo), position = "identity", width = 0, show.legend = TRUE,
             inherit.aes = FALSE) +
    geom_vline(xintercept = corte_intra, linetype = "dashed", color = color_texto_sec, linewidth = 0.5) +
    # Etiquetas en una columna fija a la derecha, para que la línea del corte no las atraviese
    geom_text(aes(x = 1.04, label = paste0(number(GL_i, accuracy = 0.001), "  (", number(peso, accuracy = 0.1), "% del comercio)")),
              hjust = 0, family = "rajdhani", fontface = "bold", size = 3.8,
              color = color_texto_tp2) +
    scale_fill_manual(values = paleta_tipo, name = "Predominio (GL_i >= 0,5 = intraindustrial)", drop = FALSE) +
    scale_x_continuous(breaks = seq(0, 1, 0.25), expand = c(0, 0), labels = label_number(accuracy = 0.01)) +
    coord_cartesian(xlim = c(0, 1.5), clip = "off") +
    labs(title = env_wrap(paste0("Argentina - ", nombre_socio, ": índice de Grubel-Lloyd por capítulo, 2024"), 75),
         subtitle = env_wrap(paste0("Los ", n_top, " capítulos del SA (2 dígitos) de mayor comercio total (X+M), de mayor a menor; ",
                                    "cubren el ", number(cobertura, accuracy = 0.1), "% del comercio bilateral. Línea punteada: corte de 0,5."), 85),
         x = "Índice de Grubel-Lloyd del capítulo", y = NULL,
         caption = env_wrap(paste0("Fuente: WITS (UN Comtrade), Argentina como declarante, SA 2022, valores en miles de USD. ",
                                   "Se excluye el capítulo 99. Entre paréntesis, peso del capítulo en el comercio bilateral total. ",
                                   "El corte de 0,5 es una convención."), 125)) +
    theme_tp2() + theme(panel.grid.major.y = element_blank())
}

g_bra <- graficar_gl("BRA", "Brasil"); g_bra
ggsave(paste0(ruta_graficos, "g_GL_top10_BRA.png"), g_bra,
       width = 10, height = 7, dpi = 300, bg = color_fondo_tp2)

g_chn <- graficar_gl("CHN", "China"); g_chn
ggsave(paste0(ruta_graficos, "g_GL_top10_CHN.png"), g_chn,
       width = 10, height = 7, dpi = 300, bg = color_fondo_tp2)

#===============================================================================#
# BLOQUE 2: DUMPING CON MERCADOS SEGMENTADOS (inciso d)
#===============================================================================#
# Demanda lineal que pasa por (Q0, P0) y baja $0,01 por cada unidad adicional:
#   P = P0 - b (Q - Q0) = a - b Q ,  con a = P0 + b Q0 ,  b = 0,01
#   IT = P Q = a Q - b Q^2        IMg = a - 2 b Q
b <- 0.01
mercados <- data.frame(mercado = c("Mercado local", "Mercado externo"),
                       Q0 = c(800, 100), P0 = c(18, 12)) %>%
  mutate(a = P0 + b * Q0,
         IT0 = P0 * Q0,
         IT1 = (Q0 + 1) * (a - b * (Q0 + 1)),      # ingreso total con una unidad más
         IMg_discreto = IT1 - IT0,                  # IT(Q0+1) - IT(Q0)
         IMg_continuo = a - 2 * b * Q0,
         elasticidad  = P0 / (b * Q0),              # |e| = (1/b)(P/Q)
         IMg_P_1_menos_1_sobre_e = P0 * (1 - 1 / elasticidad),   # chequeo: = IMg_continuo
         panel = paste0(mercado, " (|e| = ", number(elasticidad, accuracy = 0.01), ")"),
         panel = factor(panel, levels = panel))
print(as.data.frame(mercados), digits = 6)
write_csv(mercados %>% mutate(panel = as.character(panel)), paste0(ruta_tablas, "dumping_calculos.csv"))

# Curvas de demanda e ingreso marginal (hasta IMg = 0 y un poco más)
curvas <- bind_rows(lapply(seq_len(nrow(mercados)), function(i) {
  m <- mercados[i, ]
  Q <- seq(0, 1.1 * m$a / (2 * b), length.out = 200)
  data.frame(panel = m$panel, Q = Q,
             Demanda = m$a - b * Q,
             `Ingreso marginal` = m$a - 2 * b * Q, check.names = FALSE)
})) %>%
  pivot_longer(c(Demanda, `Ingreso marginal`), names_to = "curva", values_to = "valor") %>%
  filter(valor >= 0)
curvas$panel <- factor(curvas$panel, levels = levels(mercados$panel))

paleta_curvas <- c("Demanda" = color_texto_tp2, "Ingreso marginal" = color_rojo)

g_dump <- ggplot() +
  geom_line(data = curvas, aes(Q, valor, color = curva), linewidth = 1) +
  # IMg actual (horizontal hasta el eje) y cantidad actual (vertical hasta la demanda)
  geom_segment(data = mercados, aes(x = 0, xend = Q0, y = IMg_continuo, yend = IMg_continuo),
               linetype = "dashed", color = color_texto_sec, linewidth = 0.5) +
  geom_segment(data = mercados, aes(x = Q0, xend = Q0, y = 0, yend = P0),
               linetype = "dotted", color = color_texto_sec, linewidth = 0.5) +
  geom_point(data = mercados, aes(Q0, P0), size = 2.8, color = color_texto_tp2) +
  geom_point(data = mercados, aes(Q0, IMg_continuo), size = 2.8, color = color_rojo) +
  geom_text(data = mercados, aes(Q0, P0, label = paste0("P = $ ", number(P0))),
            hjust = -0.15, vjust = -0.7, family = "rajdhani", fontface = "bold", size = 3.9,
            color = color_texto_tp2) +
  # Etiqueta con fondo del color de la página, para que la línea punteada no la tache
  geom_label(data = mercados, aes(0, IMg_continuo, label = paste0("IMg = $ ", number(IMg_continuo))),
             hjust = -0.05, vjust = 1.3, family = "rajdhani", fontface = "bold", size = 3.9,
             color = color_rojo, fill = color_fondo_tp2, label.size = 0, label.padding = unit(0.15, "lines")) +
  geom_text(data = mercados, aes(Q0, 0, label = paste0("Q = ", number(Q0))),
            hjust = 0.5, vjust = 1.9, family = "rajdhani", fontface = "bold", size = 3.9,
            color = color_texto_tp2) +
  facet_wrap(~panel, scales = "free_x") +      # eje y compartido: permite comparar los IMg a simple vista
  scale_color_manual(values = paleta_curvas, name = NULL) +
  scale_x_continuous(labels = label_number(), expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(labels = label_number(prefix = "$ "), expand = expansion(mult = c(0.06, 0.08))) +
  coord_cartesian(clip = "off") +
  labs(title = env_wrap("Dumping: discriminación de precios con mercados segmentados", 75),
       subtitle = env_wrap(paste0("Con las cantidades actuales, el ingreso marginal local ($ ", number(mercados$IMg_continuo[1]),
                                  ") es menor que el externo ($ ", number(mercados$IMg_continuo[2]),
                                  "): conviene expandir las ventas en el mercado externo."), 85),
       x = "Cantidad", y = "Precio e ingreso marginal ($)",
       caption = env_wrap(paste0("Demandas lineales con pendiente -$ 0,01 por unidad adicional, que pasan por (800; $ 18) y (100; $ 12). ",
                                 "|e| = elasticidad-precio de la demanda en el punto actual. ",
                                 "El enunciado no da costos: no se dibuja el costo marginal."), 125)) +
  theme_tp2() + theme(panel.spacing = unit(2.5, "lines"))
g_dump
ggsave(paste0(ruta_graficos, "g_dumping_diagrama.png"), g_dump,
       width = 11, height = 6.5, dpi = 300, bg = color_fondo_tp2)

cat("\nListo. Gráficos en:", ruta_graficos, "| tabla en:", ruta_tablas, "\n")

#===============================================================================#
# FIN. Para correr: abrir el proyecto y ejecutar entero (después del script de GL).
#===============================================================================#