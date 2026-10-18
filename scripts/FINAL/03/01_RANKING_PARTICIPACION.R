#===============================================================================#
# TP6 - Política Comercial - SCRIPT 1: RANKING DE PARTICIPACIÓN DE MARRUECOS
# EN EL COMERCIO MUNDIAL, BIEN POR BIEN (CUCI Rev.3, 3 dígitos)
#===============================================================================#
# OBJETIVO
#   Para cada bien (3 dígitos CUCI) que Marruecos exporta o importa, calcular su
#   participación en el comercio mundial de ese bien:
#       participación_{b,f,t} = valor Marruecos_{b,f,t} / valor mundial_{b,f,t}
#   y ordenar los bienes de dos maneras:
#       (i)  por VALOR     -> responde a la parte (a) del TP: top-3 exportados e
#            importados.
#       (ii) por PARTICIPACIÓN -> responde a "¿en qué bienes Marruecos es grande
#            o pequeño?": insumo para elegir el producto de la parte (a).
#
# BASES (carpeta "bases de datos/", WITS, CUCI Rev.3, miles de USD)
#   - MAR_ALLPARTNERS_WLD.dta : Marruecos (reportero) con socio "World", por
#                               bien 3 dígitos, flujo y año (2021-2025).
#   - WLD_WLD.dta             : totales mundiales por bien 3 dígitos, flujo y año.
#
# AÑO DE REFERENCIA: anio_ref = 2023 (ver nota de integridad abajo).
#
# NOTA DE INTEGRIDAD DE DATOS (2024-2025)
#   Los valores de Marruecos como reportero en 2024 y 2025 están inflados en la
#   fuente: la suma de los bienes da ~80,6 mil M USD de exportaciones en 2024
#   contra ~42,5 mil M en 2023 (y la Office des Changes y el perfil de la OMC no
#   muestran ese salto). Con ese numerador y un denominador mundial no inflado,
#   las participaciones de 2024-2025 quedan sobreestimadas. Por eso el ranking
#   principal usa 2023 y 2021-2022 sirven de chequeo de estabilidad. 2024-2025
#   se calculan solo para dejar el contraste a la vista (columna `fuente_ok`).
#
# CONVENCIONES PROPIAS DE ESTE SCRIPT (no vienen del enunciado del TP):
#   - umbral_valor: se ignoran para el ranking por participación los bienes con
#     comercio de Marruecos menor a este valor, porque participaciones altas
#     sobre valores minúsculos no informan sobre el tamaño del país.
#   - cortes_tamano: clasificación descriptiva grande / intermedio / pequeño.
#     Es una regla práctica del script, no un criterio de la cátedra.
#
# SALIDAS
#   output/tablas/03/  : ranking_participacion_completo.csv (todos los años),
#                        ranking_participacion_{exportaciones,importaciones}_<anio_ref>.csv,
#                        top3_por_valor_<anio_ref>.csv
#   output/graficos/03/: g_ranking_participacion_{exportaciones,importaciones}.png,
#                        g_valor_vs_participacion.png
#===============================================================================#

for (p in c("tidyverse", "haven", "ggrepel", "scales", "showtext", "stringr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(tidyverse)
library(haven)
library(ggrepel)
library(scales)

# Formato de números en todos los gráficos: coma decimal y punto de miles.
# Se redefinen number() y label_number() con ese formato por defecto (funciona con cualquier versión de scales).
number <- function(x, ..., big.mark = ".", decimal.mark = ",") scales::number(x, ..., big.mark = big.mark, decimal.mark = decimal.mark)
label_number <- function(..., big.mark = ".", decimal.mark = ",") scales::label_number(..., big.mark = big.mark, decimal.mark = decimal.mark)
library(showtext)

# Directorio de trabajo = carpeta del proyecto (la que contiene "bases de datos/").
# En Posit Cloud es /cloud/project; si ya estás parado en la carpeta, no se cambia.
if (!dir.exists("bases de datos") && dir.exists("/cloud/project")) setwd("/cloud/project")
if (!dir.exists("bases de datos")) stop("No encuentro la carpeta 'bases de datos/' en el directorio de trabajo (", getwd(),
                                         "). Abrí el proyecto o usá setwd() hacia su carpeta.")
ruta_datos    <- "bases de datos/"
ruta_graficos <- "output/graficos/03/"
ruta_tablas   <- "output/tablas/03/"
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas,   recursive = TRUE, showWarnings = FALSE)

#===============================================================================#
# BLOQUE 0: PARÁMETROS Y ESTILO
#===============================================================================#

anio_ref      <- 2023           # año base (ver nota de integridad)
anios_ok      <- 2021:2023      # años con datos confiables de Marruecos
umbral_valor  <- 50000          # miles de USD (= US$ 50 M); ver convenciones
cortes_tamano <- c(grande = 10, intermedio = 3)   # en % de participación mundial
n_top_grafico <- 15

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

paleta_tamano <- c("grande" = color_rojo, "intermedio" = color_verde, "pequeño" = color_gris_dato)

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

# convierte cualquier columna (labelled, factor o texto) a character
a_chr <- function(x) trimws(as.character(haven::as_factor(x)))

# Nombres en español de los códigos CUCI Rev.3 (3 dígitos) que aparecen en los
# rankings y gráficos. Traducción propia de las descripciones en inglés de WITS
# (la base solo trae inglés). Si un código no está acá, se deja el nombre en inglés
# de la base y el script avisa por consola.
nombres_cuci_es <- c(
  "36"  = "Crustáceos y moluscos",              "37"  = "Pescado y mariscos preparados",
  "41"  = "Trigo y morcajo",                    "43"  = "Cebada",
  "44"  = "Maíz (salvo maíz dulce)",            "54"  = "Hortalizas frescas o congeladas",
  "57"  = "Frutas y frutos secos",              "61"  = "Azúcar, melazas y miel",
  "74"  = "Té y yerba mate",                    "272" = "Fertilizantes en bruto",
  "274" = "Azufre y piritas sin tostar",        "291" = "Materias animales en bruto, n.e.p.",
  "334" = "Aceites de petróleo pesados",        "342" = "Propano y butano licuados",
  "411" = "Aceites y grasas animales",          "421" = "Aceites vegetales fijos, suaves",
  "522" = "Elementos, óxidos y sales halógenas","562" = "Fertilizantes manufacturados",
  "612" = "Manufacturas de cuero",              "652" = "Tejidos de algodón",
  "653" = "Tejidos de fibras sintéticas",       "654" = "Otros tejidos textiles",
  "655" = "Tejidos de punto",                   "656" = "Tul, encajes y bordados",
  "773" = "Equipo de distribución eléctrica",   "781" = "Automóviles de pasajeros",
  "784" = "Partes y accesorios de vehículos",   "792" = "Aeronaves y naves espaciales",
  "841" = "Ropa de hombre, tejida",             "842" = "Ropa de mujer, tejida"
)
nombre_cuci_es <- function(codigo, desc_en) {
  cod <- sub("^0+", "", as.character(codigo))     # "054" -> "54"
  out <- unname(nombres_cuci_es[cod])
  ifelse(is.na(out), desc_en, out)
}

#===============================================================================#
# BLOQUE 1: CARGA Y UNIÓN DE LAS DOS BASES
#===============================================================================#

leer_wits <- function(archivo) {
  b <- read_dta(paste0(ruta_datos, archivo))
  names(b) <- tolower(names(b))
  b %>%
    transmute(
      cuci   = a_chr(productcode),
      desc   = a_chr(productdescription),
      socio  = a_chr(partnername),
      flujo  = a_chr(tradeflowname),
      anio   = as.integer(year),
      valor  = as.numeric(tradevaluein1000usd)
    )
}

mar <- leer_wits("MAR_ALLPARTNERS_WLD.dta") %>%
  filter(tolower(socio) == "world") %>%
  select(cuci, desc, flujo, anio, valor_mar = valor)

mundo <- leer_wits("WLD_WLD.dta") %>%
  select(cuci, flujo, anio, valor_mundo = valor)

# --- Auditoría de claves: no debe haber duplicados en ninguna de las dos bases
stopifnot(!anyDuplicated(mar[c("cuci", "flujo", "anio")]),
          !anyDuplicated(mundo[c("cuci", "flujo", "anio")]))

ranking <- mar %>%
  inner_join(mundo, by = c("cuci", "flujo", "anio")) %>%
  mutate(
    participacion = 100 * valor_mar / valor_mundo,
    flujo = recode(flujo, "Export" = "Exportaciones", "Import" = "Importaciones"),
    fuente_ok = anio %in% anios_ok,
    desc_es = nombre_cuci_es(cuci, desc)
  )

# --- Auditoría: bienes de Marruecos que no encontraron mundial, y participaciones > 100
cat("Filas Marruecos:", nrow(mar), "| con mundial:", nrow(ranking), "\n")
stopifnot(all(ranking$participacion <= 100, na.rm = TRUE))

# Totales de control por año (suma de los bienes de 3 dígitos), para ver el salto 2024
control_totales <- ranking %>%
  group_by(flujo, anio) %>%
  summarise(total_mar_mM = sum(valor_mar) / 1e6, .groups = "drop") %>%
  arrange(flujo, anio)
print(control_totales)

#===============================================================================#
# BLOQUE 2: RANKINGS
#===============================================================================#

clasificar <- function(p) {
  case_when(p >= cortes_tamano["grande"]     ~ "grande",
            p >= cortes_tamano["intermedio"] ~ "intermedio",
            TRUE                             ~ "pequeño")
}

ranking <- ranking %>%
  group_by(flujo, anio) %>%
  mutate(
    peso_en_total  = 100 * valor_mar / sum(valor_mar),
    rank_valor     = min_rank(desc(valor_mar)),
    rank_part      = if_else(valor_mar >= umbral_valor,
                             min_rank(desc(if_else(valor_mar >= umbral_valor, participacion, NA_real_))),
                             NA_integer_)
  ) %>%
  ungroup() %>%
  mutate(tamano = clasificar(participacion))

write_csv(ranking %>% arrange(flujo, anio, rank_valor),
          paste0(ruta_tablas, "ranking_participacion_completo.csv"))

# --- Tabla de la parte (a): top-3 por valor en el año de referencia
top3 <- ranking %>%
  filter(anio == anio_ref, rank_valor <= 3) %>%
  arrange(flujo, rank_valor) %>%
  select(flujo, rank_valor, cuci, desc_es, desc, valor_mar, peso_en_total, valor_mundo, participacion, tamano)
print(top3)
write_csv(top3, paste0(ruta_tablas, "top3_por_valor_", anio_ref, ".csv"))

# --- Rankings por participación (solo bienes con valor >= umbral)
for (fl in c("Exportaciones", "Importaciones")) {
  tab <- ranking %>%
    filter(anio == anio_ref, flujo == fl, !is.na(rank_part)) %>%
    arrange(rank_part) %>%
    select(rank_part, cuci, desc_es, desc, valor_mar, valor_mundo, participacion, tamano, peso_en_total, rank_valor)
  cat("\n", fl, anio_ref, ": ", nrow(tab), "bienes con valor >= umbral\n")
  print(head(tab, 12))
  write_csv(tab, paste0(ruta_tablas, "ranking_participacion_", tolower(fl), "_", anio_ref, ".csv"))
}

# --- Estabilidad 2021-2023 de los bienes líderes en el año base
estabilidad <- ranking %>%
  filter(anio %in% anios_ok, flujo %in% c("Exportaciones", "Importaciones")) %>%
  semi_join(ranking %>% filter(anio == anio_ref, !is.na(rank_part), rank_part <= 10),
            by = c("flujo", "cuci")) %>%
  select(flujo, cuci, desc, anio, participacion) %>%
  pivot_wider(names_from = anio, values_from = participacion, names_prefix = "part_")
write_csv(estabilidad, paste0(ruta_tablas, "estabilidad_participacion_top10.csv"))

#===============================================================================#
# BLOQUE 3: GRÁFICOS
#===============================================================================#

fmt_mM <- function(v_miles) number(v_miles / 1e6, accuracy = 0.1, suffix = " mil M")

# --- G1/G2: ranking por participación (barras horizontales), por flujo
graficar_ranking <- function(fl) {
  d <- ranking %>%
    filter(anio == anio_ref, flujo == fl, !is.na(rank_part)) %>%
    arrange(rank_part) %>%
    slice_head(n = n_top_grafico) %>%
    mutate(etiqueta = paste0(cuci, " ", str_trunc(desc_es, 34)),
           etiqueta = fct_reorder(etiqueta, participacion))
  if (any(d$desc_es == d$desc)) message("AVISO: códigos CUCI sin traducción en ", fl, ": ",
                                        paste(d$cuci[d$desc_es == d$desc], collapse = ", "))
  ggplot(d, aes(participacion, etiqueta, fill = tamano)) +
    geom_col(width = 0.72) +
    geom_text(aes(label = paste0(number(participacion, accuracy = 0.1), "%  (",
                                 number(valor_mar / 1e6, accuracy = 0.01), " mil M)")),
              hjust = -0.06, family = "rajdhani", fontface = "bold", size = 3.8,
              color = color_texto_tp2) +
    scale_fill_manual(values = paleta_tamano, name = "Tamaño en el mercado") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.35)), labels = label_number(suffix = "%")) +
    labs(title = env_wrap(paste0("Marruecos: participación en las ", tolower(fl), " mundiales, ", anio_ref), 55),
         subtitle = env_wrap(paste0("Los ", n_top_grafico, " bienes (CUCI 3 dígitos) con mayor participación, entre los que Marruecos ",
                                    "mueve al menos US$ ", number(umbral_valor / 1e3), " M."), 85),
         x = "Participación en el comercio mundial del bien (%)", y = NULL,
         caption = env_wrap(paste0("Fuente: WITS (MAR_ALLPARTNERS_WLD, WLD_WLD), CUCI Rev.3. Entre paréntesis, valor de Marruecos. Nombres de los bienes: traducción propia de WITS. ",
                                   "Cortes de tamaño (>=", cortes_tamano["grande"], "% grande, >=", cortes_tamano["intermedio"],
                                   "% intermedio) son una convención del script, no del TP."), 125)) +
    theme_tp2() + theme(panel.grid.major.y = element_blank())
}

g_exp <- graficar_ranking("Exportaciones"); g_exp
ggsave(paste0(ruta_graficos, "g_ranking_participacion_exportaciones.png"), g_exp,
       width = 10, height = 7, dpi = 300, bg = color_fondo_tp2)

g_imp <- graficar_ranking("Importaciones"); g_imp
ggsave(paste0(ruta_graficos, "g_ranking_participacion_importaciones.png"), g_imp,
       width = 10, height = 7, dpi = 300, bg = color_fondo_tp2)

# --- G3: valor (eje x, log) vs participación (eje y, log): ¿grande o pequeño?
d3 <- ranking %>% filter(anio == anio_ref, valor_mar >= umbral_valor / 5, participacion > 0)
d3_lab <- d3 %>%
  group_by(flujo) %>%
  filter(rank_valor <= 3 | (!is.na(rank_part) & rank_part <= 3)) %>%
  ungroup() %>%
  mutate(etiqueta = paste0(cuci, " ", str_trunc(desc_es, 40)))

g3 <- ggplot(d3, aes(valor_mar / 1e3, participacion, color = tamano)) +
  geom_hline(yintercept = cortes_tamano, linetype = "dashed", color = color_texto_sec, linewidth = 0.4) +
  geom_point(size = 2.4, alpha = 0.85) +
  geom_text_repel(data = d3_lab, aes(label = etiqueta), family = "rajdhani", fontface = "bold",
                  size = 3.6, color = color_texto_tp2, min.segment.length = 0, box.padding = 0.5,
                  max.overlaps = Inf, seed = 1) +
  facet_wrap(~flujo) +
  scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_log10(labels = function(x) paste0(trimws(formatC(x, format = "fg", digits = 2, decimal.mark = ",")), "%")) +
  scale_color_manual(values = paleta_tamano, name = "Tamaño en el mercado") +
  labs(title = env_wrap(paste0("Valor comerciado vs. participación mundial, ", anio_ref), 55),
       subtitle = env_wrap("Cada punto es un bien (CUCI 3 dígitos). Se rotulan los 3 mayores por valor y los 3 de mayor participación. Líneas: cortes de tamaño.", 90),
       x = "Valor de Marruecos (millones de USD, escala log)", y = "Participación mundial (escala log)",
       caption = env_wrap(paste0("Fuente: WITS (MAR_ALLPARTNERS_WLD, WLD_WLD), CUCI Rev.3. Solo bienes con valor >= US$ ", number(umbral_valor / 5 / 1e3), " M."), 125)) +
  theme_tp2() + theme(panel.spacing = unit(2.5, "lines"))
g3
ggsave(paste0(ruta_graficos, "g_valor_vs_participacion.png"), g3,
       width = 11, height = 6.5, dpi = 300, bg = color_fondo_tp2)

#===============================================================================#
# FIN DEL SCRIPT 01. Para correr: abrir el proyecto (project.Rproj) y ejecutar entero.
#===============================================================================#