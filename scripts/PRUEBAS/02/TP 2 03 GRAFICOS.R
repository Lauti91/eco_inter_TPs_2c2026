#===============================================================================#
# TP2 - SCRIPT 3 de 3: GRÁFICOS
#===============================================================================#
# CONSOLIDACIÓN: reúne en un solo lugar TODO el código de graficación
# (ggplot + print + ggsave) que antes estaba disperso entre
# TP2_01_datos_y_modelo_base.R (11 gráficos) y TP2_02_oecd_correccion_y_
# robustez.R (2 gráficos). Los 13 gráficos quedan acá, en el mismo orden en
# que aparecían originalmente (Bloque 1 -> 2 -> 3 del Script 1, después
# Sección F -> G del Script 2), agrupados con el mismo encabezado de bloque
# que tenían antes, para que sea fácil ubicar cada uno en el TP.
#
# QUÉ SE MOVIÓ Y QUÉ NO: se movió exclusivamente la construcción del
# objeto ggplot, el print() implícito (la línea suelta "g_xxx") y el
# ggsave(). La preparación de datos que alimenta cada gráfico (tablas
# intermedias, audits, regresiones) se queda donde estaba en los Scripts 1
# y 2, porque en varios casos esa preparación también se usa para otras
# tablas/audits que no son gráficos (ejemplo: kl_benchmark alimenta tanto
# g_kl como audit_tendencia_kl). Separarla habría implicado duplicar
# cálculos o arriesgar romper esas dependencias - no era necesario para
# cumplir el pedido de "un script que se dedique exclusivamente a los
# gráficos".
#
# DEPENDE DE: TP2_01_datos_y_modelo_base.R Y TP2_02_oecd_correccion_y_
# robustez.R corridos ANTES que este, en ese orden. Este script no vuelve a
# cargar datos ni a calibrar nada - solo referencia objetos que esos dos ya
# dejaron en el entorno (paletas, res_A/res_B, kl_benchmark, vcrn_mfe,
# productividad_indexada, fpp_grilla/fpp_observado/fpp_familia,
# tot_marruecos, precios_internacionales, reservas_eras, labsh_marruecos,
# ied_origen_pais_2020, leontief_vs_cd, serie_convergencia, fmt_mil(),
# ruta_graficos, paleta_sectores, paleta_paises_ho).
#
# Todos los ggsave() se mantienen apuntando a ruta_graficos (definida en
# Script 1), así que los archivos .png se siguen escribiendo en
# output/graficos/02/ con los mismos nombres de antes - ningún gráfico
# cambia de ruta ni de nombre de archivo por este reordenamiento.
#
# RONDA DE ESTÉTICA 1: se reemplaza theme_tp1() (heredado del TP1,
# genérico) por un tema propio de este script, theme_tp2(), adaptado del
# estilo usado en otro proyecto (Global Automation Atlas, Ciencia de
# Datos): fondo claro, tipografía Rajdhani vía showtext, sin ticks en los
# ejes.
#
# RONDA DE ESTÉTICA 2 (esta edición, corrige dos problemas señalados
# después de ver los .png renderizados):
#
#  (a) PALETA DEMASIADO AZUL. En la Ronda 1 el título, los ejes y hasta el
#      fondo quedaron en tonos azules (el azul de paleta_sectores["Fosfatos"]
#      aplicado también al TEXTO, no solo a los datos), lo que no tiene
#      relación con Marruecos (bandera roja y verde) y competía visualmente
#      con el azul y el rojo que sí son colores de DATOS (Fosfatos/
#      Automotor). Se corrige separando dos roles que la Ronda 1 mezclaba:
#      - Color de DATOS: paleta_sectores y paleta_paises_ho, sin cambios -
#        eso es lo que efectivamente identifica a Marruecos/fosfatos/
#        automotor en cada gráfico y se mantiene.
#      - Color de TEXTO/CHROME (títulos, ejes, grilla, fondo): pasa a una
#        escala neutra gris carbón / gris cálido, no azul. El rojo
#        (color_acento_tp2, el mismo de paleta_sectores["Automotor"], que
#        además es el rojo de la bandera) se reserva para llamar la
#        atención sobre un dato puntual (un callout, un punto destacado),
#        nunca para texto general.
#
#  (b) TEXTO QUE SE SALE DEL GRÁFICO. ggplot no hace word-wrap automático
#      de títulos/subtítulos/captions: si el texto es más largo que el
#      ancho de la imagen, se corta en el borde en vez de bajar de línea.
#      Varios títulos, subtítulos y captions (algunos armados con paste0()
#      e información numérica) eran más largos que el ancho disponible y
#      quedaban cortados a la derecha o, en el caso de un eje Y rotado muy
#      largo (g_caja_asignacion), se superponían con el título. Se corrige
#      con env_wrap() (definida abajo, un wrapper de stringr::str_wrap())
#      aplicado a todo texto de títulos/subtítulos/captions, con anchos
#      calibrados para el tamaño de fuente de cada elemento, más el eje Y
#      de g_caja_asignacion acortado (el detalle que sacó se movió al
#      caption, que ahora sí entra en el ancho de la imagen).


#===============================================================================#
# ESTÉTICA COMÚN A TODO EL SCRIPT
#===============================================================================#

library(showtext)
font_add_google("Rajdhani", "rajdhani")
showtext_auto()
showtext_opts(dpi = 300)

# --- Roles de color: DATOS vs. CHROME (ver nota (a) más arriba) ---
# Datos: se reutilizan las paletas ya definidas en el Script 1
# (paleta_sectores, paleta_paises_ho) - no se tocan.
# Chrome (texto, ejes, grilla, fondo): escala neutra, sin azul.
color_fondo_tp2   <- "#FAFAF8"   # blanco cálido, no celeste
color_texto_tp2   <- "#262421"   # gris carbón (casi negro): ejes, texto de datos, chrome general
color_texto_sec   <- "#6B6660"   # gris cálido secundario (subtítulos, captions)
color_grilla_tp2  <- "#E4E1DB"   # gris cálido claro, no azulado
color_acento_tp2  <- unname(paleta_sectores["Automotor"])  # rojo vivo - solo para callouts puntuales sobre datos
color_navy_dato   <- unname(paleta_sectores["Fosfatos"])   # azul - solo como color de SERIE, nunca de texto
color_titulo_tp2  <- "#7A1128"   # rojo-bordó (familia del rojo de la bandera, oscurecido para
# que un bloque grande de texto en negrita se lea bien) - SOLO
# para el título principal de cada gráfico; el resto del chrome
# (ejes, subtítulo, caption) se mantiene en gris neutro para no
# saturar la pieza de color.

# --- Wrap de texto: evita que títulos/subtítulos/captions se corten en el
# borde de la imagen (ver nota (b) más arriba). Los anchos están calibrados
# para el tamaño de fuente de cada elemento en una imagen de ~10 in de
# ancho a 300 dpi; si algún gráfico usa una fuente o un ancho de imagen
# distinto, ajustar el "width" en ese llamado puntual.
env_wrap <- function(x, width = 50) paste(stringr::str_wrap(x, width = width), collapse = "\n")

theme_tp2 <- function() {
  theme_minimal(base_family = "rajdhani") +
    theme(
      plot.background   = element_rect(fill = color_fondo_tp2, color = NA),
      panel.background  = element_rect(fill = color_fondo_tp2, color = NA),
      panel.grid.major  = element_line(color = color_grilla_tp2, linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(color = color_titulo_tp2, face = "bold", size = 22,
                                       hjust = 0, lineheight = 1.02, margin = margin(b = 3)),
      plot.subtitle     = element_text(color = color_texto_sec, size = 13, lineheight = 1.05,
                                       margin = margin(b = 10)),
      plot.caption      = element_text(color = color_texto_sec, size = 9.6, hjust = 0,
                                       lineheight = 1.1, margin = margin(t = 8)),
      axis.title        = element_text(color = color_texto_sec, size = 12.5, face = "bold"),
      axis.text         = element_text(color = color_texto_tp2, size = 12, face = "bold"),
      axis.ticks        = element_blank(),
      legend.text       = element_text(color = color_texto_sec, size = 11),
      legend.title      = element_text(color = color_texto_tp2, size = 12, face = "bold"),
      legend.position   = "bottom",
      legend.box        = "vertical",
      legend.margin     = margin(t = 0, b = 0),
      strip.text        = element_text(color = color_texto_tp2, face = "bold", size = 12),
      plot.margin       = margin(18, 34, 14, 18)
    )
}

# Rampa navy -> rojo para series ordinales (años): mantiene la identidad
# cromática del TP en gráficos donde el color codifica una progresión
# temporal en vez de una categoría fija (ver g_fpp y g_fpp_desplazamiento).
# Esto sigue siendo color de DATOS, no de chrome, así que puede usar el navy.
rampa_tp2 <- function(n) colorRampPalette(c(color_navy_dato, color_acento_tp2))(n)


#===============================================================================#
# BLOQUE 1: PUNTO DE PARTIDA
#===============================================================================#

## --- VCRN de fosfatos vs. automotor (TP1, retomado) ---
# Depende de: vcrn_mfe (Script 1, Bloque 1).
# Sin leyenda de "Producto" (4 categorías no entraban en el ancho de la
# imagen sin cortarse) - se reemplaza por una etiqueta directa al final de
# cada línea, igual que en g_kl.
g_vcrn_mfe <- ggplot(vcrn_mfe, aes(x = year, y = vcrn, color = grupo, linetype = sector_desc, group = interaction(grupo, sector_desc))) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.6) +
  ggrepel::geom_text_repel(data = vcrn_mfe |> group_by(sector_desc) |> filter(year == max(year)) |> ungroup(),
                           aes(label = sector_desc), fontface = "bold", family = "rajdhani", size = 3.6,
                           direction = "y", hjust = 0, nudge_x = 0.15, segment.color = NA,
                           xlim = c(NA, Inf), show.legend = FALSE) +
  scale_color_manual(values = paleta_sectores, guide = "none") +
  scale_linetype_discrete(guide = "none") +
  scale_x_continuous(breaks = unique(vcrn_mfe$year), expand = expansion(mult = c(0.02, 0.22))) +
  coord_cartesian(clip = "off") +
  labs(title = env_wrap("VCRN de fosfatos vs. automotor, 2021-2025"),
       subtitle = env_wrap("Ventaja comparativa revelada normalizada (Balassa). Color = sector, trazo = producto.", 80),
       x = "Año", y = "VCRN",
       caption = env_wrap("Fuente: bases WITS del TP1 (vcr_mar).", 125)) +
  theme_tp2() + theme(plot.margin = margin(18, 70, 14, 18))
g_vcrn_mfe
ggsave(paste0(ruta_graficos, "grafico_vcrn_mfe.png"), g_vcrn_mfe, width = 10, height = 6.5, dpi = 300, bg = color_fondo_tp2)


#===============================================================================#
# BLOQUE 2: MODELO DE FACTORES ESPECÍFICOS (corto plazo) - alpha de HCP
#===============================================================================#

## --- 2.2 Productividad media del trabajo, evolución comparada (índice base 100) ---
# Depende de: productividad_indexada (Script 1, sub-bloque 2.2).
g_productividad <- ggplot(productividad_indexada, aes(x = year, y = indice, color = sector)) +
  geom_hline(yintercept = 100, linetype = "dotted", color = color_texto_sec, linewidth = 0.6) +
  annotate("text", x = min(productividad_indexada$year), y = 100, label = "Base 100",
           color = color_texto_sec, size = 3.4, family = "rajdhani", fontface = "bold",
           vjust = -0.8, hjust = 0) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.6) +
  scale_color_manual(values = paleta_sectores) +
  labs(title = env_wrap("Productividad media del trabajo, evolución comparada"),
       subtitle = env_wrap(paste0("Producción física por trabajador, índice base 100 = primer año de cada serie (",
                                  paste(sort(unique(productividad_indexada$anio_base)), collapse = " y "), ")"), 80),
       x = "Año", y = "Índice (base 100)", color = NULL,
       caption = env_wrap(paste0("Fosfatos en miles de toneladas/trabajador, automotor en vehículos/trabajador: ",
                                 "los NIVELES no son comparables entre sectores (unidades físicas distintas), ",
                                 "por eso se comparan las evoluciones y no los valores absolutos."), 125)) +
  theme_tp2()
g_productividad
ggsave(paste0(ruta_graficos, "grafico_productividad_mfe.png"), g_productividad, width = 10, height = 6.5, dpi = 300, bg = color_fondo_tp2)

## --- 2.3 FPP real (plano Q_fosfatos x Q_automotor) ---
# Depende de: fpp_grilla, fpp_observado, anio_ref_fpp, L_total (Script 1, sub-bloque 2.3).
g_fpp <- ggplot() +
  geom_path(data = fpp_grilla, aes(x = Q_fosfatos, y = Q_auto), linewidth = 1.2, color = color_texto_sec) +
  geom_point(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, color = factor(year)), size = 3.4) +
  ggrepel::geom_text_repel(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, label = year),
                           color = color_texto_tp2, size = 3.6, fontface = "bold", family = "rajdhani",
                           segment.color = color_grilla_tp2, min.segment.length = 0) +
  scale_color_manual(values = setNames(rampa_tp2(nrow(fpp_observado)), as.character(sort(fpp_observado$year))),
                     guide = "none") +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = env_wrap("FPP empírica de Marruecos: fosfatos vs. automotor"),
       subtitle = env_wrap(paste0("Curva calibrada con Cobb-Douglas sobre L = ", fmt_mil(L_total),
                                  " trabajadores (", anio_ref_fpp, ")"), 80),
       x = "Producción de fosfatos (miles de toneladas)", y = "Producción automotriz (unidades)") +
  theme_tp2()
g_fpp
ggsave(paste0(ruta_graficos, "grafico_fpp_real.png"), g_fpp, width = 10, height = 7, dpi = 300, bg = color_fondo_tp2)

## --- 2.3 Desplazamiento de la FPP, año a año ---
# Depende de: fpp_familia, fpp_observado, anios_fpp_familia (Script 1, sub-bloque 2.3).
g_fpp_desplazamiento <- ggplot() +
  geom_path(data = fpp_familia, aes(x = Q_fosfatos, y = Q_auto, color = factor(year), group = year), linewidth = 1.1) +
  geom_point(data = fpp_observado |> filter(year %in% anios_fpp_familia),
             aes(x = Q_fosfatos, y = Q_auto, color = factor(year)), size = 2.8) +
  scale_color_manual(values = setNames(rampa_tp2(length(anios_fpp_familia)), as.character(sort(anios_fpp_familia))),
                     name = "Año") +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = env_wrap("Desplazamiento de la FPP de Marruecos, año a año"),
       x = "Producción de fosfatos (miles de toneladas)", y = "Producción automotriz (unidades)") +
  theme_tp2()
g_fpp_desplazamiento
ggsave(paste0(ruta_graficos, "grafico_fpp_desplazamiento.png"), g_fpp_desplazamiento, width = 10, height = 7, dpi = 300, bg = color_fondo_tp2)

## --- 2.4 Caja de asignación: VPMgL sobre VALOR AGREGADO ---
# Depende de: res_A (Script 1, sub-bloque 2.4), anio_ref_fpp, fmt_mil(), paleta_sectores.
# El eje Y (antes una sola línea muy larga que se superponía con el título)
# se acorta; el detalle completo de la fórmula pasa al caption, que ahora
# sí entra en el ancho de la imagen gracias a env_wrap().
# L_tot_caja: se recalcula localmente a partir de la propia grilla de
# res_A (en vez de reusar el L_total global) para que el eje secundario de
# abajo sea exacto pase lo que pase con el año de referencia usado en la
# calibración.
L_tot_caja <- res_A$grilla$L_fosfatos[1] + res_A$grilla$L_auto[1]
g_caja_asignacion <- ggplot(res_A$grilla, aes(x = L_fosfatos)) +
  geom_line(aes(y = vpmgl_fosfatos, color = "Fosfatos"), linewidth = 1.2) +
  geom_line(aes(y = vpmgl_auto, color = "Automotor"), linewidth = 1.2) +
  geom_vline(xintercept = res_A$L_f_eq, linetype = "dashed", color = color_texto_sec) +
  geom_hline(yintercept = res_A$w_eq, linetype = "dashed", color = color_texto_sec) +
  geom_vline(xintercept = res_A$L_f0, linetype = "dotted", color = color_texto_tp2, linewidth = 0.8) +
  annotate("point", x = res_A$L_f0, y = res_A$vpmgl_f_obs, size = 3.2, color = paleta_sectores["Fosfatos"]) +
  annotate("point", x = res_A$L_f0, y = res_A$vpmgl_a_obs, size = 3.2, color = paleta_sectores["Automotor"]) +
  annotate("text", x = res_A$L_f0, y = res_A$vpmgl_f_obs, label = "Observado",
           vjust = -1, hjust = -0.05, size = 3.4, color = color_texto_sec, family = "rajdhani", fontface = "bold") +
  scale_color_manual(values = paleta_sectores) +
  scale_y_log10(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  # EJE X DE DOBLE LECTURA (como la "caja de Edgeworth" de MFE del apunte
  # de cátedra): el eje inferior se lee de izquierda a derecha como trabajo
  # de fosfatos (0 a L); el eje superior (sec_axis) se lee de derecha a
  # izquierda como trabajo de automotor, porque L_automotor = L_tot -
  # L_fosfatos. Es el mismo eje horizontal, solo que mirado desde los dos
  # extremos - no son dos variables distintas.
  scale_x_continuous(
    # OJO CON LOS GLIFOS: "→"/"←" (Unicode) no existen en Rajdhani y salían
    # como un cuadrado vacío ("tofu"). Se reemplazan por flechas ASCII
    # ("->"/"<-"), que cualquier fuente tiene.
    name = "Trabajo asignado a fosfatos ->",
    labels = scales::label_number(big.mark = ".", decimal.mark = ","),
    sec.axis = sec_axis(~ L_tot_caja - ., name = "<- Trabajo asignado a automotor",
                        labels = scales::label_number(big.mark = ".", decimal.mark = ","))
  ) +
  labs(title = env_wrap("Caja de asignación del trabajo — MFE"),
       subtitle = env_wrap(paste0("Equilibrio teórico: w* ≈ ", fmt_mil(round(res_A$w_eq * 1e6 / 12)),
                                  " MAD/mes en L_fosfatos ≈ ", fmt_mil(round(res_A$L_f_eq)),
                                  " — Observado ", anio_ref_fpp, ": L_fosfatos = ", fmt_mil(res_A$L_f0)), 80),
       y = "VPMgL (escala log)", color = NULL,
       caption = env_wrap(paste0("Oferta total de trabajo (fosfatos + automotor): L = ", fmt_mil(round(L_tot_caja)),
                                 ". Brecha en el punto observado: ", round(res_A$brecha_obs, 1),
                                 "x - Escenario A (HCP). Fórmula: (1-alpha) x VA/L, es decir la derivada del valor agregado, ",
                                 "en millones de MAD por trabajador/año sobre valor agregado. ",
                                 "Eje Y en escala logarítmica: VPMgL diverge cerca de los bordes de la caja por rendimientos decrecientes."), 125)) +
  theme_tp2() +
  # Coloreo cruzado de los dos ejes X para que la doble lectura sea obvia a
  # simple vista y no dependa de que se lea el texto del título de eje: el
  # eje de ABAJO (fosfatos) queda del mismo azul que la curva de fosfatos,
  # el eje de ARRIBA (automotor) del mismo rojo que la curva de automotor.
  theme(plot.margin = margin(30, 34, 14, 20),
        axis.text.x           = element_text(color = color_navy_dato, face = "bold", size = 12),
        axis.title.x          = element_text(color = color_navy_dato, face = "bold", size = 12.5),
        axis.text.x.top       = element_text(color = color_acento_tp2, face = "bold", size = 12),
        axis.title.x.top      = element_text(color = color_acento_tp2, face = "bold", size = 12.5))
g_caja_asignacion
ggsave(paste0(ruta_graficos, "grafico_caja_asignacion.png"), g_caja_asignacion, width = 10, height = 6.5, dpi = 300, bg = color_fondo_tp2)

## --- 2.5 Términos de intercambio de Marruecos ---
# Depende de: tot_marruecos, anio_min_tot (Script 1, sub-bloque 2.5).
g_tot <- ggplot(tot_marruecos, aes(x = year, y = tot)) +
  geom_line(linewidth = 1.2, color = color_navy_dato) +
  geom_point(size = 2.4, color = color_navy_dato) +
  annotate("point", x = tot_marruecos$year[which.max(tot_marruecos$year)],
           y = tot_marruecos$tot[which.max(tot_marruecos$year)], size = 3.6, color = color_acento_tp2) +
  ggrepel::geom_text_repel(data = tot_marruecos |> filter(year == max(year)),
                           aes(label = paste0("Último dato: ", round(tot, 1))),
                           color = color_acento_tp2, size = 3.8, fontface = "bold", family = "rajdhani",
                           nudge_y = 6, segment.color = NA) +
  labs(title = env_wrap("Términos de intercambio de Marruecos"),
       subtitle = env_wrap(paste0("Índice de términos de intercambio de mercancías (2000=100) — cobertura real desde ", anio_min_tot), 80),
       x = "Año", y = "Índice ToT",
       caption = env_wrap(paste0("Fuente: World Bank WDI (TT.PRI.MRCH.XD.WD). Sin dato antes de ", anio_min_tot,
                                 " para Marruecos en este indicador - no CEPAL, no cubre países fuera de América Latina/Caribe."), 125)) +
  theme_tp2()
g_tot
ggsave(paste0(ruta_graficos, "grafico_tot.png"), g_tot, width = 10, height = 6, dpi = 300, bg = color_fondo_tp2)

## --- 2.5 Precio internacional de fertilizantes (DAP y roca fosfórica) ---
# Depende de: precios_internacionales (Script 1, Bloque 0.5). El pivot_longer()
# es puramente para el gráfico (dos series en un mismo eje de color).
g_precios <- precios_internacionales |>
  pivot_longer(cols = c(dap_usd_mt, roca_fosforica_usd_mt), names_to = "producto", values_to = "precio") |>
  mutate(producto = recode(producto, dap_usd_mt = "DAP", roca_fosforica_usd_mt = "Roca fosfórica")) |>
  ggplot(aes(x = year, y = precio, color = producto)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.4) +
  scale_color_manual(values = c("DAP" = color_navy_dato, "Roca fosfórica" = color_acento_tp2)) +
  labs(title = env_wrap("Precio internacional de fertilizantes"),
       subtitle = env_wrap("USD por tonelada métrica, f.o.b. — serie ampliada desde 1990 (Ronda 4)", 80),
       x = "Año", y = "USD/mt", color = NULL,
       caption = env_wrap("Fuente: World Bank Commodity Markets (Pink Sheet). Shock de 2007-2008: la roca fosfórica se multiplicó por 8x.", 125)) +
  theme_tp2()
g_precios
ggsave(paste0(ruta_graficos, "grafico_precios_internacionales.png"), g_precios, width = 10, height = 6, dpi = 300, bg = color_fondo_tp2)

## --- 2.6 Reservas de roca fosfórica: antes/después de la reclasificación ---
# Depende de: reservas_eras, variacion_pct_reservas (Script 1, sub-bloque 2.6).
g_reservas <- ggplot(reservas_eras, aes(x = etiqueta_periodo, y = gt)) +
  geom_col(fill = color_navy_dato, width = 0.55, color = color_fondo_tp2, linewidth = 1) +
  geom_text(aes(label = etiqueta_valor), vjust = -0.6, size = 4.4, fontface = "bold",
            family = "rajdhani", color = color_texto_tp2) +
  annotate("segment", x = 1, xend = 2,
           y = max(reservas_eras$gt) * 1.15, yend = max(reservas_eras$gt) * 1.15,
           arrow = arrow(length = unit(0.2, "cm")), color = color_acento_tp2, linewidth = 0.8) +
  annotate("text", x = 1.5, y = max(reservas_eras$gt) * 1.25,
           label = env_wrap(paste0("+", variacion_pct_reservas, "% (reclasificación IFDC/USGS, no descubrimiento físico)"), 40),
           size = 3.3, color = color_acento_tp2, family = "rajdhani", fontface = "bold", lineheight = 0.95) +
  scale_y_continuous(limits = c(0, max(reservas_eras$gt) * 1.45)) +
  labs(title = env_wrap("Reservas de roca fosfórica reportadas — Marruecos"),
       subtitle = env_wrap("Dos vintages de estimación oficial, no una trayectoria continua", 80),
       x = NULL, y = "Reservas reportadas (Gt)",
       caption = env_wrap("Fuente: USGS Mineral Commodity Summaries; IFDC (Van Kauwenbergh, 2010).", 125)) +
  theme_tp2()
g_reservas
ggsave(paste0(ruta_graficos, "grafico_reservas_fosfatos.png"), g_reservas, width = 10, height = 6, dpi = 300, bg = color_fondo_tp2)


#===============================================================================#
# BLOQUE 3: MODELO DE HECKSCHER-OHLIN (largo plazo)
#===============================================================================#

## --- 3.2 Capital por trabajador (K/L), Marruecos vs. benchmark europeo ---
# Depende de: kl_benchmark, paleta_paises_ho (Script 1, sub-bloque 3.2).
# Etiquetas directas al final de cada línea en vez de leyenda: evita el
# problema ya documentado de que dos países queden visualmente parecidos
# en una leyenda de color.
g_kl <- kl_benchmark |>
  ggplot(aes(x = year, y = kl, color = country)) +
  geom_line(linewidth = 1.2) +
  ggrepel::geom_text_repel(data = kl_benchmark |> group_by(country) |> filter(year == max(year)) |> ungroup(),
                           aes(label = country), fontface = "bold", family = "rajdhani", size = 4,
                           direction = "y", hjust = 0, nudge_x = 2, segment.color = NA, xlim = c(NA, Inf)) +
  scale_color_manual(values = paleta_paises_ho, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.14))) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  coord_cartesian(clip = "off") +
  labs(title = env_wrap("Capital por trabajador (K/L)"),
       subtitle = env_wrap("Marruecos vs. Alemania, España y Francia — serie completa PWT", 80),
       x = "Año", y = "K/L (PWT rnna/emp)",
       caption = env_wrap("Fuente: Penn World Table 10.01 (1950-2019, cobertura completa del paquete).", 125)) +
  theme_tp2() + theme(plot.margin = margin(18, 65, 14, 18))
g_kl
ggsave(paste0(ruta_graficos, "grafico_kl_benchmark.png"), g_kl, width = 10, height = 6.5, dpi = 300, bg = color_fondo_tp2)

## --- 3.3 Participación del trabajo en el ingreso — Marruecos ---
# Depende de: labsh_marruecos, tendencia_labsh_mar, anios_labsh_real, pwt10.01
# (Script 1, sub-bloque 3.3).
g_labsh <- ggplot(labsh_marruecos, aes(x = year, y = labsh_real)) +
  geom_smooth(method = "lm", se = TRUE, color = color_texto_tp2, fill = color_texto_sec,
              linewidth = 0.9, alpha = 0.12) +
  geom_line(linewidth = 1.2, color = color_acento_tp2) +
  geom_point(size = 2, color = color_acento_tp2) +
  scale_y_continuous(limits = c(0.40, 0.60), breaks = seq(0.40, 0.60, 0.05)) +
  labs(title = env_wrap("Participación del trabajo en el ingreso"),
       subtitle = env_wrap(paste0("Insumo para testear Stolper-Samuelson de largo plazo (3d) — serie real PWT desde ",
                                  anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Morocco"],
                                  ". Sin tendencia estadísticamente significativa: pendiente ",
                                  signif(tendencia_labsh_mar$pendiente_anual, 2),
                                  " por año, p = ", round(tendencia_labsh_mar$p.value, 2),
                                  ", R2 = ", round(tendencia_labsh_mar$r.squared, 3)), 80),
       x = "Año", y = "labsh (PWT)",
       caption = env_wrap(paste0("Fuente: Penn World Table 10.01. Se excluye el tramo previo (",
                                 min(pwt10.01$year[pwt10.01$country == "Morocco"]), "-",
                                 anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Morocco"] - 1,
                                 "): PWT repite un valor plano imputado sin dato real detrás. ",
                                 "El eje Y se fija en 0,40-0,60 a propósito: con un eje ajustado a los datos, ",
                                 "una serie plana con ruido de centésimas parece una tendencia marcada."), 125)) +
  theme_tp2()
g_labsh
ggsave(paste0(ruta_graficos, "grafico_labsh_marruecos.png"), g_labsh, width = 10, height = 6.3, dpi = 300, bg = color_fondo_tp2)

## --- 3.3 bis Participación del trabajo en el ingreso — comparación internacional ---
# GRÁFICO NUEVO (no existía en los Scripts 1/2 - se agrega en esta ronda
# para contrastar la serie de Marruecos del gráfico anterior contra
# España, Alemania y Francia, como pidió el usuario). No hace falta tocar
# el Script 1: kl_benchmark ya trae labsh_real para los 4 países, cada uno
# recortado a su propio año de dato real (ver detección de imputación en
# el Script 1, sub-bloque 3.3) - se reutiliza tal cual, sin volver a
# calcular nada.
# Depende de: kl_benchmark, audit_tendencia_labsh, paleta_paises_ho (Script 1, sub-bloque 3.3).
g_labsh_comparado <- kl_benchmark |>
  filter(!is.na(labsh_real)) |>
  ggplot(aes(x = year, y = labsh_real, color = country)) +
  geom_line(linewidth = 1.1) +
  ggrepel::geom_text_repel(data = kl_benchmark |> filter(!is.na(labsh_real)) |>
                             group_by(country) |> filter(year == max(year)) |> ungroup(),
                           aes(label = country), fontface = "bold", family = "rajdhani", size = 3.8,
                           direction = "y", hjust = 0, nudge_x = 1.5, segment.color = NA, xlim = c(NA, Inf)) +
  scale_color_manual(values = paleta_paises_ho, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.16))) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  coord_cartesian(clip = "off") +
  labs(title = env_wrap("Participación del trabajo en el ingreso: comparación internacional"),
       subtitle = env_wrap("Marruecos vs. Alemania, España y Francia, cada serie desde su propio año de dato real PWT", 80),
       x = "Año", y = "labsh (PWT)",
       caption = env_wrap(paste0("Fuente: Penn World Table 10.01. Cada país arranca en el año en que su labsh deja de ser ",
                                 "un valor imputado plano (Script 1, sub-bloque 3.3): Marruecos desde ",
                                 anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Morocco"],
                                 ", España desde ", anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Spain"],
                                 ", Alemania desde ", anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Germany"],
                                 ", Francia con serie completa. De los cuatro, Marruecos es el único sin caída ",
                                 "estadísticamente significativa en su ventana de dato real."), 125)) +
  theme_tp2() + theme(plot.margin = margin(18, 65, 14, 18))
g_labsh_comparado
ggsave(paste0(ruta_graficos, "grafico_labsh_comparado.png"), g_labsh_comparado, width = 10, height = 6.3, dpi = 300, bg = color_fondo_tp2)

## --- 3.4 Origen del capital extranjero ---
# Depende de: ied_origen_pais_2020 (Script 1, sub-bloque 3.4).
# Estilo de barra horizontal con la barra principal resaltada, igual al
# gráfico de exposición por grupo SOC del otro proyecto: destaca el país
# de origen dominante y agrega la etiqueta de valor al final de cada barra
# en vez de forzar al lector a leer el eje.
g_ied_origen <- ied_origen_pais_2020 |>
  mutate(pais = reorder(pais, stock_mmad),
         destacar = if_else(stock_mmad == max(stock_mmad), "top", "resto")) |>
  ggplot(aes(x = stock_mmad, y = pais, fill = destacar)) +
  geom_col(width = 0.7, color = color_fondo_tp2, linewidth = 0.8) +
  geom_text(aes(label = fmt_mil(round(stock_mmad))), hjust = -0.15, color = color_texto_sec,
            size = 4.2, family = "rajdhani", fontface = "bold") +
  scale_fill_manual(values = c(top = color_acento_tp2, resto = color_navy_dato), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = env_wrap("Stock de IED en Marruecos por país de origen"),
       subtitle = env_wrap("2020, millones de MAD", 80),
       x = "Millones de MAD", y = NULL,
       caption = env_wrap("Fuente: Office des Changes (PEG 2020) / DG Trésor / UNCTADstat.", 125)) +
  theme_tp2()
g_ied_origen
ggsave(paste0(ruta_graficos, "grafico_ied_origen.png"), g_ied_origen, width = 10, height = 6, dpi = 300, bg = color_fondo_tp2)


#===============================================================================#
# SECCIÓN F (Script 2): COBB-DOUGLAS vs. LEONTIEF
#===============================================================================#

## --- ¿Hay margen técnico para el equilibrio teórico? ---
# Depende de: leontief_vs_cd, res_A (Script 2, Sección F).
g_leontief_vs_cd <- ggplot(leontief_vs_cd, aes(x = L_fosfatos)) +
  geom_line(aes(y = Q_cobb_douglas, color = "Cobb-Douglas (sustituible)"), linewidth = 1.2) +
  geom_line(aes(y = Q_leontief, color = "Leontief (coeficientes fijos)"), linewidth = 1.2) +
  geom_vline(xintercept = res_A$L_f0, linetype = "dotted", color = color_texto_sec) +
  geom_vline(xintercept = res_A$L_f_eq, linetype = "dashed", color = color_texto_sec) +
  scale_color_manual(values = c("Cobb-Douglas (sustituible)" = color_texto_sec,
                                "Leontief (coeficientes fijos)" = color_navy_dato)) +
  labs(title = env_wrap("¿Qué tecnología describe mejor a la minería de fosfatos?", 40),
       subtitle = env_wrap(paste0("Producción de fosfatos bajo dos supuestos tecnológicos, ",
                                  "calibrados en el mismo punto observado (", res_A$anio_ref, ")"), 80),
       x = "Trabajo asignado a fosfatos", y = "Producción de fosfatos (miles de toneladas)", color = NULL,
       caption = env_wrap(paste0("Empleo observado: ", fmt_mil(res_A$L_f0),
                                 ". Equilibrio Cobb-Douglas: ", fmt_mil(round(res_A$L_f_eq)),
                                 ". Techo Leontief a capacidad pico (", res_A$anio_Q_max, ", serie ", res_A$rango_Q,
                                 "): ", fmt_mil(round(res_A$L_cap_leontief)), "."), 125)) +
  theme_tp2()
g_leontief_vs_cd
ggsave(paste0(ruta_graficos, "grafico_leontief_vs_cobb_douglas.png"), g_leontief_vs_cd, width = 10, height = 6.5, dpi = 300, bg = color_fondo_tp2)


#===============================================================================#
# SECCIÓN G (Script 2): CONVERGENCIA DE INTENSIDADES FACTORIALES
#===============================================================================#

## --- Intensidad factorial por sector, 2014-2021 ---
# Depende de: serie_convergencia, paleta_sectores (Script 2, Sección G).
g_convergencia <- ggplot(serie_convergencia, aes(x = TIME_PERIOD, y = alpha_capital, color = sector)) +
  geom_point(size = 2.8) + geom_smooth(method = "lm", se = TRUE, linewidth = 1, alpha = 0.15) +
  scale_color_manual(values = c("Fosfatos (B08+C20)" = color_navy_dato,
                                "Automotor (C29+C27)" = color_acento_tp2)) +
  scale_x_continuous(breaks = 2014:2021) +
  labs(title = env_wrap("Intensidad factorial por sector, 2014-2021"),
       subtitle = env_wrap("Participación del capital en el VA, con los perímetros sectoriales corregidos", 80),
       x = "Año", y = "Alpha (participación del capital)", color = NULL,
       caption = env_wrap("Fuente: OECD SUT_USEVA (Marruecos). Peso real de roca/química en OCP solo disponible para 2021 (Ronda 3); 2014-2020 usan el promedio 2021-2023 como aproximación. La brecha se estrecha porque automotor sube, no porque fosfatos baje.", 125)) +
  theme_tp2()
g_convergencia
ggsave(paste0(ruta_graficos, "grafico_convergencia_alpha.png"), g_convergencia, width = 10, height = 6.5, dpi = 300, bg = color_fondo_tp2)


#===============================================================================#
# QUÉ QUEDÓ EN ESTE SCRIPT
#===============================================================================#
# 14 gráficos (13 originales + 1 nuevo), en el orden en que aparecen en el
# TP, todos con theme_tp2() (chrome neutro, título en rojo-bordó) y la
# paleta de DATOS ya establecida por paleta_sectores/paleta_paises_ho:
#  Bloque 1: g_vcrn_mfe
#  Bloque 2: g_productividad, g_fpp, g_fpp_desplazamiento, g_caja_asignacion,
#            g_tot, g_precios, g_reservas
#  Bloque 3: g_kl, g_labsh, g_labsh_comparado (NUEVO), g_ied_origen
#  Script 2, Sección F: g_leontief_vs_cd
#  Script 2, Sección G: g_convergencia
#
# Todos los .png se siguen escribiendo en ruta_graficos (output/graficos/02/),
# con los mismos nombres de archivo que tenían en los scripts originales -
# ningún gráfico existente se renombra ni se mueve de carpeta. El único
# archivo nuevo es grafico_labsh_comparado.png.
#
# RONDA DE ESTÉTICA 3 (esta edición, pedidos puntuales sobre 3 gráficos):
#  - g_caja_asignacion: se agregó un eje X secundario (arriba) que lee el
#    mismo eje horizontal en sentido inverso (L_tot - L_fosfatos), como la
#    caja de asignación clásica del apunte de cátedra (MFE) donde un mismo
#    eje se lee de izquierda a derecha para un sector y de derecha a
#    izquierda para el otro.
#  - g_fpp, g_fpp_desplazamiento, g_kl: los ejes que mostraban notación
#    científica ("6e+05") pasan a notación decimal con separador de miles
#    en formato local (scales::label_number(big.mark=".", decimal.mark=",")).
#  - g_labsh_comparado: gráfico nuevo que contrasta la participación del
#    trabajo en el ingreso de Marruecos contra España, Alemania y Francia
#    (reutiliza kl_benchmark del Script 1, que ya trae labsh_real filtrado
#    por año real de cada país - no hizo falta tocar el Script 1).
#
# SI FALTA LA FUENTE "Rajdhani" (sin conexión a Google Fonts en el momento
# de correr el script), font_add_google() tira error y corta todo el
# script. Alternativa rápida si eso pasa: comentar las líneas de showtext
# (library/font_add_google/showtext_auto/showtext_opts) y cambiar
# base_family = "rajdhani" por base_family = "" en theme_tp2() - se pierde
# la tipografía pero se conserva el resto de la estética (colores, fondo,
# sin ticks, wrap de texto, etc.).
#
# SI UN TÍTULO/SUBTÍTULO/CAPTION SE SIGUE CORTANDO al correrlo con datos
# distintos a los de esta consolidación (por ejemplo, una corrida futura
# con nombres de categoría más largos), el ajuste es el "width" del
# llamado a env_wrap() de ESE texto puntual - no hay que tocar el theme.