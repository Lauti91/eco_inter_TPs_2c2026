#===============================================================================#
# TP6 - Política Comercial - SCRIPT 2: PARTE (a) PARA LOS DOS PRODUCTOS
#   784 (autopartes, importación)  y  562 (fertilizantes, exportación)
#===============================================================================#
# QUÉ HACE
#   (1) Socios principales de Marruecos en cada producto (2023).
#   (2) Participación mundial de Marruecos en cada producto (CUCI, WLD_WLD) y,
#       para fertilizantes, a 6 dígitos HS 2017 (DAP, MAP, etc.) con los
#       exportadores mundiales de TP6_EXPORTADORES_WLD_FERT_HS17.dta.
#   (3) Contraste con EE.UU.: participación de Marruecos en las importaciones
#       estadounidenses de DAP y MAP, 2017-2024 (TP6_USA_M_SOCIOS_FERT_HS17.dta).
#   (4) Tabla resumen de la parte (a) para la diapositiva 1.
#   El arancel y el análisis de bienestar de autopartes pasan al script 03.
#
# AÑO DE REFERENCIA: 2023 (los datos de Marruecos como reportero en 2024-2025
# están inflados en la fuente; ver script 1).
#
# BASES (carpeta "bases de datos/", WITS, miles de USD)
#   - TP6_MAR_M_SOCIOS_784_781_773_SITC3.dta : importaciones de 784 por socio.
#   - TP6_MAR_X_SOCIOS_FERT_HS17.dta         : exportaciones de 6 líneas HS de
#                                              fertilizantes por socio.
#   - MAR_ALLPARTNERS_WLD.dta, WLD_WLD.dta   : totales de Marruecos (socio
#                                              "World") y mundiales, CUCI 3 díg.
#   - TP6_EXPORTADORES_WLD_FERT_HS17.dta     : exportadores mundiales, HS6.
#   - TP6_USA_M_SOCIOS_FERT_HS17.dta         : importaciones de EE.UU. por socio.
#   - cvd_cronologia_ocp.csv                 : fecha de la orden del CVD (id cvd02).
#
# NOTAS DE LECTURA (leer antes de usar los números)
#   - Socios de 562: la base tiene 6 líneas HS6 (310319, 310520, 310530,
#     310540, 310551, 310559). NO incluye 310311 (superfosfatos), que en 2023
#     es otra parte del valor de 562. El script imprime la cobertura de las
#     6 líneas sobre el total CUCI 562; los destinos son los de esas líneas.
#   - Mundo de fertilizantes a 6 dígitos = suma de exportaciones de los
#     reportadores individuales, EXCLUYENDO "European Union" (que se superpone
#     con sus miembros). Es un denominador aproximado (incluye comercio
#     intra-UE y solo los reportadores que declaran).
#   - Los cortes de tamaño (>=10% grande, >=3% intermedio) son una convención
#     del script 01, no del TP.
#===============================================================================#

for (p in c("tidyverse", "haven", "ggrepel", "scales", "showtext", "stringr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(tidyverse); library(haven); library(ggrepel); library(scales); library(showtext)

# Formato de números en todos los gráficos: coma decimal y punto de miles.
# Se redefinen number() y label_number() con ese formato por defecto (funciona con cualquier versión de scales).
number <- function(x, ..., big.mark = ".", decimal.mark = ",") scales::number(x, ..., big.mark = big.mark, decimal.mark = decimal.mark)
label_number <- function(..., big.mark = ".", decimal.mark = ",") scales::label_number(..., big.mark = big.mark, decimal.mark = decimal.mark)

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
# BLOQUE 0: PARÁMETROS Y ESTILO (idéntico al script 1)
#===============================================================================#
anio_ref      <- 2023
cortes_tamano <- c(grande = 10, intermedio = 3)   # mismos cortes que el script 01
n_socios      <- 6

font_add_google("Rajdhani", "rajdhani"); showtext_auto(); showtext_opts(dpi = 300)
color_fondo_tp2 <- "#FAFAF8"; color_texto_tp2 <- "#262421"; color_texto_sec <- "#6B6660"
color_grilla_tp2 <- "#E4E1DB"; color_titulo_tp2 <- "#7A1128"
color_rojo <- "#C1272D"; color_verde <- "#006233"; color_gris_dato <- "#B8B3AA"

env_wrap <- function(x, width = 50) paste(stringr::str_wrap(x, width = width), collapse = "\n")
theme_tp2 <- function() {
  theme_minimal(base_family = "rajdhani") +
    theme(
      plot.background  = element_rect(fill = color_fondo_tp2, color = NA),
      panel.background = element_rect(fill = color_fondo_tp2, color = NA),
      panel.grid.major = element_line(color = color_grilla_tp2, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title    = element_text(color = color_titulo_tp2, face = "bold", size = 22, hjust = 0,
                                   lineheight = 1.02, margin = margin(b = 3)),
      plot.subtitle = element_text(color = color_texto_sec, size = 13, lineheight = 1.05, margin = margin(b = 10)),
      plot.caption  = element_text(color = color_texto_sec, size = 9.6, hjust = 0, lineheight = 1.1, margin = margin(t = 8)),
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

a_chr <- function(x) trimws(as.character(haven::as_factor(x)))

# Lectura con control: si falta el archivo, el script se detiene (no se inventan datos)
leer_wits <- function(archivo) {
  ruta <- paste0(ruta_datos, archivo)
  if (!file.exists(ruta)) stop("Falta el archivo: ", ruta)
  b <- read_dta(ruta); names(b) <- tolower(names(b))
  b %>% transmute(
    prod = a_chr(productcode), desc = a_chr(productdescription),
    rep = a_chr(reportername), socio = a_chr(partnername),
    flujo = a_chr(tradeflowname), anio = as.integer(year), valor = as.numeric(tradevaluein1000usd))
}

clasificar <- function(p) {
  case_when(p >= cortes_tamano["grande"]     ~ "grande",
            p >= cortes_tamano["intermedio"] ~ "intermedio",
            TRUE                             ~ "pequeño")
}

# Nombres de países en español para los gráficos (los datos de WITS vienen en inglés).
# Si aparece un país que no está en la lista, queda en inglés y el script lo avisa.
es_pais <- c("France" = "Francia", "Spain" = "España", "Romania" = "Rumania", "Portugal" = "Portugal", "China" = "China",
             "Poland" = "Polonia", "Germany" = "Alemania", "Italy" = "Italia", "Turkey" = "Turquía", "Turkiye" = "Turquía",
             "Brazil" = "Brasil", "Djibouti" = "Yibuti", "India" = "India", "Argentina" = "Argentina", "United States" = "EE.UU.",
             "Bangladesh" = "Bangladés", "Morocco" = "Marruecos", "Saudi Arabia" = "Arabia Saudita", "Tunisia" = "Túnez",
             "Australia" = "Australia", "Egypt, Arab Rep." = "Egipto", "Russian Federation" = "Rusia", "Mexico" = "México",
             "Jordan" = "Jordania", "Canada" = "Canadá", "Indonesia" = "Indonesia", "Pakistan" = "Pakistán", "Ethiopia(excludes Eritrea)" = "Etiopía",
             "Ethiopia" = "Etiopía", "Kenya" = "Kenia", "Lithuania" = "Lituania", "Belgium" = "Bélgica", "Netherlands" = "Países Bajos",
             "Japan" = "Japón", "Korea, Rep." = "Corea del Sur", "Slovak Republic" = "Eslovaquia", "Czech Republic" = "Chequia")
nombre_es <- function(x) {
  falta <- setdiff(unique(x[!x %in% names(es_pais)]), "")
  if (length(falta) > 0) message("Sin traducción (queda en inglés): ", paste(falta, collapse = ", "))
  ifelse(x %in% names(es_pais), unname(es_pais[x]), x)
}

#===============================================================================#
# BLOQUE 1: SOCIOS PRINCIPALES (año base)
#===============================================================================#
# Se excluye el socio agregado "World" (si existiera). "Unspecified" queda en el
# denominador (es parte del total) pero no entra al ranking.
leer_socios <- function(archivo) leer_wits(archivo) %>% filter(socio != "World")

s784 <- leer_socios("TP6_MAR_M_SOCIOS_784_781_773_SITC3.dta") %>%
  filter(prod == "784", flujo == "Import", anio == anio_ref) %>%
  group_by(prod, flujo, socio) %>% summarise(valor = sum(valor), .groups = "drop")

s562 <- leer_socios("TP6_MAR_X_SOCIOS_FERT_HS17.dta") %>%
  filter(flujo == "Export", anio == anio_ref) %>%
  mutate(prod = "562") %>%                       # las 6 líneas HS6 se agregan a 562
  group_by(prod, flujo, socio) %>% summarise(valor = sum(valor), .groups = "drop")

socios <- bind_rows(s784, s562) %>%
  group_by(prod, flujo) %>%
  mutate(peso = 100 * valor / sum(valor),
         rank = if_else(socio == "Unspecified", NA_integer_,
                        min_rank(desc(if_else(socio == "Unspecified", NA_real_, valor))))) %>%
  ungroup() %>% filter(!is.na(rank), rank <= n_socios) %>%
  arrange(prod, rank) %>% select(prod, flujo, rank, socio, valor, peso)
print(socios)
write_csv(socios, paste0(ruta_tablas, "socios_784_562_", anio_ref, ".csv"))

# Control de cobertura: valor de las bases de socios vs. total CUCI de Marruecos
tot_cuci <- leer_wits("MAR_ALLPARTNERS_WLD.dta") %>%
  filter(socio == "World", anio == anio_ref,
         (prod == "784" & flujo == "Import") | (prod == "562" & flujo == "Export")) %>%
  select(prod, valor_cuci = valor)
cobertura <- bind_rows(s784, s562) %>% group_by(prod) %>% summarise(valor_socios = sum(valor), .groups = "drop") %>%
  left_join(tot_cuci, by = "prod") %>% mutate(cobertura_pct = 100 * valor_socios / valor_cuci)
cat("\nCobertura de las bases de socios sobre el total CUCI de Marruecos (", anio_ref, "):\n", sep = "")
print(cobertura)
write_csv(cobertura, paste0(ruta_tablas, "cobertura_socios_", anio_ref, ".csv"))

# Gráfico de socios (para la diapositiva 1)
g_soc_d <- socios %>%
  mutate(prod_lab = recode(prod, "784" = "Autopartes (784): origen de las importaciones",
                           "562" = "Fertilizantes (562): destino de las exportaciones"),
         etq = fct_reorder(paste(nombre_es(socio), prod), peso))
g_soc <- ggplot(g_soc_d, aes(peso, etq, fill = prod)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(number(peso, accuracy = 0.1), "%")), hjust = -0.1,
            family = "rajdhani", fontface = "bold", size = 4, color = color_texto_tp2) +
  facet_wrap(~prod_lab, scales = "free") +
  scale_y_discrete(labels = function(x) sub(" 784$| 562$", "", x)) +
  scale_fill_manual(values = c("784" = color_gris_dato, "562" = color_rojo), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = label_number(suffix = "%")) +
  labs(title = env_wrap(paste0("Socios principales de Marruecos, ", anio_ref), 55),
       subtitle = env_wrap(paste0("Los ", n_socios, " mayores socios de cada producto, en % del valor de los socios de cada base."), 90),
       x = "Participación en el flujo del producto (%)", y = NULL,
       caption = env_wrap(paste0("Fuente: WITS (TP6_MAR_M_SOCIOS_784_781_773_SITC3; TP6_MAR_X_SOCIOS_FERT_HS17). ",
                                 "562 = 6 líneas HS6 sin 310311 (cobertura sobre CUCI 562: ",
                                 number(cobertura$cobertura_pct[cobertura$prod == "562"], accuracy = 0.1), "%)."), 125)) +
  theme_tp2() + theme(panel.grid.major.y = element_blank())
g_soc
ggsave(paste0(ruta_graficos, "g_socios_784_562.png"), g_soc, width = 12, height = 6, dpi = 300, bg = color_fondo_tp2)

#===============================================================================#
# BLOQUE 2: PARTICIPACIÓN MUNDIAL
#===============================================================================#
# 2.1 CUCI 3 dígitos (WLD_WLD)
mundo_cuci <- leer_wits("WLD_WLD.dta") %>% select(prod, flujo, anio, valor_mundo = valor)
part_cuci <- leer_wits("MAR_ALLPARTNERS_WLD.dta") %>%
  filter(socio == "World", anio == anio_ref,
         (prod == "784" & flujo == "Import") | (prod == "562" & flujo == "Export")) %>%
  inner_join(mundo_cuci, by = c("prod", "flujo", "anio")) %>%
  transmute(prod, flujo, valor_mar = valor, valor_mundo, participacion = 100 * valor / valor_mundo,
            tamano = clasificar(participacion))
print(part_cuci)
write_csv(part_cuci, paste0(ruta_tablas, "participacion_cuci_784_562_", anio_ref, ".csv"))

# 2.2 Fertilizantes a 6 dígitos HS 2017: Marruecos vs. resto de exportadores
expo_fert <- leer_wits("TP6_EXPORTADORES_WLD_FERT_HS17.dta") %>%
  filter(flujo == "Export", rep != "European Union")   # evita doble conteo con miembros UE

fert_hs6 <- expo_fert %>%
  group_by(prod, anio) %>%
  mutate(mundo = sum(valor), participacion = 100 * valor / mundo,
         rank = min_rank(desc(valor))) %>%
  ungroup()

tab_fert <- fert_hs6 %>% filter(anio == anio_ref, rep == "Morocco") %>%
  transmute(prod, desc, valor_mar = valor, mundo, participacion, rank_mundial = rank,
            tamano = clasificar(participacion)) %>%
  arrange(desc(valor_mar))
print(tab_fert)
write_csv(tab_fert, paste0(ruta_tablas, "participacion_fertilizantes_hs6_", anio_ref, ".csv"))

# Top exportadores de DAP (310530) y MAP (310540), año base
top_exp <- fert_hs6 %>%
  filter(anio == anio_ref, prod %in% c("310530", "310540")) %>%
  group_by(prod) %>% slice_max(valor, n = 6) %>% ungroup() %>%
  mutate(prod_lab = recode(prod, "310530" = "DAP (310530)", "310540" = "MAP (310540)"),
         es_mar = rep == "Morocco",
         etq = fct_reorder(paste(nombre_es(rep), prod), participacion))
write_csv(top_exp %>% select(prod, rep, valor, participacion, rank),
          paste0(ruta_tablas, "top_exportadores_dap_map_", anio_ref, ".csv"))

g_exp <- ggplot(top_exp, aes(participacion, etq, fill = es_mar)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(number(participacion, accuracy = 0.1), "%")), hjust = -0.1,
            family = "rajdhani", fontface = "bold", size = 4, color = color_texto_tp2) +
  facet_wrap(~prod_lab, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub(" 3105[0-9]+$| 3103[0-9]+$", "", x)) +
  scale_fill_manual(values = c("TRUE" = color_rojo, "FALSE" = color_gris_dato), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)), labels = label_number(suffix = "%")) +
  labs(title = env_wrap(paste0("DAP y MAP: principales exportadores mundiales, ", anio_ref), 55),
       subtitle = env_wrap("Participación de cada país en las exportaciones mundiales del producto (HS 6 dígitos). Marruecos en rojo.", 90),
       x = "Participación en las exportaciones mundiales (%)", y = NULL,
       caption = env_wrap("Fuente: WITS (TP6_EXPORTADORES_WLD_FERT_HS17), HS 2017. Mundo = suma de reportadores individuales, sin 'European Union'.", 125)) +
  theme_tp2() + theme(panel.grid.major.y = element_blank())
g_exp
ggsave(paste0(ruta_graficos, "g_dap_map_exportadores.png"), g_exp, width = 11, height = 6, dpi = 300, bg = color_fondo_tp2)

#===============================================================================#
# BLOQUE 3: CONTRASTE CON EE.UU. (DAP y MAP)
#===============================================================================#
usa <- leer_wits("TP6_USA_M_SOCIOS_FERT_HS17.dta") %>%
  filter(flujo == "Import", prod %in% c("310530", "310540")) %>%
  group_by(prod, anio) %>%
  summarise(m_marruecos = sum(valor[socio == "Morocco"]),
            m_total = sum(valor[socio != "World"]),
            part_mar = 100 * m_marruecos / m_total, .groups = "drop") %>%
  mutate(prod_lab = recode(prod, "310530" = "DAP (310530)", "310540" = "MAP (310540)"))
print(usa)
write_csv(usa, paste0(ruta_tablas, "usa_importaciones_dap_map_desde_marruecos.csv"))

# Fecha de la orden del CVD: se lee de la cronología con fuente (no se escribe a mano)
ruta_cvd <- paste0(ruta_datos, "cvd_cronologia_ocp.csv")
if (!file.exists(ruta_cvd)) stop("Falta el archivo: ", ruta_cvd)
fecha_orden <- read_csv(ruta_cvd, show_col_types = FALSE) %>% filter(id == "cvd02") %>% pull(fecha_publicacion)
stopifnot(length(fecha_orden) == 1)
anio_orden <- as.integer(format(as.Date(fecha_orden), "%Y"))

# Subtítulo calculado con los datos: promedio de cuota antes de la orden y cuota en el año base
pre  <- usa %>% filter(anio < anio_orden) %>% group_by(prod_lab) %>%
  summarise(pre = mean(part_mar), a0 = min(anio), a1 = max(anio), .groups = "drop")
post <- usa %>% filter(anio == anio_ref) %>% select(prod_lab, post = part_mar)
sub_usa <- pre %>% inner_join(post, by = "prod_lab") %>%
  mutate(txt = paste0(prod_lab, ": ", number(pre, accuracy = 0.1), "% (promedio ", a0, "-", a1, ") -> ",
                      number(post, accuracy = 0.1), "% en ", anio_ref)) %>% pull(txt)

g_usa <- ggplot(usa, aes(anio, part_mar, color = prod_lab)) +
  geom_vline(xintercept = anio_orden + (as.numeric(as.Date(fecha_orden) - as.Date(paste0(anio_orden, "-01-01"))) / 365),
             linetype = "dashed", color = color_texto_sec) +
  annotate("text", x = anio_orden + 0.1, y = Inf, label = paste0("Orden CVD (", format(as.Date(fecha_orden), "%d/%m/%Y"), ")"),
           hjust = 0, vjust = 1.6, family = "rajdhani", fontface = "bold", size = 4, color = color_texto_sec) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.8) +
  scale_color_manual(values = c("DAP (310530)" = color_rojo, "MAP (310540)" = color_verde), name = NULL) +
  scale_x_continuous(breaks = sort(unique(usa$anio))) +
  scale_y_continuous(labels = label_number(suffix = "%")) +
  labs(title = env_wrap("EE.UU.: participación de Marruecos en las importaciones de DAP y MAP", 55),
       subtitle = env_wrap(paste0("Cuota de Marruecos en el valor importado por EE.UU. ", paste(sub_usa, collapse = " | ")), 90),
       x = NULL, y = "Participación en las importaciones de EE.UU. (%)",
       caption = env_wrap("Fuente: WITS (TP6_USA_M_SOCIOS_FERT_HS17), HS 2017; fecha de la orden: cvd_cronologia_ocp.csv (Federal Register). Datos reportados por EE.UU. (no sufren el inflado de 2024 de Marruecos).", 125)) +
  theme_tp2()
g_usa
ggsave(paste0(ruta_graficos, "g_usa_dap_map_marruecos.png"), g_usa, width = 10, height = 6.5, dpi = 300, bg = color_fondo_tp2)

#===============================================================================#
# BLOQUE 4: TABLA RESUMEN DE LA PARTE (a) (diapositiva 1)
#===============================================================================#
top3_socios <- socios %>% filter(rank <= 3) %>% arrange(prod, rank) %>%
  group_by(prod) %>%
  summarise(top3_socios = paste0(nombre_es(socio), " (", number(peso, accuracy = 0.1), "%)", collapse = "; "), .groups = "drop")

resumen_a <- part_cuci %>%
  left_join(top3_socios, by = "prod") %>%
  mutate(producto = recode(prod, "784" = "784 Partes y accesorios de vehículos (autopartes)",
                           "562" = "562 Abonos manufacturados (fertilizantes)"),
         valor_mar_mM = valor_mar / 1e6) %>%
  select(producto, flujo, valor_mar_mM, participacion, tamano, top3_socios)

# Filas de DAP y MAP (6 dígitos): es el producto elegido para fertilizantes (ver nota de datos)
dapmap_a <- tab_fert %>% filter(prod %in% c("310530", "310540")) %>%
  transmute(producto = recode(prod, "310530" = "310530 DAP (fosfato diamónico)", "310540" = "310540 MAP (fosfato monoamónico)"),
            flujo = "Export", valor_mar_mM = valor_mar / 1e6, participacion, tamano,
            top3_socios = paste0("puesto ", rank_mundial, " entre los exportadores mundiales"))
stopifnot(nrow(dapmap_a) == 2)
resumen_a <- bind_rows(resumen_a, dapmap_a)
print(resumen_a, width = 200)
write_csv(resumen_a, paste0(ruta_tablas, "resumen_parte_a_", anio_ref, ".csv"))

#===============================================================================#
# FIN DEL SCRIPT 02. Salidas: tablas en output/tablas/03/, gráficos en output/graficos/03/.
#===============================================================================#