#===============================================================================#
# TP6 - Política Comercial - SCRIPT 4: FERTILIZANTES DAP/MAP, INCISOS (b) y (c)
#   Instrumento: derecho compensatorio (CVD) de EE.UU. sobre las exportaciones de
#   OCP (Marruecos). Marruecos es exportador GRANDE en DAP/MAP (ver script 02).
#===============================================================================#
# QUÉ HACE
#   (b) Instrumento, magnitud y objetivo
#       - Tasa del depósito del CVD en vigor (19,97% -> 2,12% -> 16,81% -> 16,60%),
#         con fecha de vigencia y estado de verificación de cada tasa.
#       - Tasa promedio por año (ponderada por días) y arancel NMF de EE.UU. (0%).
#       - Objetivo declarado (citas con su estado de verificación).
#   (c) Bienestar (país grande, equilibrio parcial con tres mercados)
#       - Orden de magnitud de la recaudación de EE.UU. (c + e = t x M) con rango,
#         para DAP/MAP y para el alcance de la orden.
#       - Evidencia de ajuste: importaciones de EE.UU. desde Marruecos (valor y
#         toneladas), desvío hacia otros orígenes, precio unitario relativo,
#         destinos de las exportaciones de Marruecos.
#       - Esquema de tres mercados (importador, mercado mundial, exportador) con
#         las áreas de la clase: a, b, c, d, e (EE.UU.) y e, f (Marruecos).
#
# BASES (carpeta "bases de datos/"). Todo dato empírico sale de estos archivos:
#   - cvd_cronologia_ocp.csv               : eventos del CVD con cita del Federal Register.
#   - cvd_tasas_deposito_ocp.csv           : tasa del depósito y fecha desde la cual rige.
#   - cvd_alcance_orden.csv                : líneas HTS del alcance de la orden.
#   - arancel_usa_dap_map.csv              : arancel NMF de EE.UU. a DAP/MAP (HTS).
#   - citas_objetivos.csv                  : objetivos declarados, con estado de verificación.
#   - TP6_USA_M_DAP_MAP_CANTIDADES_HS17.dta: importaciones de EE.UU. de DAP/MAP, valor y kg.
#   - TP6_USA_M_SOCIOS_FERT_HS17.dta       : importaciones de EE.UU. de fertilizantes (valor).
#   - TP6_MAR_X_SOCIOS_FERT_HS17.dta       : exportaciones de Marruecos por destino (valor).
#   - TP6_EXPORTADORES_WLD_FERT_HS17.dta   : exportadores mundiales (participación).
#   - pinksheet_dap_anual.csv              : precio DAP spot f.o.b. US Gulf (Banco Mundial).
#   - usa_oferta_fosfatados_2022.csv       : producción, comercio y consumo de EE.UU. (USDA ERS ERR-354, Table 1).
#   - cvd_tasas_rusia.csv                  : tasas del CVD a Rusia (Federal Register).
#
# CONVENCIONES PROPIAS DEL SCRIPT (no vienen del TP; ver nota de datos)
#   - Producto: DAP y MAP (HS 310530 y 310540), como en el inciso (a). El alcance de
#     la orden es más amplio; se calcula también (HS6 "principales" del listado).
#   - Derecho pagado (c + e = t x M): valor de importación de EE.UU. desde Marruecos
#     (valor aduanero de WITS, que es la base del derecho) x tasa del depósito. Como
#     WITS es anual, se supone que las importaciones se reparten parejo en el año:
#     la tasa del año es el promedio ponderado por días y el rango usa la tasa
#     mínima y máxima en vigor durante el año.
#   - Medida provisional (30/11/2020 a 29/03/2021): depósito a la tasa preliminar
#     (85 FR 76522). Días sin tasa conocida (suspensión por la Proclamación 11038,
#     2026) quedan NA: no se completan.
#   - 2023 es el año base: la revisión de ese período fue rescindida y los derechos
#     se liquidan a la tasa del depósito vigente al ingreso (cvd06). Para otros años
#     la liquidación definitiva depende de las revisiones, por eso son solo contexto.
#   - Exportaciones de Marruecos: se usan hasta el año base; 2024 se descarta por un
#     chequeo contra el precio DAP (se imprime).
#   - El esquema de tres mercados es SIN ESCALA (curvas lineales ilustrativas) y supone
#     un único exportador; el CVD recae solo sobre OCP y Rusia, y el resto de los
#     proveedores entra con NMF 0%.
#   - b + d de EE.UU. se CALIBRA (no se mide) con elasticidades de la USITC y de OCP; el
#     área a solo tiene un orden de magnitud (USDA ERS); f no se estima (falta la
#     elasticidad de oferta de OCP). La parte de EE.UU. como comprador "grande" se
#     apoya en sus importaciones sobre las exportaciones mundiales.
#   - G10 (g_fert_subsidio_exportador.png): esquema SIN ESCALA del lado de Marruecos,
#     subsidio a la exportación (fila superior) y a la producción (fila inferior). Las
#     identidades de bienestar se verifican con stopifnot; no se estiman b, d ni e en
#     dólares (falta la elasticidad de oferta de OCP).
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
cat("Directorio de trabajo:", getwd(), "\n")

ruta_datos    <- "bases de datos/"
ruta_graficos <- "output/graficos/03/"
ruta_tablas   <- "output/tablas/03/"
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas,   recursive = TRUE, showWarnings = FALSE)

# Guardado de gráficos con control: verifica que el archivo exista, prueba un plan B y muestra la ruta completa
guardar_png <- function(grafico, nombre, ancho, alto) {
  ruta <- file.path(ruta_graficos, nombre)
  if (file.exists(ruta)) suppressWarnings(file.remove(ruta))
  intento <- function(...) tryCatch({
    suppressWarnings(ggsave(ruta, grafico, width = ancho, height = alto, dpi = 300, bg = color_fondo_tp2, ...))
    file.exists(ruta) && file.size(ruta) > 0
  }, error = function(e) { message("  (error al guardar: ", conditionMessage(e), ")"); FALSE })
  ok <- intento()
  if (!ok) ok <- intento(device = "png")      # plan B: dispositivo gráfico base de R
  cat(if (ok) "Gráfico guardado: " else "ATENCIÓN - NO SE PUDO GUARDAR: ", normalizePath(ruta, mustWork = FALSE), "\n", sep = "")
  invisible(ok)
}

#===============================================================================#
# BLOQUE 0: PARÁMETROS, ESTILO Y FUNCIONES
#===============================================================================#
options(width = 200, tibble.width = Inf)
anio_ref <- 2023
prods_dapmap <- c("310530", "310540")         # DAP y MAP (HS 2017)

font_add_google("Rajdhani", "rajdhani"); showtext_auto(); showtext_opts(dpi = 300)
color_fondo_tp2 <- "#FAFAF8"; color_texto_tp2 <- "#262421"; color_texto_sec <- "#6B6660"
color_grilla_tp2 <- "#E4E1DB"; color_titulo_tp2 <- "#7A1128"
color_rojo <- "#C1272D"; color_verde <- "#006233"; color_gris_dato <- "#B8B3AA"
color_ambar <- "#D9A441"; color_azul <- "#4A6FA5"

# Formato de números (igual en los scripts 01 a 04: coma decimal, punto de miles)
dec <- ","; mil <- "."
cortes_tamano <- c(grande = 10, intermedio = 3)   # mismos cortes que el script 01 (convención del equipo)
clasificar <- function(p) case_when(p >= cortes_tamano["grande"] ~ "grande", p >= cortes_tamano["intermedio"] ~ "intermedio", TRUE ~ "pequeño")
fmt <- function(x, acc = 1) number(x, accuracy = acc, big.mark = mil, decimal.mark = dec)
eje_num <- label_number(big.mark = mil, decimal.mark = dec)

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
      plot.title.position   = "plot",
      plot.caption.position = "plot",
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
verificar_archivo <- function(archivo) {
  ruta <- paste0(ruta_datos, archivo)
  if (!file.exists(ruta)) stop("Falta el archivo: ", ruta)
  ruta
}
leer_csv_texto <- function(archivo) read_csv(verificar_archivo(archivo), col_types = cols(.default = "c"), show_col_types = FALSE)
leer_wits <- function(archivo) {
  b <- read_dta(verificar_archivo(archivo)); names(b) <- tolower(names(b))
  b %>% transmute(
    prod = a_chr(productcode), desc = a_chr(productdescription),
    rep = a_chr(reportername), socio = a_chr(partnername),
    flujo = a_chr(tradeflowname), anio = as.integer(year), valor = as.numeric(tradevaluein1000usd))
}
mUSD <- function(v_miles) v_miles / 1e3     # miles de USD -> millones de USD

#===============================================================================#
# BLOQUE 1: INSTRUMENTO - TASA DEL DEPÓSITO DEL CVD EN VIGOR (inciso b)
#===============================================================================#
cron <- leer_csv_texto("cvd_cronologia_ocp.csv")
tasas <- leer_csv_texto("cvd_tasas_deposito_ocp.csv") %>%
  mutate(vigente_desde = as.Date(vigente_desde), tasa_pct = as.numeric(tasa_pct)) %>%
  arrange(vigente_desde)
stopifnot(!anyNA(tasas$vigente_desde), !is.unsorted(tasas$vigente_desde, strictly = TRUE))

# Control cruzado: cada tasa positiva coincide con la tasa de su evento en la cronología,
# y las tasas de revisiones administrativas rigen desde la fecha de publicación del aviso
chk <- tasas %>% filter(!is.na(tasa_pct), tasa_pct > 0) %>%
  mutate(id_evento = str_extract(id_fuente, "^[^;]+")) %>%
  left_join(cron %>% transmute(id_evento = id, fecha_pub = as.Date(fecha_publicacion),
                               tasa_cron = suppressWarnings(as.numeric(str_replace(tasa_ocp_pct_advalorem, ",", ".")))),
            by = "id_evento")
stopifnot(!anyNA(chk$tasa_cron), all(abs(chk$tasa_pct - chk$tasa_cron) < 1e-9))
chk_rev <- chk %>% filter(str_detect(tipo, "revision"))
stopifnot(nrow(chk_rev) > 0, all(chk_rev$vigente_desde == chk_rev$fecha_pub))

fecha_prelim <- tasas %>% filter(str_detect(tipo, "^provisional")) %>% pull(vigente_desde)
tasa_prelim  <- tasas %>% filter(str_detect(tipo, "^provisional")) %>% pull(tasa_pct)
fecha_fin_prov <- tasas$vigente_desde[which(str_detect(tasas$tipo, "^provisional")) + 1] - 1
fecha_orden  <- cron %>% filter(id == "cvd02") %>% pull(fecha_publicacion) %>% as.Date()
stopifnot(length(fecha_prelim) == 1, length(fecha_orden) == 1, length(tasa_prelim) == 1, !is.na(tasa_prelim), length(fecha_fin_prov) == 1)
anio_orden <- as.integer(format(fecha_orden, "%Y"))
anio_pre   <- as.integer(format(fecha_prelim, "%Y")) - 1       # último año completo sin medida (POI = ese año)
fecha_fin  <- max(as.Date(cron$fecha_publicacion))               # última fecha con fuente

tasa_en <- function(fecha) {
  i <- findInterval(as.numeric(fecha), as.numeric(tasas$vigente_desde))
  ifelse(i == 0, NA_real_, tasas$tasa_pct[pmax(i, 1)])
}

cat("\nTasas del depósito del CVD sobre OCP (vigencia y estado de verificación):\n")
print(tasas %>% select(id, vigente_desde, tasa_pct, tipo, id_fuente, estado_verificacion), width = 250)
write_csv(tasas, paste0(ruta_tablas, "tasas_deposito_cvd_ocp.csv"))

# Arancel NMF de EE.UU. y consolidado
arancel_usa <- leer_csv_texto("arancel_usa_dap_map.csv") %>%
  mutate(tasa_general_pct = as.numeric(tasa_general_pct))
stopifnot(nrow(arancel_usa) == 2, setequal(arancel_usa$hs6, prods_dapmap), !anyNA(arancel_usa$tasa_general_pct))
cat("\nArancel NMF de EE.UU. a DAP/MAP (HTS): ", paste(arancel_usa$tasa_general_pct, collapse = "% y "),
    "%. Consolidado en la OMC: no disponible (no relevado).\n", sep = "")
write_csv(arancel_usa, paste0(ruta_tablas, "arancel_usa_dap_map.csv"))

# Objetivos declarados (con su estado de verificación)
objetivos <- leer_csv_texto("citas_objetivos.csv") %>% filter(str_detect(tema, "CVD")) %>%
  select(id, tema, afirmacion, cita_textual, emisor, fecha, url, pagina_o_seccion, estado_verificacion)
stopifnot(nrow(objetivos) > 0)
cat("\nObjetivos declarados del CVD y su suspensión:\n"); print(objetivos %>% select(id, afirmacion, emisor, fecha, estado_verificacion), width = 250)
write_csv(objetivos, paste0(ruta_tablas, "objetivos_cvd_citas.csv"))

# Tasa promedio por año (ponderada por días) y rango
usa_q <- read_dta(verificar_archivo("TP6_USA_M_DAP_MAP_CANTIDADES_HS17.dta")); names(usa_q) <- tolower(names(usa_q))
usa_q <- usa_q %>% transmute(
  prod = a_chr(productcode), socio = a_chr(partnername), anio = as.integer(year),
  valor = as.numeric(tradevaluein1000usd), kg = as.numeric(quantity), unidad = a_chr(qtyunit)) %>%
  filter(socio != "World")
stopifnot(all(usa_q$unidad == "Kg"), !anyNA(usa_q$kg), all(usa_q$prod %in% prods_dapmap))
usa_q <- usa_q %>% mutate(ton = kg / 1000)
anios_us <- sort(unique(usa_q$anio))
stopifnot(anio_ref %in% anios_us, anio_pre %in% anios_us)

tasa_anual <- map_dfr(anios_us, function(y) {
  d <- seq(as.Date(paste0(y, "-01-01")), as.Date(paste0(y, "-12-31")), by = "day")
  r <- tasa_en(d)
  tibble(anio = y, n_dias = length(d), n_dias_sin_tasa = sum(is.na(r)),
         tasa_prom = if (anyNA(r)) NA_real_ else mean(r),
         tasa_min  = if (all(is.na(r))) NA_real_ else min(r, na.rm = TRUE),
         tasa_max  = if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE))
})
cat("\nTasa del depósito por año (promedio ponderado por días; NA = hay días sin tasa conocida):\n")
print(tasa_anual)
write_csv(tasa_anual, paste0(ruta_tablas, "tasa_deposito_anual_cvd_ocp.csv"))

#===============================================================================#
# BLOQUE 2: IMPORTACIONES DE EE.UU. DE DAP/MAP (valor y toneladas)
#===============================================================================#
tot_anio <- usa_q %>% group_by(anio) %>%
  summarise(valor_tot = sum(valor), ton_tot = sum(ton), n_origenes = n_distinct(socio), .groups = "drop")
mar_anio <- usa_q %>% filter(socio == "Morocco") %>% group_by(anio) %>%
  summarise(valor_mar = sum(valor), ton_mar = sum(ton), .groups = "drop")
usa_mar <- tot_anio %>% left_join(mar_anio, by = "anio") %>%
  mutate(across(c(valor_mar, ton_mar), ~ replace_na(.x, 0)),
         part_valor = 100 * valor_mar / valor_tot, part_ton = 100 * ton_mar / ton_tot,
         vu_mar = if_else(ton_mar > 0, valor_mar * 1e3 / ton_mar, NA_real_),   # USD por tonelada
         vu_tot = valor_tot * 1e3 / ton_tot) %>%
  left_join(tasa_anual, by = "anio")
cat("\nImportaciones de EE.UU. de DAP y MAP: total y desde Marruecos (valor en miles de USD, toneladas métricas):\n")
print(usa_mar %>% select(anio, valor_tot, valor_mar, part_valor, ton_mar, part_ton, vu_mar, vu_tot), width = 200)
write_csv(usa_mar, paste0(ruta_tablas, "usa_importaciones_dap_map_marruecos.csv"))

# Desvío hacia otros orígenes
top_otros <- usa_q %>% filter(socio != "Morocco") %>% group_by(socio) %>% summarise(v = sum(valor), .groups = "drop") %>%
  slice_max(v, n = 3) %>% pull(socio)
es <- c("Russian Federation" = "Rusia", "Saudi Arabia" = "Arabia Saudita", "Mexico" = "México", "Jordan" = "Jordania",
        "Australia" = "Australia", "Tunisia" = "Túnez", "Egypt, Arab Rep." = "Egipto")
nombre_es <- function(x) ifelse(x %in% names(es), unname(es[x]), x)
niveles_origen <- c("Marruecos", nombre_es(top_otros), "Otros")
origen <- usa_q %>%
  mutate(grupo = case_when(socio == "Morocco" ~ "Marruecos", socio %in% top_otros ~ nombre_es(socio), TRUE ~ "Otros")) %>%
  group_by(anio, grupo) %>% summarise(valor = sum(valor), ton = sum(ton), .groups = "drop") %>%
  group_by(anio) %>% mutate(peso_valor = 100 * valor / sum(valor), peso_ton = 100 * ton / sum(ton)) %>% ungroup() %>%
  mutate(grupo = factor(grupo, levels = niveles_origen))
print(origen %>% filter(anio %in% c(anio_pre, anio_ref)) %>% arrange(anio, grupo), n = Inf)
write_csv(origen, paste0(ruta_tablas, "usa_importaciones_dap_map_por_origen.csv"))

# Precio unitario de MAP (310540): Marruecos vs. otros orígenes, y precio DAP del Pink Sheet
pink <- leer_csv_texto("pinksheet_dap_anual.csv") %>% transmute(anio = as.integer(anio), dap_pinksheet = as.numeric(dap_usd_por_t_nominal))
stopifnot(!anyNA(pink$dap_pinksheet), anio_ref %in% pink$anio)
precio_map <- usa_q %>% filter(prod == "310540") %>% mutate(grupo = if_else(socio == "Morocco", "mar", "otros")) %>%
  group_by(anio, grupo) %>% summarise(v = sum(valor), t = sum(ton), .groups = "drop") %>%
  mutate(vu = v * 1e3 / t) %>% select(anio, grupo, vu, t) %>%
  pivot_wider(names_from = grupo, values_from = c(vu, t)) %>%
  mutate(ratio_mar_otros = vu_mar / vu_otros) %>%
  left_join(pink, by = "anio")
cat("\nMAP (310540): precio unitario de importación de EE.UU. (USD/t) - Marruecos vs. otros orígenes:\n")
print(precio_map, width = 200)
write_csv(precio_map, paste0(ruta_tablas, "precio_unitario_map_marruecos_vs_otros.csv"))
ratio_pre <- precio_map$ratio_mar_otros[precio_map$anio == anio_pre]
ratio_ref <- precio_map$ratio_mar_otros[precio_map$anio == anio_ref]

#===============================================================================#
# BLOQUE 3: DERECHO PAGADO t x M (áreas c + e) PARA EL ALCANCE DE LA ORDEN Y PARA DAP/MAP
#===============================================================================#
alcance <- leer_csv_texto("cvd_alcance_orden.csv")
hs6_alcance <- alcance %>% filter(tipo == "principal") %>% distinct(hs6) %>% pull(hs6)

usa_fert <- leer_wits("TP6_USA_M_SOCIOS_FERT_HS17.dta") %>% filter(flujo == "Import")
faltan <- setdiff(hs6_alcance, unique(usa_fert$prod))
if (length(faltan) > 0) stop("Líneas del alcance de la orden sin datos en la base de EE.UU.: ", paste(faltan, collapse = ", "))

# Control: DAP y MAP coinciden entre las dos bases de EE.UU.
comp <- usa_fert %>% filter(socio == "Morocco", prod %in% prods_dapmap) %>% group_by(anio) %>% summarise(v = sum(valor), .groups = "drop") %>%
  inner_join(usa_mar %>% select(anio, valor_mar), by = "anio")
stopifnot(nrow(comp) > 0, all(abs(comp$v - comp$valor_mar) < 1e-6))

base_dm <- usa_mar %>% transmute(anio, alcance = "DAP y MAP", base_M = mUSD(valor_mar))
base_al <- usa_fert %>% filter(socio == "Morocco", prod %in% hs6_alcance, anio %in% anios_us) %>%
  group_by(anio) %>% summarise(base_M = mUSD(sum(valor)), .groups = "drop") %>% mutate(alcance = "Alcance de la orden (HS6 principales)")
anio_rescindido <- as.integer(substr(cron$periodo_de_revision[cron$id == "cvd06"], 1, 4))
stopifnot(length(anio_rescindido) == 1, !is.na(anio_rescindido))

recaud <- bind_rows(base_dm, base_al) %>% left_join(tasa_anual, by = "anio") %>%
  mutate(derecho_central_M = if_else(is.na(tasa_prom), NA_real_, base_M * tasa_prom / 100),
         derecho_min_M     = if_else(is.na(tasa_prom), NA_real_, base_M * tasa_min / 100),
         derecho_max_M     = if_else(is.na(tasa_prom), NA_real_, base_M * tasa_max / 100),
         liquidacion = case_when(is.na(tasa_prom) ~ "sin tasa conocida para todo el año",
                                 anio == anio_rescindido ~ "definitiva = tasa del depósito (revisión rescindida, cvd06)",
                                 TRUE ~ "depósito; la liquidación definitiva depende de las revisiones")) %>%
  arrange(alcance, anio)
cat("\nDerecho pagado (c + e = t x M), millones de USD:\n")
print(recaud %>% select(alcance, anio, base_M, tasa_prom, tasa_min, tasa_max, derecho_central_M, derecho_min_M, derecho_max_M, liquidacion), n = Inf, width = 250)
write_csv(recaud, paste0(ruta_tablas, "derecho_pagado_tM_cvd_ocp.csv"))

rec_ref <- recaud %>% filter(anio == anio_ref)
stopifnot(nrow(rec_ref) == 2, !anyNA(rec_ref$derecho_central_M))
rec_dm <- rec_ref %>% filter(alcance == "DAP y MAP"); rec_al <- rec_ref %>% filter(str_detect(alcance, "Alcance"))
cat("\n", anio_ref, ": derecho pagado en DAP y MAP = ", fmt(rec_dm$derecho_central_M, 0.1), " M USD (rango ", fmt(rec_dm$derecho_min_M, 0.1), "-",
    fmt(rec_dm$derecho_max_M, 0.1), "), base ", fmt(rec_dm$base_M, 0.1), " M USD x ", fmt(rec_dm$tasa_prom, 0.01), "%\n", sep = "")
cat("      alcance de la orden (HS6 principales): ", fmt(rec_al$derecho_central_M, 0.1), " M USD (rango ", fmt(rec_al$derecho_min_M, 0.1), "-",
    fmt(rec_al$derecho_max_M, 0.1), "), base ", fmt(rec_al$base_M, 0.1), " M USD\n", sep = "")

#===============================================================================#
# BLOQUE 4: EL EXPORTADOR - DESTINOS DE LAS EXPORTACIONES DE MARRUECOS Y TAMAÑO
#===============================================================================#
mar_x <- leer_wits("TP6_MAR_X_SOCIOS_FERT_HS17.dta") %>% filter(flujo == "Export", prod %in% prods_dapmap)

# Chequeo del último año de la base (2024): valor exportado vs. precio DAP
chequeo_2024 <- mar_x %>% filter(anio %in% c(anio_ref, anio_ref + 1)) %>% group_by(anio) %>% summarise(valor = sum(valor), .groups = "drop") %>%
  left_join(pink, by = "anio") %>% arrange(anio)
if (nrow(chequeo_2024) == 2) {
  cat("\nChequeo de ", anio_ref + 1, ": el valor exportado de DAP/MAP de Marruecos varía ",
      fmt(100 * (chequeo_2024$valor[2] / chequeo_2024$valor[1] - 1), 1), "% frente a ", anio_ref,
      " y el precio DAP del Pink Sheet varía ", fmt(100 * (chequeo_2024$dap_pinksheet[2] / chequeo_2024$dap_pinksheet[1] - 1), 1),
      "%. Se usan los datos de Marruecos hasta ", anio_ref, ".\n", sep = "")
}

destino <- mar_x %>% filter(anio <= anio_ref) %>% group_by(anio) %>%
  summarise(total = sum(valor), eeuu = sum(valor[socio == "United States"]), .groups = "drop") %>%
  mutate(resto = total - eeuu, part_eeuu = 100 * eeuu / total) %>%
  left_join(pink, by = "anio") %>%
  mutate(total_M = mUSD(total), eeuu_M = mUSD(eeuu), resto_M = mUSD(resto),
         ton_equiv_dap_Mt = total * 1e3 / dap_pinksheet / 1e6)         # valor / precio DAP: solo una referencia de volumen
cat("\nExportaciones de DAP y MAP de Marruecos (reporta Marruecos; valor en miles de USD):\n"); print(destino, width = 200)
write_csv(destino, paste0(ruta_tablas, "marruecos_exportaciones_dap_map_destino.csv"))

# Tamaño de Marruecos como exportador mundial (misma convención que el script 02)
tam <- leer_wits("TP6_EXPORTADORES_WLD_FERT_HS17.dta") %>%
  filter(flujo == "Export", rep != "European Union", anio == anio_ref, prod %in% prods_dapmap) %>%
  group_by(prod) %>% mutate(mundo = sum(valor), participacion = 100 * valor / mundo, rank_mundial = min_rank(desc(valor))) %>% ungroup() %>%
  filter(rep == "Morocco") %>% select(prod, valor_mar = valor, mundo, participacion, rank_mundial)
stopifnot(nrow(tam) == 2)
cat("\nMarruecos como exportador mundial, ", anio_ref, ":\n", sep = ""); print(tam)
write_csv(tam, paste0(ruta_tablas, "marruecos_tamano_dap_map_", anio_ref, ".csv"))
part_dap <- tam$participacion[tam$prod == "310530"]; part_map <- tam$participacion[tam$prod == "310540"]

# Instrumento de MARRUECOS (inciso b): subsidios a OCP medidos por Commerce (determinación final, POI 2019)
sub <- leer_csv_texto("subsidios_ocp_programas.csv") %>% mutate(tasa_pct = as.numeric(tasa_pct))
stopifnot(nrow(sub) > 0, !anyNA(sub$tasa_pct), all(str_detect(sub$estado_verificacion, "^verificada")))
tasa_final_cron <- as.numeric(cron$tasa_ocp_pct_advalorem[cron$id == "cvd01"])
stopifnot(abs(sum(sub$tasa_pct) - tasa_final_cron) < 0.011)      # los programas suman la tasa final (19,97%)
sub_tipo <- sub %>% mutate(grupo = case_when(tipo == "exportacion" ~ "Subsidio a la exportación",
                                             str_detect(tipo, "insumo") ~ "Subsidio a la producción: insumo minero",
                                             TRUE ~ "Otros subsidios a la producción")) %>%
  group_by(grupo) %>% summarise(tasa_pct = sum(tasa_pct), .groups = "drop") %>% mutate(peso = 100 * tasa_pct / sum(tasa_pct))
cat("\nSubsidios a OCP (Commerce, determinación final, POI 2019), % ad valorem:\n"); print(sub %>% select(programa_es, tipo, tasa_pct)); print(sub_tipo)
write_csv(sub, paste0(ruta_tablas, "subsidios_ocp_programas.csv"))
# Orden de magnitud del subsidio implícito en las exportaciones de DAP/MAP: tasa medida para un año x exportaciones de ese año
# (la tasa se calcula sobre las ventas de OCP; aplicarla a las exportaciones de DAP/MAP es una aproximación)
anio_poi <- as.integer(str_extract(cron$periodo_de_revision[cron$id == "cvd01"], "\\d{4}"))
rev2 <- cron %>% filter(id == "cvd08")
anio_por2 <- as.integer(substr(rev2$periodo_de_revision, 1, 4)); tasa_por2 <- as.numeric(rev2$tasa_ocp_pct_advalorem)
stopifnot(!is.na(anio_poi), !is.na(anio_por2), !is.na(tasa_por2))
sub_monto <- tibble(periodo = c(paste0("Investigación (", anio_poi, ")"), paste0("2.ª revisión (", anio_por2, ")")),
                    anio = c(anio_poi, anio_por2), tasa_pct = c(tasa_final_cron, tasa_por2)) %>%
  left_join(destino %>% select(anio, total_M), by = "anio") %>% mutate(subsidio_M = total_M * tasa_pct / 100)
stopifnot(!anyNA(sub_monto$subsidio_M))
cat("Orden de magnitud del subsidio implícito en las exportaciones de DAP y MAP (M USD):\n"); print(sub_monto)
write_csv(sub_monto, paste0(ruta_tablas, "subsidio_implicito_dap_map.csv"))

# ¿Es EE.UU. un comprador grande? Importaciones de EE.UU. / exportaciones mundiales del producto
# Denominador = suma de exportaciones de los reportadores sin "European Union" (misma convención que tam).
# Numerador = importaciones de EE.UU. (reporta EE.UU., valor CIF). Mezcla flujos y valuaciones: es una aproximación.
# Se usa hasta el año base (en 2024 Marruecos, que está en el denominador, viene inflado).
expo_mundo <- leer_wits("TP6_EXPORTADORES_WLD_FERT_HS17.dta") %>%
  filter(flujo == "Export", rep != "European Union", prod %in% prods_dapmap, anio <= anio_ref)
stopifnot("United States" %in% expo_mundo$rep)
comprador <- expo_mundo %>% group_by(prod, anio) %>%
  summarise(mundo_x = sum(valor), x_usa = sum(valor[rep == "United States"]), .groups = "drop") %>%
  inner_join(usa_q %>% filter(anio <= anio_ref) %>% group_by(prod, anio) %>% summarise(m_usa = sum(valor), .groups = "drop"),
             by = c("prod", "anio"))
stopifnot(anio_ref %in% comprador$anio, nrow(comprador) > 0)
comprador <- bind_rows(comprador,
                       comprador %>% group_by(anio) %>% summarise(across(c(mundo_x, x_usa, m_usa), sum), .groups = "drop") %>% mutate(prod = "DAP y MAP")) %>%
  mutate(part_m_usa = 100 * m_usa / mundo_x, saldo_usa_M = mUSD(x_usa - m_usa), tamano = clasificar(part_m_usa)) %>%
  arrange(prod, anio)
cat("\nEE.UU. como comprador: importaciones de EE.UU. / exportaciones mundiales (miles de USD; saldo = exportaciones - importaciones de EE.UU.):\n")
print(comprador, width = 200)
write_csv(comprador, paste0(ruta_tablas, "usa_comprador_dap_map.csv"))
part_usa_ref <- comprador$part_m_usa[comprador$prod == "DAP y MAP" & comprador$anio == anio_ref]
tam_usa_ref  <- comprador$tamano[comprador$prod == "DAP y MAP" & comprador$anio == anio_ref]
part_eeuu_pre <- destino$part_eeuu[destino$anio == anio_pre]
comp_dm <- comprador %>% filter(prod == "DAP y MAP")
part_usa_min <- min(comp_dm$part_m_usa); part_usa_max <- max(comp_dm$part_m_usa)
saldo_usa_ref <- comp_dm$saldo_usa_M[comp_dm$anio == anio_ref]
stopifnot(length(part_usa_ref) == 1, length(part_eeuu_pre) == 1)

#===============================================================================#
# BLOQUE 4C: EL MERCADO DE EE.UU. Y SUS PRODUCTORES (base para el área a y el inciso d)
#===============================================================================#
# Fuente: USDA ERS, ERR-354, Table 1 (datos de IFA, 2022), en miles de toneladas de nutriente. El informe no aclara
# si en DAP/MAP cuenta N + P2O5 o solo P2O5, así que solo se usan COCIENTES dentro de la misma unidad.
oferta <- leer_csv_texto("usa_oferta_fosfatados_2022.csv") %>%
  mutate(across(c(anio, produccion, importaciones, exportaciones, consumo_aparente), as.numeric))
anio_oferta <- unique(oferta$anio); stopifnot(length(anio_oferta) == 1, anio_oferta %in% usa_mar$anio, setequal(oferta$hs6, prods_dapmap))
oferta_t <- bind_rows(oferta %>% select(producto, produccion, importaciones, exportaciones, consumo_aparente),
                      oferta %>% summarise(producto = "DAP y MAP", across(c(produccion, importaciones, exportaciones, consumo_aparente), sum))) %>%
  mutate(autoabastecimiento = 100 * produccion / consumo_aparente,      # producción / consumo aparente
         part_importaciones = 100 * importaciones / consumo_aparente)   # importaciones / consumo aparente
us_of <- usa_mar %>% filter(anio == anio_oferta)
tot_of <- oferta_t %>% filter(producto == "DAP y MAP")
# Participación de Marruecos en el consumo de EE.UU. (aprox.: cuota en el VALOR importado x importaciones / consumo en nutriente)
part_mar_consumo <- tot_of$part_importaciones * us_of$part_valor / 100
# Base del área a frente a la base del derecho: producción de EE.UU. / importaciones desde Marruecos (misma aproximación)
ratio_S_Mmar <- tot_of$produccion / (tot_of$importaciones * us_of$part_valor / 100)
S_valor_aprox <- ratio_S_Mmar * mUSD(us_of$valor_mar)                  # M USD, supone igual precio por unidad de nutriente
a_por_pp <- 0.01 * S_valor_aprox                                        # área a si el precio interno subiera 1%
tM_oferta <- us_of$tasa_prom / 100 * mUSD(us_of$valor_mar)              # c + e del mismo año
mercado_usa <- tibble(anio = anio_oferta, autoabastecimiento = tot_of$autoabastecimiento, part_importaciones = tot_of$part_importaciones,
                      part_marruecos_en_importaciones_valor = us_of$part_valor, part_marruecos_en_consumo_aprox = part_mar_consumo,
                      produccion_sobre_import_marruecos = ratio_S_Mmar, valor_produccion_aprox_M = S_valor_aprox,
                      area_a_por_punto_de_precio_M = a_por_pp, derecho_pagado_tM_M = tM_oferta)
cat("\nMercado de EE.UU. de DAP y MAP (USDA ERS, ", anio_oferta, "; cocientes en nutriente):\n", sep = ""); print(oferta_t, width = 200)
cat("Orden de magnitud del área a (", anio_oferta, "):\n", sep = ""); print(mercado_usa, width = 250)
write_csv(oferta_t, paste0(ruta_tablas, "usa_oferta_dap_map_", anio_oferta, ".csv"))
write_csv(mercado_usa, paste0(ruta_tablas, "usa_area_a_orden_magnitud_", anio_oferta, ".csv"))

# CVD a Rusia (para leer el desvío: el comercio fue hacia orígenes sin derecho)
rusia <- leer_csv_texto("cvd_tasas_rusia.csv") %>% mutate(tasa_pct = as.numeric(tasa_pct), vigente_desde = as.Date(vigente_desde))
rus_orden <- rusia %>% filter(vigente_desde == min(vigente_desde))
stopifnot(nrow(rus_orden) >= 2, !anyNA(rus_orden$tasa_pct))
txt_rusia <- paste0("Rusia también tiene un derecho compensatorio desde la orden de ", format(min(rusia$vigente_desde), "%m/%Y"), ": ",
                    fmt(min(rus_orden$tasa_pct), 0.01), "% a ", fmt(max(rus_orden$tasa_pct), 0.01), "% según la empresa (",
                    fmt(rus_orden$tasa_pct[rus_orden$empresa == "All Others"], 0.01), "% el resto; cvd_tasas_rusia.csv).")
cat("\n", txt_rusia, "\n", sep = "")

#===============================================================================#
# BLOQUE 4B: CALIBRACIÓN DEL BIENESTAR DE EE.UU. EN EL MERCADO DEL DAP/MAP MARROQUÍ (inciso c)
#===============================================================================#
# Qué se calibra y con qué:
#   (1) Incidencia (área e): si OCP hubiera absorbido parte del derecho, su precio de aduana (sin derecho)
#       habría caído frente al de otros orígenes. Se mide con el precio unitario relativo de MAP.
#       theta = parte del derecho absorbida por el exportador = -ln(ratio_ref / ratio_pre) / ln(1 + t).
#       theta <= 0 -> no hay evidencia de absorción: e ~ 0 y c + e ~ c (lo paga el comprador de EE.UU.).
#   (2) Pérdida de eficiencia (b + d) con demanda de elasticidad constante para la variedad marroquí
#       (Armington): Q = A p^(-eps). Con precio sin derecho = 1 (e = 0) y con derecho = 1 + t, y
#       M1 = importaciones observadas con el derecho (valor sin derecho):
#         pérdida de EC = A [ (1+t)^(1-eps) - 1 ] / (1 - eps),  A = M1 (1+t)^eps
#         recaudación  = t M1
#         pérdida de eficiencia = pérdida de EC - recaudación
#       eps = elasticidad de sustitución de la USITC (Pub. 5172, p. II-29: 4 a 6) y, como cota baja, la de
#       OCP (2,09, nota 45). Supuesto: Marruecos tiene una porción chica del consumo de EE.UU., por lo que la
#       elasticidad de la demanda por su variedad se aproxima por la de sustitución (se declara).
#   (3) Chequeo de consistencia: importaciones contrafácticas sin derecho M0 = M1 (1+t)^eps, contra lo
#       observado antes de la medida.
# NO se calibra: el reparto entre productores de EE.UU. (área a) y consumidores, ni el excedente de
# OCP (área f): faltan producción y consumo de EE.UU. y la elasticidad de oferta de OCP.
elast <- leer_csv_texto("parametros_elasticidades.csv")
e_sust <- elast %>% filter(id == "el03"); e_ocp <- elast %>% filter(id == "el04")
stopifnot(nrow(e_sust) == 1, nrow(e_ocp) == 1, str_detect(e_sust$estado_verificacion, "^verificada"))
eps_val <- c(cota_baja_OCP = as.numeric(e_ocp$valor_min), USITC_min = as.numeric(e_sust$valor_min), USITC_max = as.numeric(e_sust$valor_max))
stopifnot(!anyNA(eps_val))

t_cal  <- rec_dm$tasa_prom / 100
M1_cal <- rec_dm$base_M                                   # millones de USD, valor de aduana (sin derecho)
theta  <- -log(ratio_ref / ratio_pre) / log(1 + t_cal)
cat("\nIncidencia: precio unitario MAP Marruecos/otros ", fmt(ratio_pre, 0.001), " (", anio_pre, ") -> ", fmt(ratio_ref, 0.001), " (", anio_ref,
    "). Parte del derecho absorbida por el exportador implícita: ", fmt(100 * theta, 1), "% ",
    if (theta <= 0) "(sin evidencia de absorción: e ~ 0)" else "", "\n", sep = "")

calib <- tibble(escenario = names(eps_val), eps = unname(eps_val)) %>%
  mutate(A = M1_cal * (1 + t_cal)^eps,
         perdida_EC = A * ((1 + t_cal)^(1 - eps) - 1) / (1 - eps),
         recaudacion_c = t_cal * M1_cal,
         perdida_eficiencia = perdida_EC - recaudacion_c,
         bienestar_neto_EEUU = recaudacion_c - perdida_EC,
         M0_contrafactico = M1_cal * (1 + t_cal)^eps) %>%
  select(-A)
stopifnot(all(calib$perdida_eficiencia > 0))
cat("Calibración (millones de USD, ", anio_ref, "; t = ", fmt(100 * t_cal, 0.01), "%, M1 = ", fmt(M1_cal, 0.1), "):\n", sep = ""); print(calib, width = 200)
write_csv(calib %>% mutate(t = t_cal, M1 = M1_cal, theta_absorcion = theta, anio = anio_ref), paste0(ruta_tablas, "calibracion_bienestar_cvd_", anio_ref, ".csv"))
M_pre_obs <- mUSD(usa_mar$valor_mar[usa_mar$anio == anio_pre])
cat("Chequeo: importaciones sin derecho implícitas ", fmt(min(calib$M0_contrafactico)), "-", fmt(max(calib$M0_contrafactico)),
    " M USD; observadas en ", anio_pre, ": ", fmt(M_pre_obs), " M USD. Si las observadas son mayores, la caída no se explica solo por el derecho",
    " (o la elasticidad efectiva es mayor).\n", sep = "")
cal_c <- calib %>% filter(escenario %in% c("USITC_min", "USITC_max"))
# Contraste independiente: elasticidad de demanda de importaciones de EE.UU. por HS6 (wiiw, datos 1996-2014)
el_us <- read_dta(verificar_archivo("Elasticities_Data.dta")) %>%
  transmute(iso3 = as.character(iso3), prod = as.character(product), eps = -as.numeric(elasticity), se = as.numeric(se), p = as.numeric(p)) %>%
  filter(iso3 == "USA", prod %in% prods_dapmap)
stopifnot(nrow(el_us) == 2)
cat("Contraste wiiw (EE.UU., demanda total de importaciones): ", paste0(el_us$prod, " = ", fmt(el_us$eps, 0.01), " (p = ", fmt(el_us$p, 0.001), ")", collapse = "; "), "\n", sep = "")
eps_us_map <- el_us$eps[el_us$prod == "310540"]; p_us_map <- el_us$p[el_us$prod == "310540"]
write_csv(el_us, paste0(ruta_tablas, "elasticidades_wiiw_usa_dap_map.csv"))

#===============================================================================#
# BLOQUE 5: GRÁFICOS
#===============================================================================#
pie_cvd <- "Fuente: Federal Register (cvd_cronologia_ocp.csv, cvd_tasas_deposito_ocp.csv); estados de verificación en esos archivos."
pie_usa <- "Fuente: WITS, EE.UU. como reportero (TP6_USA_M_DAP_MAP_CANTIDADES_HS17), HS 2017 310530 (DAP) y 310540 (MAP)."
lab_fecha <- function(d) format(d, "%d/%m/%Y")

# G1: tasa del depósito en vigor (línea de tiempo)
x_ini <- as.Date(paste0(anio_pre + 1, "-01-01"))
tl <- tasas %>% mutate(hasta = lead(vigente_desde, default = fecha_fin), desde = pmax(vigente_desde, x_ini)) %>%
  filter(hasta > x_ini) %>% mutate(dur = as.numeric(hasta - desde), medio = desde + dur / 2)
tl_pos <- tl %>% filter(!is.na(tasa_pct))
tl_con <- tl_pos %>% mutate(tasa_ant = lag(tasa_pct), x = desde) %>% filter(!is.na(tasa_ant), tasa_ant != tasa_pct)
prov <- tl %>% filter(str_detect(tipo, "^provisional")); susp <- tl %>% filter(str_detect(tipo, "^suspension"))
g1 <- ggplot() +
  geom_rect(data = prov, aes(xmin = desde, xmax = hasta, ymin = 0, ymax = Inf), fill = color_gris_dato, alpha = 0.35) +
  geom_rect(data = susp, aes(xmin = desde, xmax = hasta, ymin = 0, ymax = Inf), fill = color_ambar, alpha = 0.45) +
  geom_segment(data = tl_con, aes(x = x, xend = x, y = tasa_ant, yend = tasa_pct), color = color_texto_sec, linetype = "dotted") +
  geom_segment(data = tl_pos, aes(x = desde, xend = hasta, y = tasa_pct, yend = tasa_pct, color = tasa_pct > 0), linewidth = 3.2, lineend = "butt") +
  geom_text(data = tl_pos %>% filter(tasa_pct > 0, dur >= 150), aes(x = medio, y = tasa_pct, label = paste0(fmt(tasa_pct, 0.01), "%")),
            vjust = -1, family = "rajdhani", fontface = "bold", size = 5, color = color_texto_tp2) +
  geom_text(data = tl_pos %>% filter(tasa_pct > 0, dur < 150), aes(x = desde, y = tasa_pct, label = paste0(fmt(tasa_pct, 0.01), "%")),
            hjust = 1.1, vjust = -0.6, family = "rajdhani", fontface = "bold", size = 5, color = color_texto_tp2) +
  geom_text(data = prov, aes(x = desde, y = max(tl_pos$tasa_pct) * 1.28, label = "Medida provisional\n(tasa preliminar)"),
            hjust = 0, vjust = 1, family = "rajdhani", size = 3.9, color = color_texto_sec, lineheight = 0.95) +
  geom_text(data = susp, aes(x = desde, y = max(tl_pos$tasa_pct) * 1.28, label = "Proclamación 11038:\nimportación libre de\nderechos a pedido"),
            hjust = 1.04, vjust = 1, family = "rajdhani", size = 3.9, color = color_texto_sec, lineheight = 0.95) +
  scale_color_manual(values = c("TRUE" = color_rojo, "FALSE" = color_gris_dato), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(x_ini, fecha_fin), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.02, 0.05))) +
  coord_cartesian(ylim = c(0, max(tl_pos$tasa_pct) * 1.28), clip = "off") +
  labs(title = env_wrap("Derecho compensatorio de EE.UU. sobre OCP: tasa del depósito en vigor", 55),
       subtitle = env_wrap(paste0("Ad valorem sobre las importaciones de fosfatados desde Marruecos (OCP). Arancel NMF de EE.UU. a DAP y MAP: ",
                                  fmt(max(arancel_usa$tasa_general_pct)), "%. Tasas: ",
                                  paste(paste0(fmt(tasas$tasa_pct[!is.na(tasas$tasa_pct) & tasas$tasa_pct > 0], 0.01), "%"), collapse = "; "),
                                  ", en ese orden."), 90),
       x = NULL, y = "Tasa del depósito (% ad valorem)",
       caption = env_wrap(paste0(pie_cvd, " Sombreado gris: medida provisional (depósito a la tasa preliminar). Sombreado ámbar: importación libre de derechos a pedido, envío por envío. El gráfico termina en la última fecha con fuente (",
                                 lab_fecha(fecha_fin), ")."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g1
guardar_png(g1, "g_fert_cvd_tasa_deposito.png", 11, 6)

# G2: importaciones de EE.UU. desde Marruecos (valor) y tasa del año
pan_a <- "A. Importaciones de EE.UU. desde Marruecos, DAP y MAP (millones de USD)"
pan_b <- "B. Tasa del depósito del CVD (promedio anual ponderado por días, %)"
g2a <- usa_mar %>% transmute(anio, y = mUSD(valor_mar),
                             etq = paste0(fmt(mUSD(valor_mar)), "\n(", fmt(part_valor), "% del total)"), panel = pan_a)
g2b <- usa_mar %>% transmute(anio, y = tasa_prom, ymin = if_else(is.na(tasa_prom), NA_real_, tasa_min), ymax = if_else(is.na(tasa_prom), NA_real_, tasa_max),
                             etq = case_when(is.na(tasa_prom) ~ "sin tasa\nconocida", tasa_prom == 0 ~ "sin CVD", TRUE ~ paste0(fmt(tasa_prom, 0.1), "%")), panel = pan_b)
g2 <- ggplot() +
  geom_col(data = g2a, aes(factor(anio), y), fill = color_gris_dato, width = 0.62) +
  geom_col(data = filter(g2b, !is.na(y)), aes(factor(anio), y), fill = color_rojo, width = 0.62) +
  geom_linerange(data = filter(g2b, !is.na(ymin)), aes(factor(anio), ymin = ymin, ymax = ymax), linewidth = 1.5, color = color_texto_tp2) +
  geom_text(data = g2a, aes(factor(anio), y, label = etq), vjust = -0.3, family = "rajdhani", fontface = "bold", size = 4, color = color_texto_tp2, lineheight = 0.95) +
  geom_text(data = g2b, aes(factor(anio), if_else(is.na(ymax), 0, ymax), label = etq), vjust = -0.3, family = "rajdhani", fontface = "bold",
            size = 4, color = color_texto_tp2, lineheight = 0.95) +
  facet_wrap(~ factor(panel, levels = c(pan_a, pan_b)), ncol = 1, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.28)), labels = eje_num) +
  labs(title = env_wrap(paste0("Marruecos pasó de ", fmt(usa_mar$part_valor[usa_mar$anio == anio_pre]), "% a ", fmt(usa_mar$part_valor[usa_mar$anio == anio_ref]),
                               "% de las importaciones de DAP y MAP de EE.UU."), 55),
       subtitle = env_wrap(paste0("Participación en el valor importado por EE.UU., ", anio_pre, " frente a ", anio_ref, " (en toneladas: ",
                                  fmt(usa_mar$part_ton[usa_mar$anio == anio_pre]), "% a ", fmt(usa_mar$part_ton[usa_mar$anio == anio_ref]), "%). ",
                                  "Segmento = tasa mínima y máxima del año."), 90),
       x = NULL, y = NULL,
       caption = env_wrap(paste0(pie_usa, " Tasa: cvd_tasas_deposito_ocp.csv. Medida provisional del ", lab_fecha(fecha_prelim), " al ", lab_fecha(fecha_fin_prov),
                                 " a la tasa preliminar (", fmt(tasa_prelim, 0.01), "%). Es correlación temporal, no un efecto causal aislado."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g2
guardar_png(g2, "g_fert_usa_importaciones_marruecos.png", 11, 7.5)

# G3: desvío hacia otros orígenes (participación en el valor importado por EE.UU.)
pal_origen <- setNames(c(color_rojo, "#3E3A36", color_texto_sec, color_gris_dato, color_grilla_tp2), niveles_origen)
col_txt <- setNames(c("white", "white", "white", color_texto_tp2, color_texto_tp2), niveles_origen)
otro1_pre <- origen$peso_valor[origen$anio == anio_pre & origen$grupo == nombre_es(top_otros[1])]
otro1_ref <- origen$peso_valor[origen$anio == anio_ref & origen$grupo == nombre_es(top_otros[1])]
stopifnot(length(otro1_pre) == 1, length(otro1_ref) == 1)
g3 <- ggplot(origen, aes(factor(anio), peso_valor, fill = grupo)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = if_else(peso_valor >= 6, paste0(fmt(peso_valor), "%"), ""), color = grupo),
            position = position_stack(vjust = 0.5), family = "rajdhani", fontface = "bold", size = 4.2, show.legend = FALSE) +
  geom_vline(xintercept = which(sort(unique(origen$anio)) == anio_orden) - 0.5, linetype = "dashed", color = color_texto_tp2) +
  annotate("text", x = which(sort(unique(origen$anio)) == anio_orden) - 0.45, y = 103, hjust = 0, vjust = 0, family = "rajdhani", size = 3.9,
           color = color_texto_tp2, label = paste0("Medida provisional (", format(fecha_prelim, "%m/%Y"), ") y orden (", format(fecha_orden, "%m/%Y"), ")")) +
  scale_fill_manual(values = pal_origen, name = NULL) + scale_color_manual(values = col_txt, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
  labs(title = env_wrap(paste0("Desvío de comercio: ", nombre_es(top_otros[1]), " pasó de ", fmt(otro1_pre), "% a ", fmt(otro1_ref),
                               "% de las importaciones de DAP y MAP de EE.UU."), 55),
       subtitle = env_wrap(paste0("Participación en el valor importado por EE.UU. Los tres mayores orígenes distintos de Marruecos son ",
                                  paste(nombre_es(top_otros), collapse = ", "), "; el resto se agrupa en 'Otros'."), 95),
       x = NULL, y = "Participación en el valor importado",
       caption = env_wrap(paste0(pie_usa, " ", txt_rusia), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g3
guardar_png(g3, "g_fert_usa_desvio_origenes.png", 11, 6.5)

# G4: precio unitario de MAP (Marruecos vs. otros) y Pink Sheet
g4_d <- precio_map %>% filter(!is.na(vu_mar)) %>%
  transmute(anio, `Marruecos (MAP)` = vu_mar, `Otros orígenes (MAP)` = vu_otros, `DAP spot f.o.b. US Gulf (Pink Sheet)` = dap_pinksheet) %>%
  pivot_longer(-anio, names_to = "serie", values_to = "usd_t") %>% filter(!is.na(usd_t))
etq_serie <- c("Marruecos (MAP)" = "Marruecos (MAP)", "Otros orígenes (MAP)" = "Otros orígenes (MAP)", "DAP spot f.o.b. US Gulf (Pink Sheet)" = "DAP Pink Sheet")
g4_d <- g4_d %>% mutate(etq = unname(etq_serie[serie]))
g4_col <- c("Marruecos (MAP)" = color_rojo, "Otros orígenes (MAP)" = "#3E3A36", "DAP spot f.o.b. US Gulf (Pink Sheet)" = color_gris_dato)
g4 <- ggplot(g4_d, aes(anio, usd_t, color = serie, linetype = serie)) +
  geom_line(linewidth = 1.4) + geom_point(size = 2.6) +
  geom_text_repel(data = g4_d %>% filter(anio == max(anio)), aes(label = etq), hjust = 0, direction = "y", nudge_x = 0.12, xlim = c(NA, Inf),
                  family = "rajdhani", fontface = "bold", size = 4.2, segment.color = NA, show.legend = FALSE, box.padding = 0.35) +
  scale_color_manual(values = g4_col, guide = "none") +
  scale_linetype_manual(values = c("Marruecos (MAP)" = "solid", "Otros orígenes (MAP)" = "solid", "DAP spot f.o.b. US Gulf (Pink Sheet)" = "dashed"), guide = "none") +
  scale_x_continuous(breaks = sort(unique(g4_d$anio)), expand = expansion(mult = c(0.02, 0.30))) +
  scale_y_continuous(labels = eje_num, limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(title = env_wrap("El precio unitario de MAP de Marruecos no muestra descuento frente al de otros orígenes", 55),
       subtitle = env_wrap(paste0("Precio unitario de MAP de Marruecos / otros orígenes: ", fmt(ratio_pre, 0.01), " en ", anio_pre, " y ", fmt(ratio_ref, 0.01),
                                  " en ", anio_ref, ". Sin descuento visible de precio, el ajuste parece haber sido en cantidades (indicio de que el área e es chica)."), 90),
       x = NULL, y = "USD por tonelada",
       caption = env_wrap(paste0(pie_usa, " Precio unitario = valor aduanero / cantidad, sin el derecho; mezcla de grados. El Pink Sheet es DAP f.o.b. US Gulf (otro producto y base), solo como referencia."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g4
guardar_png(g4, "g_fert_precio_unitario_map.png", 11, 6)

# G5: derecho pagado t x M en el año base (c + e)
g5_d <- rec_ref %>% mutate(alcance = factor(alcance, levels = rev(c("DAP y MAP", "Alcance de la orden (HS6 principales)"))))
g5 <- ggplot(g5_d, aes(derecho_central_M, alcance)) +
  geom_col(aes(fill = alcance), width = 0.55, show.legend = FALSE) +
  geom_linerange(aes(xmin = derecho_min_M, xmax = derecho_max_M), linewidth = 1.6, color = color_texto_tp2) +
  geom_text(aes(x = 0, label = paste0("central ", fmt(derecho_central_M), " M USD (rango ", fmt(derecho_min_M), "-", fmt(derecho_max_M),
                                      "); base ", fmt(base_M), " M USD")),
            hjust = 0, nudge_y = 0.42, family = "rajdhani", fontface = "bold", size = 4.2, color = color_texto_tp2) +
  scale_fill_manual(values = setNames(c(color_rojo, color_gris_dato), levels(g5_d$alcance)[c(2, 1)])) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08)), labels = eje_num) +
  labs(title = env_wrap(paste0("Derecho pagado por las exportaciones de OCP a EE.UU. en ", anio_ref, " (área c + e = t x M)"), 55),
       subtitle = env_wrap(paste0("Tasa del año: ", fmt(rec_dm$tasa_prom, 0.01), "% (promedio ponderado por días de ", fmt(rec_dm$tasa_max, 0.01), "% hasta el ",
                                  lab_fecha(tasas$vigente_desde[tasas$tasa_pct == rec_dm$tasa_min & !is.na(tasas$tasa_pct)][1] - 1), " y ", fmt(rec_dm$tasa_min, 0.01),
                                  "% desde el ", lab_fecha(tasas$vigente_desde[tasas$tasa_pct == rec_dm$tasa_min & !is.na(tasas$tasa_pct)][1]),
                                  "). Es la recaudación de EE.UU. (c + e); la parte que soporta Marruecos (e, pérdida de términos de intercambio) es como máximo este monto."), 90),
       x = "Millones de USD", y = NULL,
       caption = env_wrap(paste0(pie_usa, " Alcance: líneas HS6 principales del listado de la orden (cvd_alcance_orden.csv; valores de TP6_USA_M_SOCIOS_FERT_HS17). ",
                                 "Segmento = tasa mínima y máxima en vigor en el año. Supone importaciones parejas en el año y que el derecho se paga sobre el valor aduanero; ",
                                 "no descuenta exclusiones del alcance (p. ej. grados técnicos) ni la suspensión por envío de 2026."), 125)) +
  theme_tp2() + theme(panel.grid.major.y = element_blank())
g5
guardar_png(g5, "g_fert_derecho_pagado_tM.png", 11, 5.5)

# G6: destinos de las exportaciones de DAP y MAP de Marruecos
g6_d <- destino %>% select(anio, `EE.UU.` = eeuu_M, `Resto del mundo` = resto_M) %>% pivot_longer(-anio, names_to = "destino", values_to = "M") %>%
  mutate(destino = factor(destino, levels = c("Resto del mundo", "EE.UU.")))
g6_tot <- destino %>% transmute(anio, total_M, etq = paste0(fmt(total_M), "\n(EE.UU.: ", fmt(part_eeuu), "%)"))
g6 <- ggplot(g6_d, aes(factor(anio), M, fill = destino)) +
  geom_col(width = 0.68) +
  geom_text(data = g6_tot, aes(factor(anio), total_M, label = etq), inherit.aes = FALSE, vjust = -0.3, family = "rajdhani", fontface = "bold",
            size = 4, color = color_texto_tp2, lineheight = 0.95) +
  scale_fill_manual(values = c("EE.UU." = color_rojo, "Resto del mundo" = color_gris_dato), name = NULL, breaks = c("EE.UU.", "Resto del mundo")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = eje_num) +
  labs(title = env_wrap(paste0("EE.UU. pasó de ", fmt(destino$part_eeuu[destino$anio == anio_pre]), "% a ", fmt(destino$part_eeuu[destino$anio == anio_ref]),
                               "% de las exportaciones de DAP y MAP de Marruecos"), 55),
       subtitle = env_wrap(paste0("Exportaciones totales de DAP y MAP: ", fmt(destino$total_M[destino$anio == anio_pre]), " M USD en ", anio_pre, " y ",
                                  fmt(destino$total_M[destino$anio == anio_ref]), " M USD en ", anio_ref, " (nominal). El precio DAP del Pink Sheet pasó de ",
                                  fmt(pink$dap_pinksheet[pink$anio == anio_pre]), " a ", fmt(pink$dap_pinksheet[pink$anio == anio_ref]), " USD/t."), 90),
       x = NULL, y = "Millones de USD",
       caption = env_wrap(paste0("Fuente: WITS, Marruecos como reportero (TP6_MAR_X_SOCIOS_FERT_HS17), HS 2017 310530 y 310540; precio: Pink Sheet (pinksheet_dap_anual.csv). ",
                                 "Se usa hasta ", anio_ref, " (ver chequeo del año siguiente en la consola). Las importaciones que registra EE.UU. desde Marruecos difieren de las exportaciones que registra Marruecos hacia EE.UU. (brecha de espejo)."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g6
guardar_png(g6, "g_fert_marruecos_destinos.png", 11, 6.5)

# G7: esquema de tres mercados (SIN ESCALA): EE.UU. importador, mercado mundial, Marruecos exportador
# Curvas lineales ilustrativas: precio = a + b x cantidad. Los equilibrios se calculan de esas curvas.
us_S <- c(a = 20, b = 0.6);  us_D <- c(a = 140, b = -0.6)     # EE.UU.: oferta y demanda
ex_O <- c(a = 10, b = 0.4);  ex_D <- c(a = 70,  b = -0.4)     # Marruecos: oferta y demanda
t_il <- 20                                                     # derecho específico ilustrativo
q_S  <- function(p) (p - us_S["a"]) / us_S["b"];  q_D  <- function(p) (p - us_D["a"]) / us_D["b"]
q_O  <- function(p) (p - ex_O["a"]) / ex_O["b"];  q_Dx <- function(p) (p - ex_D["a"]) / ex_D["b"]
DM <- function(p) unname(q_D(p) - q_S(p)); XS <- function(p) unname(q_O(p) - q_Dx(p))
Pw <- uniroot(function(p) DM(p) - XS(p), c(0, 200))$root                 # precio mundial con libre comercio
PT <- uniroot(function(p) DM(p) - XS(p - t_il), c(0, 200))$root          # precio en EE.UU. con derecho (P* + t)
Ps <- PT - t_il                                                          # precio que recibe el exportador (P*)
stopifnot(Ps < Pw, Pw < PT, DM(PT) > 0)
S1 <- unname(q_S(Pw)); S2 <- unname(q_S(PT)); D1 <- unname(q_D(Pw)); D2 <- unname(q_D(PT)); M1 <- DM(Pw); M2 <- DM(PT)
O1 <- unname(q_O(Pw)); O2 <- unname(q_O(Ps)); Dx1 <- unname(q_Dx(Pw)); Dx2 <- unname(q_Dx(Ps))
stopifnot(abs((O2 - Dx2) - M2) < 1e-9, abs((O1 - Dx1) - M1) < 1e-9)      # exportaciones = importaciones

pn <- c("(a) Mercado de EE.UU. (importador)", "(b) Mercado mundial", "(c) Mercado de Marruecos (exportador)")
ylim_s <- c(30, 92); lim_x <- c(1.15 * D1, 1.6 * M1, 1.25 * O1)
pp <- seq(ylim_s[1], ylim_s[2], by = 0.5)
curvas <- bind_rows(
  tibble(panel = pn[1], curva = "S", x = unname(q_S(pp)), y = pp), tibble(panel = pn[1], curva = "D", x = unname(q_D(pp)), y = pp),
  tibble(panel = pn[2], curva = "DM", x = DM(pp), y = pp), tibble(panel = pn[2], curva = "XS", x = XS(pp), y = pp),
  tibble(panel = pn[2], curva = "XS + t", x = XS(pp), y = pp + t_il),
  tibble(panel = pn[3], curva = "O*", x = unname(q_O(pp)), y = pp), tibble(panel = pn[3], curva = "D*", x = unname(q_Dx(pp)), y = pp)) %>%
  mutate(lim = lim_x[match(panel, pn)], y_corte = c(ylim_s[1], ylim_s[1], 40)[match(panel, pn)]) %>%
  filter(x >= 0, x <= lim, y >= y_corte, y <= ylim_s[2])
precios <- bind_rows(
  tibble(panel = pn[1], p = c(PT, Pw, Ps), tipo = c("P* + t", "Pw", "P*")),
  tibble(panel = pn[2], p = c(PT, Pw, Ps), tipo = c("P* + t", "Pw", "P*")),
  tibble(panel = pn[3], p = c(Pw, Ps),     tipo = c("Pw", "P*")))
poly <- bind_rows(
  tibble(panel = pn[1], area = "a", fam = "a", x = c(0, S1, S2, 0),   y = c(Pw, Pw, PT, PT)),
  tibble(panel = pn[1], area = "b", fam = "perdida", x = c(S1, S2, S2), y = c(Pw, PT, Pw)),
  tibble(panel = pn[1], area = "c", fam = "c", x = c(S2, D2, D2, S2), y = c(Pw, Pw, PT, PT)),
  tibble(panel = pn[1], area = "d", fam = "perdida", x = c(D2, D1, D2), y = c(Pw, Pw, PT)),
  tibble(panel = pn[1], area = "e", fam = "e", x = c(S2, D2, D2, S2), y = c(Ps, Ps, Pw, Pw)),
  tibble(panel = pn[2], area = "c", fam = "c", x = c(0, M2, M2, 0),   y = c(Pw, Pw, PT, PT)),
  tibble(panel = pn[2], area = "e", fam = "e", x = c(0, M2, M2, 0),   y = c(Ps, Ps, Pw, Pw)),
  tibble(panel = pn[2], area = "b + d", fam = "perdida", x = c(M2, M2, M1), y = c(PT, Pw, Pw)),
  tibble(panel = pn[2], area = "f", fam = "perdida", x = c(M2, M2, M1), y = c(Pw, Ps, Pw)),
  tibble(panel = pn[3], area = "e", fam = "e", x = c(Dx2, O2, O2, Dx2), y = c(Ps, Ps, Pw, Pw)),
  tibble(panel = pn[3], area = "f ", fam = "perdida", x = c(Dx2, Dx2, Dx1), y = c(Ps, Pw, Pw)),
  tibble(panel = pn[3], area = "f", fam = "perdida", x = c(O2, O2, O1), y = c(Ps, Pw, Pw)))
etq_area <- tribble(
  ~panel, ~area, ~x, ~y,
  pn[1], "a",     S1 * 0.55,           (Pw + PT) / 2,
  pn[1], "b",     S1 + (S2 - S1) * 0.62, Pw + (PT - Pw) * 0.22,
  pn[1], "c",     (S2 + D2) / 2,       (Pw + PT) / 2,
  pn[1], "d",     D2 + (D1 - D2) * 0.30, Pw + (PT - Pw) * 0.20,
  pn[1], "e",     (S2 + D2) / 2,       (Pw + Ps) / 2,
  pn[2], "c",     M2 / 2,              (Pw + PT) / 2,
  pn[2], "e",     M2 / 2,              (Pw + Ps) / 2,
  pn[2], "b + d", M2 + (M1 - M2) * 0.34, Pw + (PT - Pw) * 0.18,
  pn[2], "f",     M2 + (M1 - M2) * 0.34, Pw - (Pw - Ps) * 0.28,
  pn[3], "e",     (Dx2 + O2) / 2,      (Pw + Ps) / 2,
  pn[3], "f ",    Dx2 + (Dx1 - Dx2) * 0.22, Pw - (Pw - Ps) * 0.25,
  pn[3], "f",     O2 + (O1 - O2) * 0.28, Pw - (Pw - Ps) * 0.22) %>% mutate(area = str_trim(area))
etq_x <- tribble(
  ~panel, ~x, ~lab,
  pn[1], S1, "S1", pn[1], S2, "S2", pn[1], D2, "D2", pn[1], D1, "D1",
  pn[2], M2, "M2", pn[2], M1, "M1",
  pn[3], Dx1, "D*1", pn[3], Dx2, "D*2", pn[3], O2, "O*2", pn[3], O1, "O*1")
# etiquetas de curva en el extremo derecho de cada curva
etq_curva <- curvas %>% group_by(panel, curva) %>% slice_max(x, n = 1, with_ties = FALSE) %>% ungroup() %>% mutate(hj = -0.25, vj = 0.5)
# D* se rotula en su extremo superior izquierdo (el extremo derecho es el cruce con O*, el precio de autarquía)
etq_curva <- etq_curva %>% mutate(x = if_else(curva == "D*", unname(q_Dx(66)), x), y = if_else(curva == "D*", 66, y),
                                  hj = if_else(curva == "D*", -0.3, hj), vj = if_else(curva == "D*", -0.35, vj))
fam_col <- c("a" = color_verde, "perdida" = color_rojo, "c" = color_ambar, "e" = color_azul)
g7 <- ggplot() +
  geom_polygon(data = poly, aes(x, y, fill = fam, group = interaction(panel, area)), alpha = 0.55, color = NA) +
  geom_line(data = curvas, aes(x, y, group = interaction(panel, curva), linetype = curva == "XS + t"), linewidth = 1.2, color = color_texto_tp2, show.legend = FALSE) +
  geom_hline(data = precios, aes(yintercept = p), linetype = "dashed", color = color_texto_sec) +
  geom_text(data = precios, aes(x = -Inf, y = p, label = paste0(tipo, "  ")), hjust = 1, vjust = 0.5, family = "rajdhani",
            fontface = "bold", size = 4.2, color = color_texto_tp2) +
  geom_text(data = etq_area, aes(x, y, label = area), family = "rajdhani", fontface = "bold", size = 5.5, color = color_texto_tp2) +
  geom_text(data = etq_x, aes(x, y = ylim_s[1] + 2, label = lab), family = "rajdhani", fontface = "bold", size = 4, color = color_texto_tp2) +
  geom_text(data = etq_curva, aes(x, y, label = curva, hjust = hj, vjust = vj), family = "rajdhani", fontface = "bold", size = 4.4, color = color_texto_tp2) +
  facet_wrap(~ factor(panel, levels = pn), scales = "free_x") +
  scale_fill_manual(values = fam_col, guide = "none") + scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dotted"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(ylim = ylim_s, clip = "off") +
  labs(title = env_wrap("Derecho de EE.UU. sobre un exportador grande: efectos en el importador, el mercado mundial y Marruecos", 62),
       subtitle = env_wrap(paste0("EE.UU.: variación del EC = -(a+b+c+d); del EP = +a; recaudación = c + e; bienestar = e - b - d. Marruecos: pérdida de términos de intercambio = -e; ",
                                  "pérdida de eficiencia = -f. En ", anio_ref, ", c + e = t x M = ", fmt(rec_dm$derecho_central_M), " M USD en DAP y MAP (rango ",
                                  fmt(rec_dm$derecho_min_M), "-", fmt(rec_dm$derecho_max_M), "). Marruecos exporta el ", fmt(part_dap), "% (DAP) y el ",
                                  fmt(part_map), "% (MAP) del total mundial."), 105),
       x = "Cantidad", y = NULL,
       caption = env_wrap(paste0("Esquema sin escala (curvas lineales ilustrativas, un único exportador). c + e = t x M se mide con datos; b + d se calibra (",
                                 fmt(min(cal_c$perdida_eficiencia), 0.1), "-", fmt(max(cal_c$perdida_eficiencia), 0.1), " M USD, gráfico calibrado); e ~ 0 por los precios unitarios. ",
                                 "El área a solo tiene un orden de magnitud: con producción de EE.UU. equivalente al ", fmt(tot_of$autoabastecimiento), "% de su consumo (USDA ERS, ", anio_oferta,
                                 "), cada 1% que suba el precio interno equivale a unos ", fmt(a_por_pp), " M USD para los productores. f no se estima (falta la elasticidad de oferta de OCP). ",
                                 "El derecho recae solo sobre OCP y Rusia; otros proveedores entran con NMF 0%, por lo que la demanda que enfrenta OCP es más elástica que la del esquema ",
                                 "(menos traslado al precio, e chica, y más pérdida de cantidad, f). EE.UU. como comprador: sus importaciones equivalen al ", fmt(part_usa_ref, 0.1),
                                 "% de las exportaciones mundiales de DAP y MAP en ", anio_ref, " (entre ", fmt(part_usa_min, 0.1), "% y ", fmt(part_usa_max, 0.1), "% en ",
                                 min(comp_dm$anio), "-", max(comp_dm$anio), ": en el límite entre intermedio y grande) y era el destino del ", fmt(part_eeuu_pre),
                                 "% de las de Marruecos en ", anio_pre, ". Además exporta casi lo mismo que importa (saldo ", fmt(saldo_usa_ref), " M USD en ", anio_ref, ")."), 140)) +
  theme_tp2() +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(color = color_texto_tp2),
        panel.spacing = unit(3, "lines"), plot.margin = margin(18, 34, 14, 50))
g7
guardar_png(g7, "g_fert_bienestar_esquema.png", 14, 7)

# G8: bienestar de EE.UU. calibrado en el mercado del DAP/MAP marroquí
g8_d <- calib %>% mutate(lab = case_when(escenario == "cota_baja_OCP" ~ paste0("Elasticidad ", fmt(eps, 0.01), "\n(OCP, cota baja)"),
                                         TRUE ~ paste0("Elasticidad ", fmt(eps, 1), "\n(USITC)"))) %>%
  transmute(lab = factor(lab, levels = lab), `Recaudación (c)` = recaudacion_c, `Pérdida de los compradores de EE.UU. (-(c + b + d))` = -perdida_EC,
            `Bienestar neto de EE.UU. (-(b + d))` = bienestar_neto_EEUU) %>%
  pivot_longer(-lab, names_to = "comp", values_to = "M") %>%
  mutate(comp = factor(comp, levels = c("Recaudación (c)", "Pérdida de los compradores de EE.UU. (-(c + b + d))", "Bienestar neto de EE.UU. (-(b + d))")))
g8 <- ggplot(g8_d, aes(lab, M, fill = comp)) +
  geom_hline(yintercept = 0, color = color_texto_tp2) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_text(aes(label = fmt(M, 0.1), vjust = if_else(M >= 0, -0.4, 1.3)), position = position_dodge(width = 0.78),
            family = "rajdhani", fontface = "bold", size = 4, color = color_texto_tp2) +
  scale_fill_manual(values = c(color_ambar, color_rojo, "#3E3A36"), name = NULL) +
  scale_y_continuous(labels = eje_num, expand = expansion(mult = c(0.12, 0.12))) +
  labs(title = env_wrap(paste0("Calibrado: el derecho le cuesta a EE.UU. entre ", fmt(min(-cal_c$bienestar_neto_EEUU)), " y ", fmt(max(-cal_c$bienestar_neto_EEUU)),
                               " M USD netos en ", anio_ref), 55),
       subtitle = env_wrap(paste0("Mercado del DAP y MAP marroquí, ", anio_ref, ". Derecho medio ", fmt(100 * t_cal, 0.01), "%; importaciones ", fmt(M1_cal), " M USD. ",
                                  "Sin absorción del exportador (precio relativo de MAP ", fmt(ratio_pre, 0.01), " -> ", fmt(ratio_ref, 0.01), "), la recaudación es transferencia ",
                                  "de compradores al fisco y la pérdida neta es b + d."), 95),
       x = NULL, y = "Millones de USD",
       caption = env_wrap(paste0("Calibración, no medición: demanda de elasticidad constante para la variedad marroquí; elasticidades de USITC Pub. 5172 p. II-29 (4 a 6) y de OCP (nota 45). ",
                                 "Contraste: la elasticidad de importaciones de MAP de EE.UU. estimada por wiiw (Ghodsi et al. 2016, datos 1996-2014) es ", fmt(eps_us_map, 0.01),
                                 " (p = ", fmt(p_us_map, 0.001), "), cerca del extremo bajo de la USITC. ",
                                 "Las importaciones sin derecho implícitas (", fmt(min(cal_c$M0_contrafactico)), "-", fmt(max(cal_c$M0_contrafactico)), " M USD) son menores que las observadas en ",
                                 anio_pre, " (", fmt(M_pre_obs), " M USD): la caída real no se explica solo por el derecho. ",
                                 "Los productores de EE.UU. (área a) solo tienen un orden de magnitud: cerca de ", fmt(a_por_pp, 0.1), " M USD por cada 1% que sube el precio interno (USDA ERS, ", anio_oferta, "); ",
                                 "no se estima f (sin elasticidad de oferta de OCP)."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
guardar_png(g8, "g_fert_bienestar_calibrado.png", 11, 6.5)

# G9: el instrumento de Marruecos: subsidios a OCP por programa
g9_d <- sub %>% mutate(grupo = case_when(tipo == "exportacion" ~ "Exportación",
                                         str_detect(tipo, "insumo") ~ "Producción (insumo minero)", TRUE ~ "Producción (otros)"),
                       programa_es = fct_reorder(programa_es, tasa_pct))
g9 <- ggplot(g9_d, aes(tasa_pct, programa_es, fill = grupo)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(fmt(tasa_pct, 0.01), "%")), hjust = -0.1, family = "rajdhani", fontface = "bold", size = 4.2, color = color_texto_tp2) +
  scale_fill_manual(values = c("Producción (insumo minero)" = color_rojo, "Exportación" = color_ambar, "Producción (otros)" = color_gris_dato), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)), labels = function(x) paste0(x, "%")) +
  labs(title = env_wrap(paste0("El instrumento de Marruecos: subsidios a OCP equivalentes al ", fmt(tasa_final_cron, 0.01), "% de sus ventas"), 55),
       subtitle = env_wrap(paste0("Subsidios medidos por el Departamento de Comercio de EE.UU. (POI ", anio_poi, "). El ",
                                  fmt(sub_tipo$peso[sub_tipo$grupo == "Subsidio a la producción: insumo minero"]), "% es la roca fosfórica cedida por debajo de su valor (rojo: subsidio a la producción); ",
                                  "el subsidio a la exportación propiamente dicho (ámbar) es ", fmt(sub_tipo$tasa_pct[sub_tipo$grupo == "Subsidio a la exportación"], 0.01),
                                  "%. Orden de magnitud sobre las exportaciones de DAP y MAP: ", fmt(sub_monto$subsidio_M[1]), " M USD (", sub_monto$anio[1], ") y ",
                                  fmt(sub_monto$subsidio_M[2]), " M USD (", sub_monto$anio[2], ", tasa ", fmt(sub_monto$tasa_pct[2], 0.01), "%)."), 95),
       x = "% ad valorem sobre las ventas de OCP", y = NULL,
       caption = env_wrap(paste0("Fuente: Commerce, Issues and Decision Memorandum de la determinación final C-714-001, pp. 5-6 (subsidios_ocp_programas.csv); exportaciones: WITS, Marruecos como reportero. ",
                                 "Los programas suman la tasa final. El monto aplica la tasa a las exportaciones de DAP y MAP del mismo año: es un orden de magnitud. ",
                                 "El derecho compensatorio de EE.UU. replica esta tasa para neutralizar el subsidio en su mercado."), 125)) +
  theme_tp2() + theme(panel.grid.major.y = element_blank(), legend.position = "none")
guardar_png(g9, "g_fert_subsidios_ocp.png", 11, 6)

# G10: esquema del exportador grande que subsidia (el instrumento del lado de Marruecos). SIN ESCALA.
# Fila superior: subsidio a la exportación (lo que mide la fila "exportación" del gráfico anterior).
# Fila inferior: subsidio a la producción (el insumo minero, que es la mayor parte del subsidio medido).
# Curvas lineales ilustrativas; los equilibrios y la identidad de bienestar se calculan de esas curvas.
ex_S <- c(a = 10, b = 0.4);  ex_Dm <- c(a = 70, b = -0.4)      # Marruecos: oferta y demanda internas
rw_S <- c(a = 20, b = 0.6);  rw_D  <- c(a = 140, b = -0.6)     # resto del mundo: oferta y demanda internas
s_il <- 11                                                      # subsidio por unidad, ilustrativo
qq   <- function(p, par) unname((p - par["a"]) / par["b"])
Sm   <- function(p) qq(p, ex_S);  Dmo <- function(p) qq(p, ex_Dm)
Mw   <- function(p) qq(p, rw_D) - qq(p, rw_S)                   # demanda de importaciones del resto del mundo
XS0  <- function(p) Sm(p) - Dmo(p)                              # oferta exportable de Marruecos sin subsidio
XSe  <- function(p) XS0(p + s_il)                               # con subsidio a la exportación (p = precio mundial)
XSp  <- function(p) Sm(p + s_il) - Dmo(p)                       # con subsidio a la producción
Pw2  <- uniroot(function(p) Mw(p) - XS0(p), c(30, 100))$root    # precio mundial sin subsidio
Pe_s <- uniroot(function(p) Mw(p) - XSe(p), c(30, 100))$root    # precio mundial con subsidio a la exportación (P*)
Pp_s <- uniroot(function(p) Mw(p) - XSp(p), c(30, 100))$root    # precio mundial con subsidio a la producción (P*)
Pe_h <- Pe_s + s_il                                             # precio interno de Marruecos con subsidio a la exportación
Pp_e <- Pp_s + s_il                                             # precio efectivo que recibe el productor (P* + s)
stopifnot(Pe_s < Pw2, Pw2 < Pe_h, Pp_s < Pw2, Pw2 < Pp_e)
# cantidades: E = subsidio a la exportación, P = subsidio a la producción
E_S1 <- Sm(Pw2); E_S2 <- Sm(Pe_h); E_D1 <- Dmo(Pw2); E_D2 <- Dmo(Pe_h); E_X1 <- E_S1 - E_D1; E_X2 <- E_S2 - E_D2
P_S1 <- Sm(Pw2); P_S2 <- Sm(Pp_e); P_D1 <- Dmo(Pw2); P_D2 <- Dmo(Pp_s); P_X1 <- P_S1 - P_D1; P_X2 <- P_S2 - P_D2
stopifnot(abs(E_X2 - Mw(Pe_s)) < 1e-9, abs(P_X2 - Mw(Pp_s)) < 1e-9, abs(E_X1 - Mw(Pw2)) < 1e-9)
# identidad de bienestar de Marruecos: dEC + dEP - costo fiscal = -(b + d) - e   (e = pérdida de términos de intercambio)
E_b <- 0.5 * (E_S2 - E_S1) * (Pe_h - Pw2); E_d <- 0.5 * (E_D1 - E_D2) * (Pe_h - Pw2); E_e <- (Pw2 - Pe_s) * E_X2
E_neto <- -(Pe_h - Pw2) * (E_D1 + E_D2) / 2 + (Pe_h - Pw2) * (E_S1 + E_S2) / 2 - s_il * E_X2
P_b <- 0.5 * (P_S2 - P_S1) * (Pp_e - Pw2); P_d <- 0.5 * (P_D2 - P_D1) * (Pw2 - Pp_s); P_e <- (Pw2 - Pp_s) * P_X2
P_neto <- (Pw2 - Pp_s) * (P_D1 + P_D2) / 2 + (Pp_e - Pw2) * (P_S1 + P_S2) / 2 - s_il * P_S2
stopifnot(abs(E_neto + E_b + E_d + E_e) < 1e-8, abs(P_neto + P_b + P_d + P_e) < 1e-8, E_neto < 0, P_neto < 0)

pn2 <- c("(a) Subsidio a la exportación: mercado de Marruecos", "(b) Subsidio a la exportación: mercado mundial",
         "(c) Subsidio a la producción: mercado de Marruecos",  "(d) Subsidio a la producción: mercado mundial")
yl2 <- c(32, 84); pp2 <- seq(yl2[1], yl2[2], by = 0.5)
lim2 <- c(1.3 * E_S2, 1.4 * E_X2, 1.3 * P_S2, 1.4 * P_X2)
curv2 <- bind_rows(
  tibble(panel = pn2[1], curva = "S",      x = Sm(pp2),  y = pp2),          tibble(panel = pn2[1], curva = "D", x = Dmo(pp2), y = pp2),
  tibble(panel = pn2[2], curva = "DM",     x = Mw(pp2),  y = pp2),          tibble(panel = pn2[2], curva = "XS", x = XS0(pp2), y = pp2),
  tibble(panel = pn2[2], curva = "XS - s", x = XS0(pp2), y = pp2 - s_il),
  tibble(panel = pn2[3], curva = "S",      x = Sm(pp2),  y = pp2),          tibble(panel = pn2[3], curva = "D", x = Dmo(pp2), y = pp2),
  tibble(panel = pn2[3], curva = "S - s",  x = Sm(pp2),  y = pp2 - s_il),
  tibble(panel = pn2[4], curva = "DM",     x = Mw(pp2),  y = pp2),          tibble(panel = pn2[4], curva = "XS", x = XS0(pp2), y = pp2),
  tibble(panel = pn2[4], curva = "XS'",    x = XSp(pp2), y = pp2)) %>%
  mutate(lim = lim2[match(panel, pn2)], sub = curva %in% c("XS - s", "S - s", "XS'")) %>%
  filter(x >= 0, x <= lim, y >= yl2[1], y <= yl2[2])
prec2 <- bind_rows(
  tibble(panel = pn2[1], p = c(Pe_h, Pw2, Pe_s), tipo = c("P* + s", "Pw", "P*")),
  tibble(panel = pn2[2], p = c(Pe_h, Pw2, Pe_s), tipo = c("P* + s", "Pw", "P*")),
  tibble(panel = pn2[3], p = c(Pp_e, Pw2, Pp_s), tipo = c("P* + s", "Pw", "P*")),
  tibble(panel = pn2[4], p = c(Pw2, Pp_s),       tipo = c("Pw", "P*")))
poly2 <- bind_rows(
  tibble(panel = pn2[1], area = "b", fam = "perdida", x = c(E_S1, E_S2, E_S2), y = c(Pw2, Pw2, Pe_h)),
  tibble(panel = pn2[1], area = "d", fam = "perdida", x = c(E_D2, E_D1, E_D2), y = c(Pw2, Pw2, Pe_h)),
  tibble(panel = pn2[1], area = "e", fam = "e",       x = c(E_D2, E_S2, E_S2, E_D2), y = c(Pe_s, Pe_s, Pw2, Pw2)),
  tibble(panel = pn2[2], area = "e", fam = "e",       x = c(0, E_X2, E_X2, 0), y = c(Pe_s, Pe_s, Pw2, Pw2)),
  tibble(panel = pn2[3], area = "b", fam = "perdida", x = c(P_S1, P_S2, P_S2), y = c(Pw2, Pw2, Pp_e)),
  tibble(panel = pn2[3], area = "d", fam = "perdida", x = c(P_D1, P_D2, P_D2), y = c(Pw2, Pw2, Pp_s)),
  tibble(panel = pn2[3], area = "e", fam = "e",       x = c(P_D2, P_S2, P_S2, P_D2), y = c(Pp_s, Pp_s, Pw2, Pw2)),
  tibble(panel = pn2[4], area = "e", fam = "e",       x = c(0, P_X2, P_X2, 0), y = c(Pp_s, Pp_s, Pw2, Pw2)))
rect2 <- tribble(~panel, ~xmin, ~xmax, ~ymin, ~ymax,
                 pn2[1], E_D2, E_S2, Pe_s, Pe_h,
                 pn2[2], 0,    E_X2, Pe_s, Pe_h,
                 pn2[3], 0,    P_S2, Pp_s, Pp_e)
etq2 <- tribble(~panel, ~lab, ~x, ~y, ~fam,
  pn2[1], "b", (E_S1 + 2 * E_S2) / 3, (2 * Pw2 + Pe_h) / 3, "area",
  pn2[1], "d", (2 * E_D2 + E_D1) / 3, (2 * Pw2 + Pe_h) / 3, "area",
  pn2[1], "e", (E_D2 + E_S2) / 2,     (Pe_s + Pw2) / 2,     "area",
  pn2[1], "costo = s x X2", (E_D2 + E_S2) / 2, Pe_h + 1.8,  "costo",
  pn2[2], "e", E_X2 / 2,              (Pe_s + Pw2) / 2,     "area",
  pn2[2], "costo = s x X2", E_X2 / 2, Pe_h + 1.8,           "costo",
  pn2[3], "b", (P_S1 + 2 * P_S2) / 3, (2 * Pw2 + Pp_e) / 3, "area",
  pn2[3], "d", (P_D1 + 2 * P_D2) / 3, (2 * Pw2 + Pp_s) / 3, "area",
  pn2[3], "e", (P_D2 + P_S2) / 2,     (Pp_s + Pw2) / 2,     "area",
  pn2[3], "costo = s x S2", P_S2 / 2, Pp_e + 1.8,           "costo",
  pn2[4], "e", P_X2 / 2,              (Pp_s + Pw2) / 2,     "area")
etq_x2 <- tribble(~panel, ~x, ~lab,
  pn2[1], E_S1, "S1", pn2[1], E_S2, "S2", pn2[1], E_D2, "D2", pn2[1], E_D1, "D1",
  pn2[2], E_X1, "X1", pn2[2], E_X2, "X2",
  pn2[3], P_S1, "S1", pn2[3], P_S2, "S2", pn2[3], P_D1, "D1", pn2[3], P_D2, "D2",
  pn2[4], P_X1, "X1", pn2[4], P_X2, "X2")
etq_c2 <- curv2 %>% group_by(panel, curva) %>% slice_max(x, n = 1, with_ties = FALSE) %>% ungroup()
g10 <- ggplot() +
  geom_polygon(data = poly2, aes(x, y, fill = fam, group = interaction(panel, area)), alpha = 0.55, color = NA) +
  geom_rect(data = rect2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = NA, color = color_ambar, linewidth = 0.9, linetype = "dashed") +
  geom_line(data = curv2, aes(x, y, group = interaction(panel, curva), linetype = sub), linewidth = 1.2, color = color_texto_tp2, show.legend = FALSE) +
  geom_hline(data = prec2, aes(yintercept = p), linetype = "dashed", color = color_texto_sec) +
  geom_text(data = prec2, aes(x = -Inf, y = p, label = paste0(tipo, "  ")), hjust = 1, vjust = 0.5, family = "rajdhani",
            fontface = "bold", size = 4.2, color = color_texto_tp2) +
  geom_text(data = filter(etq2, fam == "area"), aes(x, y, label = lab), family = "rajdhani", fontface = "bold", size = 5.5, color = color_texto_tp2) +
  geom_text(data = filter(etq2, fam == "costo"), aes(x, y, label = lab), family = "rajdhani", fontface = "bold", size = 4, color = "#8A6100") +
  geom_text(data = etq_x2, aes(x, y = yl2[1] + 2, label = lab), family = "rajdhani", fontface = "bold", size = 4, color = color_texto_tp2) +
  geom_text(data = etq_c2, aes(x, y, label = curva), hjust = -0.25, family = "rajdhani", fontface = "bold", size = 4.4, color = color_texto_tp2) +
  geom_text(data = tibble(panel = pn2, lab = pn2), aes(x = -Inf, y = yl2[2] + 5, label = lab), hjust = 0, family = "rajdhani",
            fontface = "bold", size = 5, color = color_texto_tp2) +
  facet_wrap(~ factor(panel, levels = pn2), scales = "free_x", ncol = 2) +
  scale_fill_manual(values = c("perdida" = color_rojo, "e" = color_azul), guide = "none") +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dotted"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(ylim = yl2, clip = "off") +
  labs(title = env_wrap("Marruecos como exportador grande que subsidia: baja el precio mundial y el país pierde términos de intercambio", 62),
       subtitle = env_wrap(paste0("Bienestar neto de Marruecos = variación del EC + variación del EP - costo fiscal = -(b + d) - e (b + d: pérdida de eficiencia; e: pérdida de términos de intercambio). ",
                                  "Los compradores del resto del mundo ganan lo que Marruecos pierde en e. Commerce mide el subsidio a OCP en ", fmt(tasa_final_cron, 0.01), "% de sus ventas (POI ", anio_poi, "): ",
                                  "solo ", fmt(sub_tipo$tasa_pct[sub_tipo$grupo == "Subsidio a la exportación"], 0.01), " puntos son a la exportación (fila superior); el resto actúa sobre la producción (fila inferior)."), 105),
       x = "Cantidad", y = NULL,
       caption = env_wrap(paste0("Esquema sin escala (curvas lineales ilustrativas): la forma de las curvas y el tamaño del subsidio por unidad no se calibran con datos. ",
                                 "El único dato del esquema es la tasa de subsidio de Commerce (subsidios_ocp_programas.csv) y su orden de magnitud sobre las exportaciones de DAP y MAP: ",
                                 fmt(sub_monto$subsidio_M[1]), " M USD (", sub_monto$anio[1], ") y ", fmt(sub_monto$subsidio_M[2]), " M USD (", sub_monto$anio[2], "). ",
                                 "No se estiman b, d y e en dólares: falta la elasticidad de oferta de OCP. El costo fiscal es el recuadro punteado (en la fila inferior es la renta minera cedida). ",
                                 "Quienes pierden con el menor precio mundial son los productores competidores, entre ellos los de EE.UU., que originaron el derecho compensatorio."), 140)) +
  theme_tp2() +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(color = color_texto_tp2),
        strip.text = element_blank(), plot.subtitle = element_text(color = color_texto_sec, size = 13, lineheight = 1.05, margin = margin(b = 30)),
        panel.spacing = unit(3, "lines"), plot.margin = margin(18, 34, 14, 50))
g10
guardar_png(g10, "g_fert_subsidio_exportador.png", 14, 10)

#===============================================================================#
# BLOQUE 6: RESUMEN PARA LAS DIAPOSITIVAS (b) y (c) DE FERTILIZANTES
#===============================================================================#
us_ref <- usa_mar %>% filter(anio == anio_ref); us_pre <- usa_mar %>% filter(anio == anio_pre)
resumen_fert <- tibble(
  indicador = c("Tasa del depósito del CVD al inicio del año base (%)", "Tasa del depósito del CVD al fin del año base (%)",
                "Tasa promedio del año base, ponderada por días (%)", "Arancel NMF de EE.UU. a DAP y MAP (%)",
                "Participación de Marruecos en importaciones de EE.UU. de DAP y MAP, valor, año previo (%)",
                "Participación de Marruecos en importaciones de EE.UU. de DAP y MAP, valor, año base (%)",
                "Importaciones de EE.UU. desde Marruecos de DAP y MAP, año base (M USD)",
                "Derecho pagado c + e = t x M en DAP y MAP, año base, central (M USD)",
                "Derecho pagado c + e = t x M en DAP y MAP, año base, rango (M USD)",
                "Derecho pagado c + e = t x M en el alcance de la orden, año base, central (M USD)",
                "Precio unitario MAP Marruecos / otros orígenes, año previo", "Precio unitario MAP Marruecos / otros orígenes, año base",
                "Exportaciones de DAP y MAP de Marruecos, año base (M USD)", "Participación de EE.UU. en esas exportaciones, año base (%)",
                "Participación de Marruecos en exportaciones mundiales de DAP, año base (%)",
                "Participación de Marruecos en exportaciones mundiales de MAP, año base (%)",
                "Importaciones de EE.UU. / exportaciones mundiales de DAP y MAP, año base (%)",
                "Participación de EE.UU. en las exportaciones de DAP y MAP de Marruecos, año previo (%)",
                "Parte del derecho absorbida por el exportador, implícita en precios (%)",
                "Pérdida de eficiencia calibrada b + d, USITC (M USD)",
                "Bienestar neto de EE.UU. calibrado, USITC (M USD)",
                "Producción de EE.UU. / consumo aparente de DAP y MAP (%)",
                "Participación aproximada de Marruecos en el consumo de EE.UU. (%)",
                "Área a si el precio interno de EE.UU. sube 1% (M USD, orden de magnitud)"),
  valor = c(tasa_en(as.Date(paste0(anio_ref, "-01-01"))), tasa_en(as.Date(paste0(anio_ref, "-12-31"))), rec_dm$tasa_prom, max(arancel_usa$tasa_general_pct),
            us_pre$part_valor, us_ref$part_valor, mUSD(us_ref$valor_mar), rec_dm$derecho_central_M, NA_real_, rec_al$derecho_central_M,
            ratio_pre, ratio_ref, destino$total_M[destino$anio == anio_ref], destino$part_eeuu[destino$anio == anio_ref], part_dap, part_map, part_usa_ref, part_eeuu_pre, 100 * theta, NA_real_, NA_real_,
            tot_of$autoabastecimiento, part_mar_consumo, a_por_pp),
  detalle = c("", "", paste0("mínimo ", fmt(rec_dm$tasa_min, 0.01), " - máximo ", fmt(rec_dm$tasa_max, 0.01)), "", paste0("año ", anio_pre), "", "", "",
              paste0(fmt(rec_dm$derecho_min_M, 0.1), "-", fmt(rec_dm$derecho_max_M, 0.1)), paste0("rango ", fmt(rec_al$derecho_min_M, 0.1), "-", fmt(rec_al$derecho_max_M, 0.1)),
              paste0("año ", anio_pre), "", "reporta Marruecos", "reporta Marruecos", "", "", tam_usa_ref, paste0("año ", anio_pre, "; reporta Marruecos"),
              if (theta <= 0) "sin evidencia de absorción (e ~ 0)" else "",
              paste0(fmt(min(cal_c$perdida_eficiencia), 0.1), "-", fmt(max(cal_c$perdida_eficiencia), 0.1), " (elasticidad 4-6)"),
              paste0(fmt(min(cal_c$bienestar_neto_EEUU), 0.1), " a ", fmt(max(cal_c$bienestar_neto_EEUU), 0.1)),
              paste0("año ", anio_oferta, "; USDA ERS (IFA)"), paste0("año ", anio_oferta, "; aproximación valor x nutriente"),
              paste0("año ", anio_oferta, "; c + e del mismo año = ", fmt(tM_oferta, 0.1), " M USD")))
cat("\nResumen para diapositivas:\n"); print(resumen_fert, n = Inf, width = 200)
write_csv(resumen_fert, paste0(ruta_tablas, "resumen_fertilizantes_b_c_", anio_ref, ".csv"))

#===============================================================================#
# FIN DEL SCRIPT 04. Salidas: tablas en output/tablas/03/, gráficos en output/graficos/03/.
#===============================================================================#

cat("\nGráficos en ", normalizePath(ruta_graficos), ":\n", sep = ""); print(list.files(ruta_graficos))
cat("Tablas en ", normalizePath(ruta_tablas), ":\n", sep = ""); print(list.files(ruta_tablas))
