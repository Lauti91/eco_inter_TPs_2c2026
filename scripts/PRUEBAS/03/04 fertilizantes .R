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
#
# CONVENCIONES PROPIAS DEL SCRIPT (no vienen del TP; ver nota de datos)
#   - Producto: DAP y MAP (HS 310530 y 310540), como en el inciso (a). El alcance de
#     la orden es más amplio; se calcula también (HS6 "principales" del listado).
#   - Derecho pagado (c + e = t x M): valor de importación de EE.UU. desde Marruecos
#     (valor aduanero de WITS, que es la base del derecho) x tasa del depósito. Como
#     WITS es anual, se supone que las importaciones se reparten parejo en el año:
#     la tasa del año es el promedio ponderado por días y el rango usa la tasa
#     mínima y máxima en vigor durante el año.
#   - Años sin tasa conocida (medida provisional con tasa preliminar no relevada,
#     suspensión por la Proclamación 11038) quedan NA: no se completan.
#   - 2023 es el año base: la revisión de ese período fue rescindida y los derechos
#     se liquidan a la tasa del depósito vigente al ingreso (cvd06). Para otros años
#     la liquidación definitiva depende de las revisiones, por eso son solo contexto.
#   - Exportaciones de Marruecos: se usan hasta el año base; 2024 se descarta por un
#     chequeo contra el precio DAP (se imprime).
#   - El esquema de tres mercados es SIN ESCALA (curvas lineales ilustrativas) y supone
#     un único exportador; el CVD recae solo sobre OCP y Rusia, y el resto de los
#     proveedores entra con NMF 0%.
#   - NO se cuantifican ΔEC, ΔEP, ni las pérdidas de eficiencia (b + d, f): requieren
#     producción y consumo de EE.UU. en toneladas de producto (tachados por la USITC)
#     y la elasticidad de oferta de OCP, que no están disponibles. Tampoco se verifica
#     con datos que EE.UU. sea "grande" en el mercado mundial (faltan importaciones
#     mundiales); el esquema ilustra la asimetría.
#===============================================================================#

for (p in c("tidyverse", "haven", "ggrepel", "scales", "showtext", "stringr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(tidyverse); library(haven); library(ggrepel); library(scales); library(showtext)

ruta_datos    <- "bases de datos/"
ruta_graficos <- "output/graficos/03/"
ruta_tablas   <- "output/tablas/03/"
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas,   recursive = TRUE, showWarnings = FALSE)

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

# Formato de números (igual que los textos de los scripts 02 y 03: punto decimal, espacio de miles)
dec <- "."; mil <- " "
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
fecha_orden  <- cron %>% filter(id == "cvd02") %>% pull(fecha_publicacion) %>% as.Date()
stopifnot(length(fecha_prelim) == 1, length(fecha_orden) == 1)
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
  geom_text(data = prov, aes(x = desde, y = max(tl_pos$tasa_pct) * 1.28, label = "Medida provisional\n(tasa preliminar no relevada)"),
            hjust = 0, vjust = 1, family = "rajdhani", size = 3.9, color = color_texto_sec, lineheight = 0.95) +
  geom_text(data = susp, aes(x = desde, y = max(tl_pos$tasa_pct) * 1.28, label = "Proclamación 11038:\npago suspendido por envío\n(sin verificar)"),
            hjust = 1.04, vjust = 1, family = "rajdhani", size = 3.9, color = color_texto_sec, lineheight = 0.95) +
  scale_color_manual(values = c("TRUE" = color_rojo, "FALSE" = color_gris_dato), guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = c(x_ini, fecha_fin), expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.02, 0.05))) +
  coord_cartesian(ylim = c(0, max(tl_pos$tasa_pct) * 1.28), clip = "off") +
  labs(title = env_wrap("Derecho compensatorio de EE.UU. sobre OCP: tasa del depósito en vigor", 55),
       subtitle = env_wrap(paste0("Ad valorem sobre las importaciones de fosfatados desde Marruecos (OCP). Arancel NMF de EE.UU. a DAP y MAP: ",
                                  fmt(max(arancel_usa$tasa_general_pct)), "%. Tasas: ",
                                  paste(paste0(fmt(tasas$tasa_pct[!is.na(tasas$tasa_pct) & tasas$tasa_pct > 0], 0.01), "%"), collapse = ", "),
                                  ", en ese orden."), 90),
       x = NULL, y = "Tasa del depósito (% ad valorem)",
       caption = env_wrap(paste0(pie_cvd, " Sombreado gris: medida provisional, con tasa no relevada. El gráfico termina en la última fecha con fuente (",
                                 lab_fecha(fecha_fin), ")."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g1
ggsave(paste0(ruta_graficos, "g_fert_cvd_tasa_deposito.png"), g1, width = 11, height = 6, dpi = 300, bg = color_fondo_tp2)

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
       caption = env_wrap(paste0(pie_usa, " Tasa: ", "cvd_tasas_deposito_ocp.csv. 2020: medida provisional desde el ", lab_fecha(fecha_prelim),
                                 "; 2021: tasa provisional hasta el 29/03 (no relevada). Es correlación temporal, no un efecto causal aislado."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g2
ggsave(paste0(ruta_graficos, "g_fert_usa_importaciones_marruecos.png"), g2, width = 11, height = 7.5, dpi = 300, bg = color_fondo_tp2)

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
       caption = env_wrap(paste0(pie_usa, " Rusia tiene un CVD propio (otra tasa, no relevada en esta base)."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g3
ggsave(paste0(ruta_graficos, "g_fert_usa_desvio_origenes.png"), g3, width = 11, height = 6.5, dpi = 300, bg = color_fondo_tp2)

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
ggsave(paste0(ruta_graficos, "g_fert_precio_unitario_map.png"), g4, width = 11, height = 6, dpi = 300, bg = color_fondo_tp2)

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
ggsave(paste0(ruta_graficos, "g_fert_derecho_pagado_tM.png"), g5, width = 11, height = 5.5, dpi = 300, bg = color_fondo_tp2)

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
ggsave(paste0(ruta_graficos, "g_fert_marruecos_destinos.png"), g6, width = 11, height = 6.5, dpi = 300, bg = color_fondo_tp2)

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
       caption = env_wrap(paste0("Esquema sin escala (curvas lineales ilustrativas, un único exportador). Solo c + e = t x M se cuantifica con datos; ",
                                 "las áreas a, b, d, e y f no se estiman: faltan producción y consumo de EE.UU. en toneladas de producto y la elasticidad de oferta de OCP. ",
                                 "El derecho recae solo sobre OCP y Rusia; otros proveedores entran con NMF 0%, por lo que la demanda que enfrenta OCP es más elástica que la del esquema ",
                                 "(menos traslado al precio, e chica, y más pérdida de cantidad, f). Que EE.UU. sea 'grande' en el mercado mundial no se verifica con datos."), 140)) +
  theme_tp2() +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(color = color_texto_tp2),
        panel.spacing = unit(3, "lines"), plot.margin = margin(18, 34, 14, 50))
g7
ggsave(paste0(ruta_graficos, "g_fert_bienestar_esquema.png"), g7, width = 14, height = 7, dpi = 300, bg = color_fondo_tp2)

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
                "Participación de Marruecos en exportaciones mundiales de MAP, año base (%)"),
  valor = c(tasa_en(as.Date(paste0(anio_ref, "-01-01"))), tasa_en(as.Date(paste0(anio_ref, "-12-31"))), rec_dm$tasa_prom, max(arancel_usa$tasa_general_pct),
            us_pre$part_valor, us_ref$part_valor, mUSD(us_ref$valor_mar), rec_dm$derecho_central_M, NA_real_, rec_al$derecho_central_M,
            ratio_pre, ratio_ref, destino$total_M[destino$anio == anio_ref], destino$part_eeuu[destino$anio == anio_ref], part_dap, part_map),
  detalle = c("", "", paste0("mínimo ", fmt(rec_dm$tasa_min, 0.01), " - máximo ", fmt(rec_dm$tasa_max, 0.01)), "", paste0("año ", anio_pre), "", "", "",
              paste0(fmt(rec_dm$derecho_min_M, 0.1), "-", fmt(rec_dm$derecho_max_M, 0.1)), paste0("rango ", fmt(rec_al$derecho_min_M, 0.1), "-", fmt(rec_al$derecho_max_M, 0.1)),
              paste0("año ", anio_pre), "", "reporta Marruecos", "reporta Marruecos", "", ""))
cat("\nResumen para diapositivas:\n"); print(resumen_fert, n = Inf, width = 200)
write_csv(resumen_fert, paste0(ruta_tablas, "resumen_fertilizantes_b_c_", anio_ref, ".csv"))

#===============================================================================#
# FIN DEL SCRIPT 04. Salidas: tablas en output/tablas/04/, gráficos en output/graficos/04/.
#===============================================================================#