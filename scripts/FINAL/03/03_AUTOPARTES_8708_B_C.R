#===============================================================================#
# TP6 - Política Comercial - SCRIPT 3: AUTOPARTES (HS 8708), INCISOS (b) y (c)
#   Instrumento: arancel NMF aplicado de Marruecos (importación). País pequeño.
#===============================================================================#
# QUÉ HACE
#   (b) Instrumento y magnitud
#       - Perfil del arancel NMF aplicado 2023 en las 52 líneas de 10 dígitos de
#         HS 8708 (indicadores de la clase: media simple y ponderada, desvío, CV,
#         máximo, % libres, picos internacionales >15%).
#       - Origen de las importaciones por régimen (UE / otros acuerdos / Turquía /
#         sin preferencia verificada) y arancel efectivamente aplicable a cada una.
#       - Arancel consolidado (bound) de las líneas 8708 de la lista de la OMC.
#   (c) Bienestar (país pequeño, equilibrio parcial)
#       - Orden de magnitud de la recaudación (área c = t x M) con rango.
#       - Esquema gráfico de las áreas a, b, c, d (sin escala).
#
# BASES (carpeta "bases de datos/"). Todo dato empírico sale de estos archivos:
#   - spark_8708_nmf_2023_lineas.csv   : 52 líneas 8708, NMF 2023 (OMC/TTD), con URL.
#   - acuerdos_socios_8708.csv         : socios con preferencia (0%) y su fuente.
#   - TP6_MAR_M_SOCIOS_AUTOPARTES_HS17.dta : importaciones de Marruecos por HS6 y socio.
#   - TP6_MAR_ARANCEL_CONSOLIDADO_OMC.dta  : aranceles consolidados (lista 2002).
#
# CONVENCIONES PROPIAS DEL SCRIPT (no vienen del TP; ver nota de datos)
#   - Las importaciones de WITS llegan a HS6, no a 10 dígitos. Para pasar del HS6
#     a la tasa se usan tres valores: mínimo, promedio simple y máximo de las
#     líneas de 10 dígitos de ese HS6. El "central" (promedio simple) supone que
#     todas las líneas de un HS6 pesan lo mismo; el rango muestra cuánto puede
#     cambiar eso.
#   - Socios sin preferencia verificada = los que no figuran en
#     acuerdos_socios_8708.csv. Pueden tener alguna preferencia no relevada
#     (GSTP, ZLECAF, otros miembros de PAFTA): la recaudación es, por eso, una
#     cota superior para ese grupo.
#   - Turquía paga 0% salvo en 5 líneas (36%): central = 36% x (líneas con 36% /
#     líneas del HS6); mínimo = 0; máximo = 36% si el HS6 tiene alguna línea con 36%.
#   - La recaudación calculada es POTENCIAL (valores CIF de WITS x tasa): supone
#     cumplimiento pleno y no descuenta regímenes suspensivos ni zonas francas
#     (no relevados en estas bases).
#   - Alcance: HS 8708 (parte de la CUCI 784). El script imprime qué parte de la
#     CUCI 784 de 2023 representa.
#   - La pérdida de eficiencia (b + d) se CALIBRA con elasticidades de importación de
#     Marruecos por HS6 (wiiw; ver bloque 3B). NO se cuantifica ΔEP (área a): falta
#     producción doméstica con fuente.
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

# Guardado de gráficos con control (si el PNG está abierto en otro programa, avisa y prueba un plan B)
guardar_png <- function(grafico, nombre, ancho, alto) {
  ruta <- file.path(ruta_graficos, nombre)
  if (file.exists(ruta)) suppressWarnings(file.remove(ruta))
  intento <- function(...) tryCatch({
    suppressWarnings(ggsave(ruta, grafico, width = ancho, height = alto, dpi = 300, bg = "#FAFAF8", ...))
    file.exists(ruta) && file.size(ruta) > 0
  }, error = function(e) { message("  (error al guardar: ", conditionMessage(e), ")"); FALSE })
  ok <- intento()
  if (!ok) ok <- intento(device = "png")
  cat(if (ok) "Gráfico guardado: " else "ATENCIÓN - NO SE PUDO GUARDAR (¿está abierto en otro programa?): ", normalizePath(ruta, mustWork = FALSE), "\n", sep = "")
  invisible(ok)
}

#===============================================================================#
# BLOQUE 0: PARÁMETROS Y ESTILO
#===============================================================================#
anio_ref      <- 2023
pico_internacional <- 15      # % (umbral de la clase para "picos internacionales")
mult_pico_nacional <- 3       # pico nacional: arancel > 3 veces el promedio simple (clase práctica del 16/10)

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
leer_wits <- function(archivo) {
  b <- read_dta(verificar_archivo(archivo)); names(b) <- tolower(names(b))
  b %>% transmute(
    prod = a_chr(productcode), desc = a_chr(productdescription),
    rep = a_chr(reportername), socio = a_chr(partnername),
    flujo = a_chr(tradeflowname), anio = as.integer(year), valor = as.numeric(tradevaluein1000usd))
}
mUSD <- function(v_miles) v_miles / 1e3     # miles de USD -> millones de USD

#===============================================================================#
# BLOQUE 1: ARANCEL NMF APLICADO 2023, 52 LÍNEAS DE HS 8708
#===============================================================================#
lineas <- read_csv(verificar_archivo("spark_8708_nmf_2023_lineas.csv"),
                   col_types = cols(.default = "c"), show_col_types = FALSE) %>%
  transmute(linea, hs6 = as.character(hs6), nmf = as.numeric(nmf_pct), pref, url)

stopifnot(nrow(lineas) == 52, !anyNA(lineas$nmf), all(startsWith(lineas$linea, "8708")),
          !anyDuplicated(lineas$linea))

lineas <- lineas %>% mutate(turquia_36 = str_detect(pref, fixed("36.0%")))
stopifnot(sum(lineas$turquia_36) == 5)     # coincide con la fuente: 36% en 5 de 52 líneas

# Indicadores de la clase (sobre las 52 líneas 8708; no sobre todo el arancel)
perfil <- tibble(
  n_lineas              = nrow(lineas),
  promedio_simple       = mean(lineas$nmf),
  desvio_estandar       = sd(lineas$nmf),
  coef_variacion        = sd(lineas$nmf) / mean(lineas$nmf),
  minimo                = min(lineas$nmf),
  maximo                = max(lineas$nmf),
  pct_lineas_libres     = 100 * mean(lineas$nmf == 0),
  pct_picos_internac    = 100 * mean(lineas$nmf > pico_internacional),
  umbral_pico_nacional  = mult_pico_nacional * mean(lineas$nmf),
  pct_picos_nacional    = 100 * mean(lineas$nmf > mult_pico_nacional * mean(lineas$nmf)))
print(perfil, width = Inf)
write_csv(perfil, paste0(ruta_tablas, "perfil_arancelario_8708_", anio_ref, ".csv"))

lineas_por_tasa <- lineas %>% count(nmf, name = "n_lineas") %>% arrange(nmf) %>%
  mutate(pct_lineas = 100 * n_lineas / sum(n_lineas), pico_internac = nmf > pico_internacional,
         pico_nacional = nmf > perfil$umbral_pico_nacional)
print(lineas_por_tasa)
write_csv(lineas_por_tasa, paste0(ruta_tablas, "lineas_por_tasa_8708_", anio_ref, ".csv"))

# Resumen por HS6 (para cruzar con las importaciones, que llegan hasta HS6)
tarifas_hs6 <- lineas %>% group_by(hs6) %>%
  summarise(n_lineas = n(), t_min = min(nmf), t_medio = mean(nmf), t_max = max(nmf),
            n_lineas_36 = sum(turquia_36), .groups = "drop")
print(tarifas_hs6)

#===============================================================================#
# BLOQUE 2: IMPORTACIONES 2023 POR HS6 Y SOCIO, Y RÉGIMEN ARANCELARIO
#===============================================================================#
imp_todo <- leer_wits("TP6_MAR_M_SOCIOS_AUTOPARTES_HS17.dta") %>%
  filter(flujo == "Import", anio == anio_ref, socio != "World")

imp <- imp_todo %>% filter(startsWith(prod, "8708"))
faltan <- setdiff(unique(imp$prod), tarifas_hs6$hs6)
if (length(faltan) > 0) stop("HS6 de importaciones sin línea arancelaria en el CSV de tarifas: ", paste(faltan, collapse = ", "))

# Alcance: qué parte de la CUCI 784 representa HS 8708
tot_784 <- leer_wits("MAR_ALLPARTNERS_WLD.dta") %>%
  filter(socio == "World", anio == anio_ref, prod == "784", flujo == "Import") %>% pull(valor)
cat("\nImportaciones HS 8708 (", anio_ref, "): ", number(mUSD(sum(imp$valor)), accuracy = 0.1), " M USD = ",
    number(100 * sum(imp$valor) / tot_784, accuracy = 0.1), "% de la CUCI 784 (",
    number(mUSD(tot_784), accuracy = 0.1), " M USD)\n", sep = "")

# Socios con preferencia (archivo con fuente)
acuerdos <- read_csv(verificar_archivo("acuerdos_socios_8708.csv"), show_col_types = FALSE) %>%
  select(socio = socio_wits, grupo)
stopifnot(!anyDuplicated(acuerdos$socio))

imp <- imp %>%
  left_join(acuerdos, by = "socio") %>%
  mutate(regimen = case_when(
    socio == "Unspecified" ~ "Origen no especificado",
    grupo == "UE"          ~ "UE (0%)",
    grupo == "Turquia"     ~ "Turquía (0%; 36% en 5 líneas)",
    !is.na(grupo)          ~ "Otros acuerdos (0%)",
    TRUE                   ~ "Sin preferencia verificada (NMF)")) %>%
  left_join(tarifas_hs6, by = c("prod" = "hs6"))

# Tasa aplicable (%) por fila: central / mínima / máxima
imp <- imp %>% mutate(
  t_c = case_when(regimen == "Sin preferencia verificada (NMF)" ~ t_medio,
                  regimen == "Turquía (0%; 36% en 5 líneas)"    ~ 36 * n_lineas_36 / n_lineas,
                  regimen == "Origen no especificado"           ~ NA_real_,
                  TRUE ~ 0),
  t_lo = case_when(regimen == "Sin preferencia verificada (NMF)" ~ t_min,
                   regimen == "Origen no especificado"           ~ NA_real_,
                   TRUE ~ 0),
  t_hi = case_when(regimen == "Sin preferencia verificada (NMF)" ~ t_max,
                   regimen == "Turquía (0%; 36% en 5 líneas)"    ~ if_else(n_lineas_36 > 0, 36, 0),
                   regimen == "Origen no especificado"           ~ NA_real_,
                   TRUE ~ 0))

# Origen no especificado: se informa aparte y queda FUERA de los totales de recaudación
esp <- imp %>% filter(regimen == "Origen no especificado")
cat("Origen no especificado: ", number(mUSD(sum(esp$valor)), accuracy = 0.1), " M USD (",
    number(100 * sum(esp$valor) / sum(imp$valor), accuracy = 0.01), "% de HS 8708); excluido de la recaudación.\n", sep = "")

imp_c <- imp %>% filter(regimen != "Origen no especificado")

origen_regimen <- imp %>% group_by(regimen) %>%
  summarise(valor_M = mUSD(sum(valor)),
            rec_central_M = mUSD(sum(valor * t_c / 100)),
            rec_min_M     = mUSD(sum(valor * t_lo / 100)),
            rec_max_M     = mUSD(sum(valor * t_hi / 100)), .groups = "drop") %>%
  mutate(peso = 100 * valor_M / sum(valor_M),
         tasa_efectiva_central = 100 * rec_central_M / valor_M) %>%
  arrange(desc(valor_M))
print(origen_regimen)
write_csv(origen_regimen, paste0(ruta_tablas, "origen_regimen_8708_", anio_ref, ".csv"))

# Principales socios del grupo sin preferencia verificada (para revisar si alguno podría tener preferencia no relevada)
socios_nmf <- imp %>% filter(regimen == "Sin preferencia verificada (NMF)") %>%
  group_by(socio) %>% summarise(valor_M = mUSD(sum(valor)), .groups = "drop") %>%
  arrange(desc(valor_M)) %>% mutate(peso_en_grupo = 100 * valor_M / sum(valor_M))
print(head(socios_nmf, 12))
write_csv(socios_nmf, paste0(ruta_tablas, "socios_sin_preferencia_8708_", anio_ref, ".csv"))

#===============================================================================#
# BLOQUE 3: RECAUDACIÓN t x M (ÁREA c) Y COTA SUPERIOR SIN PREFERENCIAS
#===============================================================================#
M_tot <- sum(imp_c$valor)                       # sin origen no especificado
M_nmf <- sum(imp_c$valor[imp_c$regimen == "Sin preferencia verificada (NMF)"])

# (A) Régimen aplicable: cada origen paga su tasa (0% con preferencia)
rec_A <- c(central = sum(imp_c$valor * imp_c$t_c / 100),
           minimo  = sum(imp_c$valor * imp_c$t_lo / 100),
           maximo  = sum(imp_c$valor * imp_c$t_hi / 100))
# (B) Cota superior de manual: toda la importación paga NMF (sin preferencias)
rec_B <- c(central = sum(imp_c$valor * imp_c$t_medio / 100),
           minimo  = sum(imp_c$valor * imp_c$t_min / 100),
           maximo  = sum(imp_c$valor * imp_c$t_max / 100))

# Arancel NMF ponderado por importaciones (indicador de la clase; sesgo a la baja)
t_pond <- c(central = unname(100 * rec_B["central"] / M_tot),
            minimo  = unname(100 * rec_B["minimo"]  / M_tot),
            maximo  = unname(100 * rec_B["maximo"]  / M_tot))

rec_tab <- tibble(
  escenario = c("A. Régimen aplicable (con preferencias)", "B. Cota superior: todo paga NMF"),
  base_M_USD = mUSD(c(M_tot, M_tot)),
  recaudacion_central_M = mUSD(c(rec_A["central"], rec_B["central"])),
  recaudacion_min_M     = mUSD(c(rec_A["minimo"],  rec_B["minimo"])),
  recaudacion_max_M     = mUSD(c(rec_A["maximo"],  rec_B["maximo"]))) %>%
  mutate(tasa_efectiva_central = 100 * recaudacion_central_M / base_M_USD)
print(rec_tab)
write_csv(rec_tab, paste0(ruta_tablas, "recaudacion_tM_8708_", anio_ref, ".csv"))

cat("\nArancel NMF ponderado por importaciones (HS 8708): ", number(t_pond["central"], accuracy = 0.1),
    "% (rango ", number(t_pond["minimo"], accuracy = 0.1), "-", number(t_pond["maximo"], accuracy = 0.1), "%)\n", sep = "")
cat("Importaciones que pagan NMF (sin preferencia verificada): ", number(mUSD(M_nmf), accuracy = 0.1),
    " M USD = ", number(100 * M_nmf / M_tot, accuracy = 0.1), "% de HS 8708\n", sep = "")

#===============================================================================#
# BLOQUE 3B: CALIBRACIÓN DE LA PÉRDIDA DE EFICIENCIA (b + d) CON ELASTICIDADES wiiw
#===============================================================================#
# Fuente: wiiw Import Demand Elasticities (Ghodsi, Grübler y Stehrer, 2016, wiiw WP 132; CC BY 4.0),
# elasticidades de demanda de importaciones de Marruecos por HS6, estimadas con datos 1996-2014 y
# nomenclatura previa a HS2007. Concordancia HS2017 -> nomenclatura wiiw: concordancia_hs8708_wiiw.csv (WCO).
# Método (país pequeño, demanda de elasticidad constante por HS6 y origen gravado):
#   M1 = importación observada que paga arancel t (valor CIF, sin arancel); A = M1 (1+t)^eps
#   pérdida de EC en ese flujo = A [(1+t)^(1-eps) - 1] / (1-eps)   (si eps = 1: M1 (1+t) ln(1+t))
#   pérdida de eficiencia b + d = pérdida de EC - t M1
# Es la elasticidad de las importaciones TOTALES del HS6: subestima la sustitución entre orígenes (desvío
# hacia la UE, que entra con 0%), por eso b + d calibrado es una cota inferior. No separa a (productores)
# de consumidores: falta producción doméstica.
el_w <- read_dta(verificar_archivo("Elasticities_Data.dta")) %>%
  transmute(iso3 = as.character(iso3), hs_w = as.character(product), eps_hat = -as.numeric(elasticity),
            eps_bind = -as.numeric(elasticity_sign), se = as.numeric(se), p = as.numeric(p)) %>%
  filter(iso3 == "MAR", startsWith(hs_w, "8708"))
conc <- read_csv(verificar_archivo("concordancia_hs8708_wiiw.csv"), col_types = cols(.default = "c"), show_col_types = FALSE)
el_hs6 <- conc %>% left_join(el_w, by = c("hs6_base_wiiw" = "hs_w"))
if (anyNA(el_hs6$eps_hat)) stop("Concordancia sin elasticidad wiiw: ", paste(el_hs6$hs6_base_wiiw[is.na(el_hs6$eps_hat)], collapse = ", "))
el_hs6 <- el_hs6 %>% group_by(hs6 = hs6_2017) %>%
  summarise(eps_c = mean(eps_hat), eps_lo = mean(pmax(eps_hat - 1.645 * se, 0)), eps_hi = mean(eps_hat + 1.645 * se),
            significativa = all(!is.na(eps_bind)), codigos_wiiw = paste(hs6_base_wiiw, collapse = "+"), .groups = "drop")
faltan_el <- setdiff(unique(imp_c$prod), el_hs6$hs6)
if (length(faltan_el) > 0) stop("HS6 importados sin elasticidad: ", paste(faltan_el, collapse = ", "))
print(el_hs6, n = Inf)
write_csv(el_hs6, paste0(ruta_tablas, "elasticidades_wiiw_8708_marruecos.csv"))

perdida_ec <- function(M1, t, eps) {
  ifelse(t <= 0 | M1 <= 0, 0,
         ifelse(abs(1 - eps) < 1e-6, M1 * (1 + t) * log(1 + t),
                M1 * (1 + t)^eps * ((1 + t)^(1 - eps) - 1) / (1 - eps)))
}
dw <- imp_c %>% left_join(el_hs6, by = c("prod" = "hs6")) %>%
  mutate(across(c(t_c, t_lo, t_hi, t_medio, t_min, t_max), ~ .x / 100),
         # A. régimen aplicable (con preferencias)
         dwl_A_c  = perdida_ec(valor, t_c,  eps_c)  - t_c  * valor,
         dwl_A_lo = perdida_ec(valor, t_lo, eps_lo) - t_lo * valor,
         dwl_A_hi = perdida_ec(valor, t_hi, eps_hi) - t_hi * valor,
         # B. cota superior: todo paga NMF
         dwl_B_c  = perdida_ec(valor, t_medio, eps_c)  - t_medio * valor,
         dwl_B_lo = perdida_ec(valor, t_min,   eps_lo) - t_min   * valor,
         dwl_B_hi = perdida_ec(valor, t_max,   eps_hi) - t_max   * valor)
calib_784 <- tibble(
  escenario = c("A. Régimen aplicable (con preferencias)", "B. Cota superior: todo paga NMF"),
  recaudacion_c_M = rec_tab$recaudacion_central_M,
  perdida_eficiencia_M = mUSD(c(sum(dw$dwl_A_c), sum(dw$dwl_B_c))),
  perdida_eficiencia_min_M = mUSD(c(sum(dw$dwl_A_lo), sum(dw$dwl_B_lo))),
  perdida_eficiencia_max_M = mUSD(c(sum(dw$dwl_A_hi), sum(dw$dwl_B_hi)))) %>%
  mutate(perdida_EC_sin_a_M = recaudacion_c_M + perdida_eficiencia_M)
cat("\nCalibración de b + d (millones de USD, ", anio_ref, "; elasticidades wiiw de Marruecos por HS6):\n", sep = ""); print(calib_784, width = 200)
peso_nosig <- 100 * sum(dw$valor[!dw$significativa]) / sum(dw$valor)
cat("Importaciones en HS6 con elasticidad no significativa al 10% (se usa la estimación puntual): ", number(peso_nosig, accuracy = 0.1), "% del valor\n", sep = "")
write_csv(calib_784, paste0(ruta_tablas, "calibracion_bienestar_8708_", anio_ref, ".csv"))

#===============================================================================#
# BLOQUE 4: ARANCEL CONSOLIDADO (BOUND) DE LAS LÍNEAS 8708
#===============================================================================#
# La lista consolidada es de 2002 y su nomenclatura no coincide línea a línea con
# la de 2023: se compara la DISTRIBUCIÓN, no cada línea. Se usan solo las filas de
# 10 dígitos para no contar dos veces los niveles de 8 y 10 dígitos.
cons <- read_dta(verificar_archivo("TP6_MAR_ARANCEL_CONSOLIDADO_OMC.dta")); names(cons) <- tolower(names(cons))
cons8708 <- cons %>%
  mutate(tl_chr = sprintf("%.0f", as.numeric(tl))) %>%
  filter(startsWith(tl_chr, "8708"), nchar(tl_chr) == 10) %>%
  transmute(linea_2002 = tl_chr, consolidado = as.numeric(bounddutyav))
stopifnot(nrow(cons8708) > 0, !anyNA(cons8708$consolidado))

consolidado_res <- cons8708 %>% count(consolidado, name = "n_lineas") %>% arrange(consolidado) %>%
  mutate(pct_lineas = 100 * n_lineas / sum(n_lineas))
print(consolidado_res)
write_csv(consolidado_res, paste0(ruta_tablas, "consolidado_8708_distribucion.csv"))
cat("\nConsolidado 8708 (lista 2002, filas de 10 dígitos): mínimo ", min(cons8708$consolidado), "%, máximo ",
    max(cons8708$consolidado), "%, promedio simple ", number(mean(cons8708$consolidado), accuracy = 0.1),
    "% | Aplicado 2023: mínimo ", number(min(lineas$nmf), accuracy = 0.1), "%, máximo ", number(max(lineas$nmf), accuracy = 1), "%, promedio simple ",
    number(mean(lineas$nmf), accuracy = 0.1), "%\n", sep = "")

#===============================================================================#
# BLOQUE 5: GRÁFICOS
#===============================================================================#
pie_fuente <- "Fuente: OMC (TTD) vía spark_8708_nmf_2023_lineas.csv; WITS (TP6_MAR_M_SOCIOS_AUTOPARTES_HS17); acuerdos_socios_8708.csv."

# G1: estructura del arancel NMF aplicado (52 líneas)
g1_d <- lineas_por_tasa %>% mutate(etq = factor(paste0(number(nmf, accuracy = 0.1), "%"),
                                                levels = paste0(number(nmf, accuracy = 0.1), "%")))
g1 <- ggplot(g1_d, aes(etq, n_lineas, fill = pico_internac)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(n_lineas, " líneas (", number(pct_lineas, accuracy = 1), "%)", if_else(pico_nacional, "\npico nacional", ""))),
            lineheight = 0.95,
            vjust = -0.5, family = "rajdhani", fontface = "bold", size = 4.2, color = color_texto_tp2) +
  scale_fill_manual(values = c("TRUE" = color_rojo, "FALSE" = color_gris_dato), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = env_wrap(paste0("Autopartes (HS 8708): arancel NMF aplicado por Marruecos, ", anio_ref), 55),
       subtitle = env_wrap(paste0("52 líneas de 10 dígitos. Promedio simple ", number(perfil$promedio_simple, accuracy = 0.1),
                                  "%, máximo ", number(perfil$maximo, accuracy = 1), "%, ", number(perfil$pct_picos_internac, accuracy = 1),
                                  "% de las líneas por encima de ", pico_internacional, "% (picos internacionales, en rojo) y ", number(perfil$pct_picos_nacional, accuracy = 1),
                                  "% por encima de ", mult_pico_nacional, " veces el promedio (", number(perfil$umbral_pico_nacional, accuracy = 0.1),
                                  "%, picos nacionales). Ponderado por importaciones: ", number(t_pond["central"], accuracy = 0.1), "%, ", if_else(t_pond["central"] > perfil$promedio_simple, "mayor", "menor"), " que el simple."), 90),
       x = "Arancel NMF aplicado (ad valorem)", y = "Número de líneas arancelarias",
       caption = env_wrap(paste0(pie_fuente, " Las 52 líneas tienen tasa ad valorem en la fuente (no hay específicos ni mixtos que convertir). Ponderado: promedio simple de las líneas dentro de cada HS6, ponderado por importaciones HS6; los aranceles prohibitivos pesan poco, así que el ponderado tiende a subestimar la protección."), 125)) +
  theme_tp2() + theme(panel.grid.major.x = element_blank())
g1
guardar_png(g1, "g_autopartes_arancel_estructura.png", 10, 6.5)

# G2: origen de las importaciones por régimen
orden_reg <- c("UE (0%)", "Otros acuerdos (0%)", "Turquía (0%; 36% en 5 líneas)",
               "Sin preferencia verificada (NMF)", "Origen no especificado")
g2_d <- origen_regimen %>% filter(regimen != "Origen no especificado") %>% mutate(regimen = factor(regimen, levels = rev(orden_reg)),
                                  tipo = if_else(regimen == "Sin preferencia verificada (NMF)", "NMF", "otro"))
g2 <- ggplot(g2_d, aes(valor_M, regimen, fill = tipo)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(number(valor_M, accuracy = 1), " M USD (", number(peso, accuracy = 0.1), "%)")),
            hjust = -0.05, family = "rajdhani", fontface = "bold", size = 4.2, color = color_texto_tp2) +
  scale_fill_manual(values = c("NMF" = color_rojo, "otro" = color_gris_dato), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35)), labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = env_wrap(paste0("Autopartes (HS 8708): alrededor de ", number(100 * M_nmf / M_tot, accuracy = 1), "% de las importaciones paga el arancel NMF"), 55),
       subtitle = env_wrap(paste0("Importaciones de Marruecos de HS 8708 por régimen, ", anio_ref,
                                  ". El resto entra con preferencia (0%), salvo 5 líneas para Turquía."), 90),
       x = "Millones de USD (valor CIF, WITS)", y = NULL,
       caption = env_wrap(paste0(pie_fuente, " 'Sin preferencia verificada' = socios que no figuran en acuerdos_socios_8708.csv; pueden tener preferencias no relevadas (GSTP, ZLECAF, otros miembros de PAFTA). No se muestra el origen no especificado (",
                                 number(mUSD(sum(esp$valor)), accuracy = 0.1), " M USD)."), 125)) +
  theme_tp2() + theme(panel.grid.major.y = element_blank())
g2
guardar_png(g2, "g_autopartes_origen_regimen.png", 11, 6)

# G3: recaudación potencial t x M (rango)
g3_d <- rec_tab %>% mutate(escenario = factor(escenario, levels = rev(escenario)))
g3 <- ggplot(g3_d, aes(recaudacion_central_M, escenario)) +
  geom_col(aes(fill = escenario), width = 0.55, show.legend = FALSE) +
  geom_linerange(aes(xmin = recaudacion_min_M, xmax = recaudacion_max_M), linewidth = 1.6, color = color_texto_tp2) +
  geom_text(aes(x = 0,
                label = paste0("central ", number(recaudacion_central_M, accuracy = 1), " M USD (rango ",
                               number(recaudacion_min_M, accuracy = 1), "-", number(recaudacion_max_M, accuracy = 1), ")")),
            hjust = 0, nudge_y = 0.42, family = "rajdhani", fontface = "bold", size = 4.2, color = color_texto_tp2) +
  scale_fill_manual(values = c(setNames(c(color_rojo, color_gris_dato), levels(g3_d$escenario)[c(2, 1)]))) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08)), labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = env_wrap("Autopartes (HS 8708): recaudación potencial del arancel (área c = t x M)", 55),
       subtitle = env_wrap(paste0("Base: ", number(mUSD(M_tot), accuracy = 1), " M USD importados en ", anio_ref,
                                  ". Con las preferencias vigentes la tasa efectiva es ", number(rec_tab$tasa_efectiva_central[1], accuracy = 0.1),
                                  "% (central); sin preferencias sería ", number(rec_tab$tasa_efectiva_central[2], accuracy = 0.1), "%."), 90),
       x = "Millones de USD", y = NULL,
       caption = env_wrap(paste0(pie_fuente, " Barra = recaudación con el promedio simple de las líneas de cada HS6. Línea negra = rango si en cada HS6 rigiera la tasa de su línea mínima o máxima (no es un intervalo de confianza). Recaudación potencial: supone cumplimiento pleno y no descuenta regímenes suspensivos ni zonas francas."), 125)) +
  theme_tp2() + theme(panel.grid.major.y = element_blank())
g3
guardar_png(g3, "g_autopartes_recaudacion_tM.png", 11, 5.5)

# G4: esquema de bienestar, arancel en país pequeño (SIN ESCALA)
# Curvas lineales ilustrativas: S(q) = 20 + 0.8 q ; D(q) = 100 - 0.8 q ; Pw = 40 ; Pw + t = 55
Pw <- 40; Pt <- 55
S_p <- function(q) 20 + 0.8 * q
D_p <- function(q) 100 - 0.8 * q
q_de_S <- function(p) (p - 20) / 0.8
q_de_D <- function(p) (100 - p) / 0.8
S1 <- q_de_S(Pw); S2 <- q_de_S(Pt); D2 <- q_de_D(Pt); D1 <- q_de_D(Pw)
M1 <- D1 - S1; M2 <- D2 - S2
M_p <- function(m) 60 - 0.4 * m          # demanda de importaciones: D(p) - S(p)

paneles <- c("(a) Mercado interno (precio y cantidad)", "(b) Mercado de importaciones (precio y cantidad)")
curvas <- bind_rows(
  tibble(panel = paneles[1], curva = "S", x = c(0, 100), y = S_p(c(0, 100))),
  tibble(panel = paneles[1], curva = "D", x = c(0, 100), y = D_p(c(0, 100))),
  tibble(panel = paneles[2], curva = "M", x = c(0, 60), y = M_p(c(0, 60))))
precios <- bind_rows(
  tibble(panel = paneles[1], p = c(Pw, Pt), tipo = c("Pw", "Pw + t")),
  tibble(panel = paneles[2], p = c(Pw, Pt), tipo = c("Pw", "Pw + t")))
poly <- bind_rows(
  tibble(panel = paneles[1], area = "a", x = c(0, S1, S2, 0),   y = c(Pw, Pw, Pt, Pt)),
  tibble(panel = paneles[1], area = "b", x = c(S1, S2, S2),     y = c(Pw, Pt, Pw)),
  tibble(panel = paneles[1], area = "c", x = c(S2, D2, D2, S2), y = c(Pw, Pw, Pt, Pt)),
  tibble(panel = paneles[1], area = "d", x = c(D2, D1, D2),     y = c(Pw, Pw, Pt)),
  tibble(panel = paneles[2], area = "c", x = c(0, M2, M2, 0),   y = c(Pw, Pw, Pt, Pt)),
  tibble(panel = paneles[2], area = "b + d", x = c(M2, M1, M2), y = c(Pt, Pw, Pw)))
etq_area <- tribble(
  ~panel,       ~area,   ~x,                ~y,
  paneles[1],   "a",     S2 / 2.6,          (Pw + Pt) / 2,
  paneles[1],   "b",     S1 + (S2 - S1) * 0.38, Pw + (Pt - Pw) * 0.22,
  paneles[1],   "c",     (S2 + D2) / 2,     (Pw + Pt) / 2,
  paneles[1],   "d",     D2 + (D1 - D2) * 0.30, Pw + (Pt - Pw) * 0.22,
  paneles[2],   "c",     M2 / 2,            (Pw + Pt) / 2,
  paneles[2],   "b + d", M2 + (M1 - M2) * 0.30, Pw + (Pt - Pw) * 0.25)
etq_x <- tribble(
  ~panel,      ~x,  ~lab,
  paneles[1],  S1,  "S1", paneles[1], S2, "S2", paneles[1], D2, "D2", paneles[1], D1, "D1",
  paneles[2],  M2,  "M2", paneles[2], M1, "M1")
etq_curva <- tribble(
  ~panel,      ~x,   ~y,           ~lab,
  paneles[1],  100,  S_p(100),     "S",
  paneles[1],  100,  D_p(100),     "D",
  paneles[2],  60,   M_p(60),      "M")

g4 <- ggplot() +
  geom_polygon(data = poly, aes(x, y, fill = area), alpha = 0.55, color = NA) +
  geom_line(data = curvas, aes(x, y, group = curva), linewidth = 1.2, color = color_texto_tp2) +
  geom_hline(data = precios, aes(yintercept = p), linetype = "dashed", color = color_texto_sec) +
  geom_text(data = precios, aes(x = -Inf, y = p, label = paste0(tipo, "  ")), hjust = 1, vjust = 0.5, family = "rajdhani",
            fontface = "bold", size = 4.2, color = color_texto_tp2) +
  geom_text(data = etq_area, aes(x, y, label = area), family = "rajdhani", fontface = "bold", size = 6, color = color_texto_tp2) +
  geom_text(data = etq_x, aes(x, y = 12, label = lab), family = "rajdhani", fontface = "bold", size = 4.2, color = color_texto_tp2) +
  geom_text(data = etq_curva, aes(x, y, label = lab), hjust = -0.6, family = "rajdhani", fontface = "bold", size = 5, color = color_texto_tp2) +
  facet_wrap(~panel, scales = "free_x") +
  scale_fill_manual(values = c("a" = color_verde, "b" = color_rojo, "c" = "#D9A441", "d" = color_rojo, "b + d" = color_rojo), guide = "none") +
  coord_cartesian(ylim = c(10, 100), clip = "off") +
  labs(title = env_wrap("País pequeño: efectos del arancel sobre las importaciones de autopartes", 60),
       subtitle = env_wrap(paste0("Variación del EC = -(a + b + c + d)   |   Variación del EP = +a   |   Recaudación = c = t x M (",
                                  number(rec_tab$recaudacion_central_M[1], accuracy = 1), " M USD potenciales con las preferencias vigentes; rango ",
                                  number(rec_tab$recaudacion_min_M[1], accuracy = 1), "-", number(rec_tab$recaudacion_max_M[1], accuracy = 1),
                                  ")   |   Pérdida de eficiencia = -(b + d), calibrada en ", number(calib_784$perdida_eficiencia_M[1], accuracy = 0.1), " M USD (rango ",
                                  number(calib_784$perdida_eficiencia_min_M[1], accuracy = 0.1), "-", number(calib_784$perdida_eficiencia_max_M[1], accuracy = 0.1),
                                  ")   |   Sin efecto sobre los términos de intercambio."), 100),
       x = "Cantidad", y = NULL,
       caption = env_wrap(paste0("Esquema sin escala (curvas lineales ilustrativas). El área c se calcula con datos (t x M). ",
                                 "b + d se calibra con elasticidades de importación de Marruecos por HS6 (wiiw, Ghodsi, Grübler y Stehrer 2016; datos 1996-2014): cota inferior, porque no capta el desvío entre orígenes. ",
                                 "El área a no se estima: falta producción doméstica. ",
                                 "En el esquema el arancel se aplica a todas las importaciones; en los datos solo el ",
                                 number(100 * M_nmf / M_tot, accuracy = 1), "% de HS 8708 paga NMF (el resto entra con preferencia, ver gráfico de origen por régimen)."), 130)) +
  theme_tp2() +
  theme(axis.text = element_blank(), panel.grid.major = element_blank(), axis.line = element_line(color = color_texto_tp2),
        panel.spacing = unit(3, "lines"), plot.margin = margin(18, 34, 14, 50))
g4
guardar_png(g4, "g_autopartes_bienestar_esquema.png", 12, 6.5)

#===============================================================================#
# BLOQUE 6: RESUMEN PARA LA DIAPOSITIVA (b) y (c) DE AUTOPARTES
#===============================================================================#
resumen_784 <- tibble(
  indicador = c("Líneas HS 8708 con arancel NMF 2023", "NMF promedio simple (%)", "NMF máximo (%)",
                "Líneas con arancel > 15% (%)", "Líneas por encima de 3 veces el promedio (picos nacionales, %)", "NMF ponderado por importaciones, central (%)",
                "Consolidado 8708 (lista 2002): mínimo-máximo (%)",
                "Importaciones HS 8708 (M USD)", "Importaciones que pagan NMF (% de HS 8708)",
                "Recaudación potencial con preferencias, central (M USD)",
                "Recaudación potencial con preferencias, rango (M USD)",
                "Recaudación potencial sin preferencias, central (M USD)",
                "Pérdida de eficiencia b + d calibrada, con preferencias (M USD)",
                "Pérdida de eficiencia b + d calibrada, sin preferencias (M USD)"),
  valor = c(perfil$n_lineas, perfil$promedio_simple, perfil$maximo, perfil$pct_picos_internac, perfil$pct_picos_nacional, t_pond["central"],
            NA_real_, mUSD(M_tot), 100 * M_nmf / M_tot, rec_tab$recaudacion_central_M[1], NA_real_, rec_tab$recaudacion_central_M[2],
            calib_784$perdida_eficiencia_M[1], calib_784$perdida_eficiencia_M[2]),
  detalle = c("", "", "", "", paste0("umbral ", number(perfil$umbral_pico_nacional, accuracy = 0.1), "%"), paste0("rango ", number(t_pond["minimo"], accuracy = 0.1), "-", number(t_pond["maximo"], accuracy = 0.1)),
              paste0(min(cons8708$consolidado), "-", max(cons8708$consolidado)), "sin origen no especificado", "",
              "", paste0(number(rec_tab$recaudacion_min_M[1], accuracy = 0.1), "-", number(rec_tab$recaudacion_max_M[1], accuracy = 0.1)), "",
              paste0("rango ", number(calib_784$perdida_eficiencia_min_M[1], accuracy = 0.1), "-", number(calib_784$perdida_eficiencia_max_M[1], accuracy = 0.1), "; wiiw"),
              paste0("rango ", number(calib_784$perdida_eficiencia_min_M[2], accuracy = 0.1), "-", number(calib_784$perdida_eficiencia_max_M[2], accuracy = 0.1), "; wiiw")))
print(resumen_784, n = Inf)
write_csv(resumen_784, paste0(ruta_tablas, "resumen_784_b_c_", anio_ref, ".csv"))

#===============================================================================#
# FIN DEL SCRIPT 03. Salidas: tablas en output/tablas/03/, gráficos en output/graficos/03/.
#===============================================================================#
