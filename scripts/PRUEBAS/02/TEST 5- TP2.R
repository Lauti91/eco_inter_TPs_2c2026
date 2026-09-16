#===============================================================================#
# TP2 - Economía Internacional
# Marruecos: Modelo de Factores Específicos (MFE) y Heckscher-Ohlin (HO)
#===============================================================================#
# Arranca corriendo el script final del TP1 completo (o cargando sus
# objetos livianos si ya se guardaron), para heredar bases limpias,
# VCR/VCRN, IIC por socio, tema visual y paletas.
#
# CAMBIO CENTRAL DE ESTA VERSIÓN: todas las cifras de OCP, USGS, OICA,
# Office des Changes, HCP, CNSS, UNCTADstat, AMDIE y Caisse de
# Compensation se leen directamente del Excel que compiló Gemini (agente
# Gemini Spark) en dos rondas de investigación web dirigida - BLOQUE 0.5.
# Ya no hay tibble(...) con números tipeados a mano en ningún punto del
# script: si un número cambia, se corrige en la fuente (el Excel) y el
# script entero se recalcula solo.
#
# METODOLOGÍA DE RELEVAMIENTO (para citar en la presentación si hace
# falta): la búsqueda y extracción de cada dato fue ejecutada por un
# agente de IA (Gemini, modelo Gemini Spark) siguiendo instrucciones
# puntuales sobre qué fuentes primarias consultar (OCP, USGS, Office des
# Changes, HCP, CNSS, UNCTADstat, AMDIE, Ministère de l'Industrie) - no
# generada desde el conocimiento propio del modelo. Cada fila del Excel
# trae su fuente primaria, URL exacta y fecha de consulta (ver
# TP2_fuentes_de_datos.md y TP2_seguimiento_investigacion_ronda2.md para
# el detalle completo). Esto no reemplaza una verificación manual de las
# cifras más sensibles antes de citarlas como definitivas.
#
# Sigue sin estar corrido ni validado en R (se escribió sin acceso a R
# para probarlo) - revisar nombres de columnas de pwt10.01 al cargarlo la
# primera vez, y revisar el bloque de parsing de la Sección 0.5 contra el
# Excel real la primera vez que se corre (los "case_when"/str_detect de
# nombres de variable dependen de que el texto exacto de la columna
# Variable no haya cambiado entre lo que Spark generó y lo que quedó en
# el archivo final).

objetos_tp1 <- "output/tablas/01/objetos_heredados_tp1.RData"
if (file.exists(objetos_tp1)) {
  load(objetos_tp1)
} else {
  source("scripts/FINAL/01/01_INDICADORES_FINAL.R", print.eval = FALSE)
}
# print.eval = FALSE evita reintentar mostrar cada gráfico del TP1 en pantalla
# durante el source() (causa típica de "Viewport has zero dimension(s)" si el
# panel de Plots está colapsado). No afecta los ggsave() del TP1.

paquetes_nuevos <- c("WDI", "pwt10", "readxl")
for (pkg in paquetes_nuevos) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(WDI)
library(pwt10)
library(readxl)

ruta_graficos <- "output/graficos/02/"
ruta_tablas   <- "output/tablas/02/"
if (!dir.exists(ruta_graficos)) dir.create(ruta_graficos, recursive = TRUE)
if (!dir.exists(ruta_tablas))   dir.create(ruta_tablas, recursive = TRUE)

paleta_sectores  <- c("Fosfatos" = "#1b4965", "Automotor" = "#c1121f")
paleta_paises_ho <- c("Morocco" = "#c1272d", "Germany" = "#FFCE00",
                      "Spain" = "#AA151B", "France" = "#0055A4")

sectores_mfe <- c("272", "562", "781", "784")
nombres_sectores_mfe <- c(
  "272" = "Fertilizantes crudos", "562" = "Fertilizantes manufacturados",
  "781" = "Autos de pasajeros",   "784" = "Autopartes"
)


#===============================================================================#
# BLOQUE 0.5: CARGA Y PARSEO DE LA BASE DE INVESTIGACIÓN (Gemini Spark)
#===============================================================================#

# AJUSTAR ESTA RUTA si el Excel queda en otra carpeta o con otro nombre -
# es la única línea que hay que tocar si cambia la ubicación del archivo.
ruta_investigacion <- "bases de datos/TP2_Economia_Internacional_Marruecos_Fuentes_de_Datos.xlsx"

leer_hoja_investigacion <- function(ruta, hoja) {
  read_excel(ruta, sheet = hoja) |>
    set_names(c("bloque", "punto", "variable", "valor_raw", "unidad",
                "anio", "fuente", "url", "fecha_consulta", "notas"))
}

# La columna Valor mezcla números limpios, "NO ENCONTRADO", rangos de texto
# ("75% - 85%", "20000 - 25000") y fracciones ya decimales (0.635) para
# magnitudes que en otras filas vienen como porcentaje-texto ("75%") - la
# codificación no fue consistente entre filas, así que el parser cubre las
# tres formas en vez de asumir un único formato.
parsear_valor <- function(valor_raw) {
  x <- str_trim(as.character(valor_raw))
  es_no_encontrado <- str_detect(x, regex("NO ENCONTRADO", ignore_case = TRUE))
  es_rango <- str_detect(x, "-") & !es_no_encontrado & str_detect(x, "\\d")
  es_pct_texto <- str_detect(x, "%")
  
  limpiar_num <- function(s) as.numeric(str_trim(str_remove_all(s, "%")))
  
  valor_min <- rep(NA_real_, length(x))
  valor_max <- rep(NA_real_, length(x))
  partes <- str_split(x, "-")
  for (i in seq_along(x)) {
    if (isTRUE(es_rango[i]) && length(partes[[i]]) == 2) {
      valor_min[i] <- limpiar_num(partes[[i]][1])
      valor_max[i] <- limpiar_num(partes[[i]][2])
    }
  }
  
  valor_unico <- ifelse(es_no_encontrado | es_rango, NA_real_, suppressWarnings(limpiar_num(x)))
  valor_unico[es_pct_texto & !es_rango] <- valor_unico[es_pct_texto & !es_rango] / 100
  valor_min[es_pct_texto & es_rango] <- valor_min[es_pct_texto & es_rango] / 100
  valor_max[es_pct_texto & es_rango] <- valor_max[es_pct_texto & es_rango] / 100
  
  tibble(
    valor = valor_unico,
    valor_min = valor_min,
    valor_max = valor_max,
    valor_medio = ifelse(es_rango, (valor_min + valor_max) / 2, valor_unico)
  )
}

datos_investigacion <- bind_rows(
  leer_hoja_investigacion(ruta_investigacion, "Ronda 1") |> mutate(ronda = 1),
  leer_hoja_investigacion(ruta_investigacion, "Ronda 2 - Profundización") |> mutate(ronda = 2)
) |>
  mutate(anio = as.integer(anio))

datos_investigacion <- bind_cols(datos_investigacion, parsear_valor(datos_investigacion$valor_raw))

# Chequeo de cobertura: filas que no se pudieron convertir a número
# (esperable que sean las "NO ENCONTRADO" y algún texto puramente
# descriptivo, como el régimen fiscal de AMDIE - revisar que no haya
# ninguna otra ahí por un problema de parsing).
datos_investigacion |>
  filter(is.na(valor) & is.na(valor_medio) &
           !str_detect(valor_raw, regex("NO ENCONTRADO", ignore_case = TRUE))) |>
  select(variable, valor_raw, anio)

extraer_serie <- function(nombre_variable, columna = "valor") {
  datos_investigacion |>
    filter(variable == nombre_variable) |>
    select(anio, all_of(columna)) |>
    rename(year = anio) |>
    arrange(year)
}

## --- OCP: financieros, empleo, dividendos ---
ocp_financieros <- extraer_serie("Ingresos (Chiffre d'affaires)") |> rename(ingresos_mmad = valor)
ocp_financieros <- full_join(ocp_financieros,
                             extraer_serie("EBITDA") |> rename(ebitda_mmad = valor), by = "year")
ocp_financieros <- full_join(ocp_financieros,
                             extraer_serie("Gastos de capital (Capex)") |> rename(capex_mmad = valor), by = "year")
ocp_financieros <- full_join(ocp_financieros,
                             extraer_serie("Dividendos pagados al Tesoro marroquí") |> rename(dividendos_mmad = valor), by = "year") |>
  arrange(year)

ocp_empleo_serie <- extraer_serie("Empleo directo OCP Group (Fosfatos)") |> rename(empleo = valor)
# La consolidada global (~20.000) se usa solo como chequeo de robustez en
# 2.4.1, no como serie principal - no está tabulada año a año en el Excel,
# así que queda fija como escalar (viene del texto de la ronda 1: "~32.000
# consolidado global"; el consolidado en Marruecos usado en 2019/2024 es
# ~20.000, que sí está en la serie principal).
ocp_empleo_consolidado_alt <- 20000

## --- USGS: reservas y producción física de roca fosfórica ---
fosfatos_reservas <- extraer_serie("Reservas de roca fosfórica (Marruecos)") |>
  rename(reservas_mt = valor)
# NOTA: en una versión anterior de este script esta cifra se había
# tipeado a mano como 50.000 (miles de toneladas) - el valor real en el
# Excel es 50.000.000, un error de 1000x que quedó expuesto justamente al
# pasar a leer la fuente en vez de escribirla de memoria.

fosfatos_produccion <- extraer_serie("Producción física de roca fosfórica") |>
  rename(produccion_mt = valor)

## --- Automotor: producción física, empleo y exportaciones ---
auto_produccion <- extraer_serie("Producción de vehículos de motor") |>
  rename(unidades = valor)

auto_empleo_serie <- bind_rows(
  datos_investigacion |>
    filter(variable == "Empleo total sector automotor") |>
    transmute(year = anio, empleo = valor, nota = "declarado/observado"),
  datos_investigacion |>
    filter(variable == "Empleo total sector automotor (meta proyectada)") |>
    transmute(year = anio, empleo = valor, nota = "meta proyectada, no observada")
) |>
  arrange(year)

auto_exportaciones_oc <- extraer_serie("Exportaciones sector automotor (Total)") |>
  rename(total_mmad = valor)
auto_exportaciones_oc <- full_join(auto_exportaciones_oc,
                                   extraer_serie("Exportaciones automotrices - Segmento Câblage (Cableado)") |> rename(cableado_mmad = valor),
                                   by = "year")
auto_exportaciones_oc <- full_join(auto_exportaciones_oc,
                                   extraer_serie("Exportaciones automotrices - Segmento Construction (Ensamble)") |> rename(ensamblaje_mmad = valor),
                                   by = "year")
auto_exportaciones_oc <- full_join(auto_exportaciones_oc,
                                   extraer_serie("Exportaciones Phosphates et dérivés (Referencia cruce)") |> rename(fosfatos_ref_mmad = valor),
                                   by = "year") |>
  arrange(year)
# 2023: la declaración ministerial ("Exportaciones sector automotor
# (Declaración ministerial)" = 148.000) queda fuera de total_mmad a
# propósito - se usa 141.763 (balanza, Office des Changes) por ser el dato
# comparable en la misma fuente a lo largo de toda la serie 2019-2025.
# Para consultar la cifra ministerial: extraer_serie("Exportaciones sector
# automotor (Declaración ministerial)").

## --- Coeficientes de reparto del VA por rama (HCP, TRE Base 2014) ---
io_coeficientes <- datos_investigacion |>
  filter(str_detect(variable, "^Coeficiente")) |>
  mutate(
    rama = case_when(
      str_detect(variable, "Minería") ~ "Minería/fosfatos (B00)",
      str_detect(variable, "Automotor") ~ "Automotor/IMME (C00)"
    ),
    tipo = case_when(
      str_detect(variable, "^Coeficiente capital") ~ "capital",
      str_detect(variable, "^Coeficiente trabajo") ~ "trabajo",
      str_detect(variable, "insumo-producto") ~ "ci_produccion"
    )
  ) |>
  select(rama, tipo, valor_min, valor_max) |>
  pivot_wider(names_from = tipo, values_from = c(valor_min, valor_max),
              names_glue = "{tipo}_va_{.value}") |>
  rename_with(~ str_replace(., "valor_", ""), everything()) |>
  mutate(capital_va_medio = (capital_va_min + capital_va_max) / 2,
         trabajo_va_medio = (trabajo_va_min + trabajo_va_max) / 2)
# io_coeficientes queda con las mismas columnas que antes (rama,
# capital_va_min/max, trabajo_va_min/max, ci_produccion_min/max,
# capital_va_medio, trabajo_va_medio) - todo el código de 2.2/2.3/2.4/3.3
# que las usa no necesita cambios.

## --- Salarios: promedio general y por sector (CNSS) ---
salario_promedio_general <- extraer_serie("Salario medio mensual declarado - Total cotizantes régimen general") |>
  rename(salario_mad_mes = valor)

salarios_cnss_sector_2020 <- datos_investigacion |>
  filter(str_starts(variable, "Salario medio declarado -"), anio == 2020) |>
  transmute(
    sector = str_remove(variable, "^Salario medio declarado - "),
    salario_mad_mes = valor
  )
# NOTA: en una versión anterior este objeto tenía 7 sectores tipeados a
# mano (incluía Comercio, Servicios de mercado y Construcción). En el
# Excel real solo hay 4 filas de este tipo (Industrie, Agriculture,
# Transports, Activités financières) - los otros tres habían salido del
# resumen narrativo del informe, no de la planilla de datos. Quedan
# afuera hasta que Spark los releve como filas propias.

## --- IED: stock total, por país de origen, e inversión en el sector automotor ---
ied_stock_total <- extraer_serie("Stock de IED entrante en Marruecos (Inward FDI Stock)") |>
  rename(stock_musd = valor)

ied_origen_pais_2020 <- datos_investigacion |>
  filter(str_starts(variable, "Stock de IED entrante originario de"), anio == 2020) |>
  transmute(
    pais = str_remove(variable, "^Stock de IED entrante originario de "),
    stock_mmad = valor_medio  # valor_medio cubre tanto las filas puntuales
    # (Francia, Emiratos, España) como las que vinieron como rango
    # (Países Bajos, Estados Unidos) sin tener que tratarlas distinto
  )

auto_inversion <- bind_rows(
  datos_investigacion |>
    filter(str_starts(variable, "Inversión")) |>
    transmute(entidad = str_remove(variable, "^Inversión (acumulada|comprometida) "),
              monto_mmad = valor, año_corte = anio),
  datos_investigacion |>
    filter(str_starts(variable, "Inversiones")) |>
    transmute(entidad = str_remove(variable, "^Inversiones comprometidas "),
              monto_mmad = valor, año_corte = anio)
)

ied_manufactura <- extraer_serie("Stock de IED en industria manufacturera") |>
  rename(stock_mmad = valor)
# Solo queda el punto de 2020 (152.000) - el "~170.000 hacia 2024" que
# aparece en las Notas de esa fila es una aproximación mencionada al
# pasar, no una fila propia con su año - no se tabula como dato 2024
# hasta que exista una fila real para ese año.

ied_manufactura_flujo <- datos_investigacion |>
  filter(variable == "Flujo neto anual de IED manufacturera") |>
  select(anio, valor_min, valor_max)

## --- HCP: valor agregado sectorial ---
hcp_va_crecimiento <- extraer_serie("Crecimiento volumen VA - Minería (B00)") |>
  rename(mineria_pct = valor)
hcp_va_crecimiento <- full_join(hcp_va_crecimiento,
                                extraer_serie("Crecimiento volumen VA - Manufactura (C00)") |> rename(manufactura_pct = valor),
                                by = "year") |> arrange(year)

hcp_va_total_mmad <- extraer_serie("Valor agregado total economía (precios básicos)") |>
  rename(total = valor)

## --- Caisse de Compensation y régimen AMDIE (Bloque 4) ---
caisse_compensacion <- bind_rows(
  datos_investigacion |>
    filter(variable == "Carga global de compensación (Caisse de Compensation)") |>
    transmute(year = anio, carga_mmad = valor, nota = NA_character_),
  datos_investigacion |>
    filter(variable == "Carga global de compensación presupuestada") |>
    transmute(year = anio, carga_mmad = valor, nota = "presupuestado, Loi de Finances")
) |>
  arrange(year)

regimen_amdie_texto <- datos_investigacion |>
  filter(variable == "Régimen de zonas francas e incentivos fiscales a la inversión automotriz") |>
  pull(valor_raw)
# Texto descriptivo (no numérico) para usar tal cual en la diapositiva del
# Bloque 4: "Exoneración IS 5 años + 15% posterior; primas hasta 30%".

## --- Precios internacionales (Pink Sheet) ---
precios_internacionales <- extraer_serie("Precio internacional DAP (f.o.b. US Gulf)") |>
  rename(dap_usd_mt = valor)
precios_internacionales <- full_join(precios_internacionales,
                                     extraer_serie("Precio internacional roca fosfórica (f.o.b. North Africa)") |> rename(roca_fosforica_usd_mt = valor),
                                     by = "year")
precios_internacionales <- full_join(precios_internacionales,
                                     extraer_serie("Precio internacional gas natural (Europe TTF)") |> rename(gas_natural_usd_mmbtu = valor),
                                     by = "year") |>
  arrange(year)


#===============================================================================#
# BLOQUE 1: PUNTO DE PARTIDA
#===============================================================================#
# Corresponde al Bloque 1 de la consigna: retomar del TP1 los sectores
# exportadores elegidos e indicar qué exportan y a quiénes.

## --- 1.1 VCRN de fosfatos y automotor, evolución completa 2021-2025 ---
vcrn_mfe <- vcr_mar |>
  filter(cuci %in% sectores_mfe) |>
  mutate(
    sector_desc = nombres_sectores_mfe[cuci],
    grupo = if_else(cuci %in% c("272", "562"), "Fosfatos", "Automotor")
  )

g_vcrn_mfe <- ggplot(vcrn_mfe, aes(x = year, y = vcrn, color = sector_desc, linetype = grupo)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "VCRN de fosfatos vs. automotor, 2021-2025",
       subtitle = "Ventaja comparativa revelada normalizada (Balassa)",
       x = "Año", y = "VCRN", color = "Producto", linetype = "Sector",
       caption = "Fuente: bases WITS del TP1 (vcr_mar).") +
  theme_tp1()
g_vcrn_mfe
ggsave(paste0(ruta_graficos, "grafico_vcrn_mfe.png"), g_vcrn_mfe,
       width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 1.2 Exportaciones (valor) de fosfatos y automotor - WITS 2021-2025 ---
exportaciones_mfe_wits <- mar_exp |>
  filter(p == "WLD", cuci %in% sectores_mfe) |>
  mutate(
    sector_desc = nombres_sectores_mfe[cuci],
    grupo = if_else(cuci %in% c("272", "562"), "Fosfatos", "Automotor")
  ) |>
  select(year, cuci, sector_desc, grupo, value, share)

## --- 1.3 A quiénes exporta: IIC de automotor por socio (heatmap del TP1) ---
iic_automotor <- iic_heatmap_completo |>
  filter(cuci %in% c("781", "784")) |>
  left_join(tibble(socio = names(nombres_socio), socio_nombre = nombres_socio), by = "socio")
iic_automotor


#===============================================================================#
# BLOQUE 2: MODELO DE FACTORES ESPECÍFICOS (corto plazo)
#===============================================================================#

## --- 2.2 Evolución temporal de la productividad media del trabajo ---
calcular_vpmgl_cobb_douglas <- function(produccion, empleo, alpha) {
  base <- inner_join(produccion, empleo, by = "year")
  col_prod <- names(produccion)[2]
  base |>
    mutate(
      producto_medio_trabajo = .data[[col_prod]] / empleo,
      pmgl_relativo = (1 - alpha) * producto_medio_trabajo
    )
}

vpmgl_fosfatos <- calcular_vpmgl_cobb_douglas(
  fosfatos_produccion, ocp_empleo_serie,
  alpha = io_coeficientes$capital_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
)

vpmgl_automotor <- calcular_vpmgl_cobb_douglas(
  auto_produccion |> rename(unidades_producidas = unidades),
  auto_empleo_serie |> select(year, empleo),
  alpha = io_coeficientes$capital_va_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]
)

g_productividad <- bind_rows(
  vpmgl_fosfatos |> mutate(sector = "Fosfatos"),
  vpmgl_automotor |> mutate(sector = "Automotor")
) |>
  ggplot(aes(x = year, y = producto_medio_trabajo, color = sector)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  scale_color_manual(values = paleta_sectores) +
  scale_y_log10() +
  labs(title = "Productividad media del trabajo (proxy de VPMgL)",
       subtitle = "Producción física / empleo, escala log — solo la FORMA de cada curva importa acá",
       x = "Año", y = "Unidades físicas / empleado (log)", color = NULL,
       caption = "Calibrado con coeficientes de reparto del VA (HCP, vía Excel de Gemini) como alpha de un Cobb-Douglas simple.") +
  theme_tp1()
g_productividad
ggsave(paste0(ruta_graficos, "grafico_productividad_mfe.png"), g_productividad,
       width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.3 La FPP real de Marruecos (plano Q_fosfatos x Q_automotor) ---
anio_ref_fpp <- 2023

L_fosfatos_0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref_fpp]
L_auto_0     <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref_fpp]
L_total      <- L_fosfatos_0 + L_auto_0

Q_fosfatos_0 <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio_ref_fpp]
Q_auto_0     <- auto_produccion$unidades[auto_produccion$year == anio_ref_fpp]

alpha_fosfatos <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
alpha_auto     <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]

A_fosfatos <- Q_fosfatos_0 / (L_fosfatos_0 ^ (1 - alpha_fosfatos))
A_auto     <- Q_auto_0     / (L_auto_0     ^ (1 - alpha_auto))

fpp_grilla <- tibble(L_fosfatos = seq(1000, L_total - 1000, length.out = 300)) |>
  mutate(
    L_auto     = L_total - L_fosfatos,
    Q_fosfatos = A_fosfatos * L_fosfatos ^ (1 - alpha_fosfatos),
    Q_auto     = A_auto     * L_auto     ^ (1 - alpha_auto)
  )

fpp_observado <- inner_join(
  fosfatos_produccion |> rename(Q_fosfatos = produccion_mt),
  auto_produccion |> rename(Q_auto = unidades),
  by = "year"
)

g_fpp <- ggplot() +
  geom_path(data = fpp_grilla, aes(x = Q_fosfatos, y = Q_auto), linewidth = 1.1, color = "gray30") +
  geom_point(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, color = factor(year)), size = 3) +
  geom_text(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, label = year), vjust = -1, size = 3) +
  labs(title = "FPP empírica de Marruecos: fosfatos vs. automotor",
       subtitle = paste0("Curva calibrada con Cobb-Douglas sobre L = ", format(L_total, big.mark = "."),
                         " trabajadores (", anio_ref_fpp, "); puntos = producción observada por año"),
       x = "Producción de fosfatos (miles de toneladas)",
       y = "Producción automotriz (unidades)",
       color = "Año observado",
       caption = "Curva calibrada con alpha de io_coeficientes; el punto 2023 coincide con la curva por ser el año de calibración.") +
  theme_tp1()
g_fpp
ggsave(paste0(ruta_graficos, "grafico_fpp_real.png"), g_fpp, width = 10, height = 7, dpi = 300, bg = "white")

anios_fpp_familia <- intersect(
  intersect(ocp_empleo_serie$year, auto_empleo_serie$year[auto_empleo_serie$nota == "declarado/observado"]),
  intersect(fosfatos_produccion$year, auto_produccion$year)
)

fpp_familia <- map_dfr(anios_fpp_familia, function(anio) {
  L_f_anio <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio]
  L_a_anio <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio]
  L_tot_anio <- L_f_anio + L_a_anio
  Q_f_anio <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio]
  Q_a_anio <- auto_produccion$unidades[auto_produccion$year == anio]
  A_f_anio <- Q_f_anio / (L_f_anio ^ (1 - alpha_fosfatos))
  A_a_anio <- Q_a_anio / (L_a_anio ^ (1 - alpha_auto))
  
  tibble(year = anio, L_fosfatos = seq(1000, L_tot_anio - 1000, length.out = 200)) |>
    mutate(
      L_auto     = L_tot_anio - L_fosfatos,
      Q_fosfatos = A_f_anio * L_fosfatos ^ (1 - alpha_fosfatos),
      Q_auto     = A_a_anio * L_auto     ^ (1 - alpha_auto)
    )
})

g_fpp_desplazamiento <- ggplot() +
  geom_path(data = fpp_familia, aes(x = Q_fosfatos, y = Q_auto, color = factor(year), group = year),
            linewidth = 1) +
  geom_point(data = fpp_observado |> filter(year %in% anios_fpp_familia),
             aes(x = Q_fosfatos, y = Q_auto, color = factor(year)), size = 2.5) +
  labs(title = "Desplazamiento de la FPP de Marruecos, año a año",
       subtitle = "Una curva calibrada por año (no una sola curva fija) - cada punto observado cae sobre su propia curva",
       x = "Producción de fosfatos (miles de toneladas)",
       y = "Producción automotriz (unidades)",
       color = "Año",
       caption = "Cada curva recalibra A_fosfatos/A_auto con el empleo y la producción observados ESE año, manteniendo fijo el alpha de io_coeficientes.") +
  theme_tp1()
g_fpp_desplazamiento
ggsave(paste0(ruta_graficos, "grafico_fpp_desplazamiento.png"), g_fpp_desplazamiento,
       width = 10, height = 7, dpi = 300, bg = "white")

## --- 2.4 Caja de asignación del trabajo y salario de equilibrio (VPMgL) ---
precio_fosfatos <- ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp] / Q_fosfatos_0
precio_auto     <- auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp] / Q_auto_0

caja_asignacion <- fpp_grilla |>
  mutate(
    pmgl_fosfatos  = (1 - alpha_fosfatos) * A_fosfatos * L_fosfatos ^ (-alpha_fosfatos),
    pmgl_auto      = (1 - alpha_auto)     * A_auto     * L_auto     ^ (-alpha_auto),
    vpmgl_fosfatos = precio_fosfatos * pmgl_fosfatos,
    vpmgl_auto     = precio_auto     * pmgl_auto
  )

fila_equilibrio <- caja_asignacion |>
  mutate(brecha = abs(vpmgl_fosfatos - vpmgl_auto)) |>
  slice_min(brecha, n = 1)
w_equilibrio  <- mean(c(fila_equilibrio$vpmgl_fosfatos, fila_equilibrio$vpmgl_auto))
L_fosfatos_eq <- fila_equilibrio$L_fosfatos

## --- 2.4.1 VPMgL en el punto EFECTIVAMENTE OBSERVADO (no solo el teórico) ---
vpmgl_fosfatos_obs <- (1 - alpha_fosfatos) *
  (ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp] / L_fosfatos_0)
vpmgl_auto_obs <- (1 - alpha_auto) *
  (auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp] / L_auto_0)
brecha_vpmgl_obs <- vpmgl_fosfatos_obs / vpmgl_auto_obs

vpmgl_fosfatos_obs_consolidado <- (1 - alpha_fosfatos) *
  (ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp] / ocp_empleo_consolidado_alt)
brecha_vpmgl_obs_consolidado <- vpmgl_fosfatos_obs_consolidado / vpmgl_auto_obs

vpmgl_fosfatos_obs_oc <- (1 - alpha_fosfatos) *
  (auto_exportaciones_oc$fosfatos_ref_mmad[auto_exportaciones_oc$year == anio_ref_fpp] / L_fosfatos_0)
brecha_vpmgl_obs_oc <- vpmgl_fosfatos_obs_oc / vpmgl_auto_obs

tabla_robustez_vpmgl <- tibble(
  supuesto = c("Principal: 17.000 empleados, ingresos totales OCP",
               "Robustez 1: 20.000 empleados (consolidado)",
               "Robustez 2: exportación aduanera de fosfatos (no ingresos totales OCP)"),
  vpmgl_fosfatos_mmad_anio = c(vpmgl_fosfatos_obs, vpmgl_fosfatos_obs_consolidado, vpmgl_fosfatos_obs_oc),
  brecha_vs_automotor = c(brecha_vpmgl_obs, brecha_vpmgl_obs_consolidado, brecha_vpmgl_obs_oc)
)
tabla_robustez_vpmgl
write_csv(tabla_robustez_vpmgl, paste0(ruta_tablas, "robustez_vpmgl_observado.csv"))

g_caja_asignacion <- ggplot(caja_asignacion, aes(x = L_fosfatos)) +
  geom_line(aes(y = vpmgl_fosfatos, color = "Fosfatos"), linewidth = 1.1) +
  geom_line(aes(y = vpmgl_auto, color = "Automotor"), linewidth = 1.1) +
  geom_vline(xintercept = L_fosfatos_eq, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = w_equilibrio, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = L_fosfatos_0, linetype = "dotted", color = "gray20", linewidth = 0.8) +
  annotate("point", x = L_fosfatos_0, y = vpmgl_fosfatos_obs, size = 3, color = paleta_sectores["Fosfatos"]) +
  annotate("point", x = L_fosfatos_0, y = vpmgl_auto_obs, size = 3, color = paleta_sectores["Automotor"]) +
  coord_cartesian(ylim = c(0, 2)) +
  scale_color_manual(values = paleta_sectores) +
  labs(title = "Caja de asignación del trabajo — MFE",
       subtitle = paste0("Equilibrio competitivo teórico: w* \u2248 ", round(w_equilibrio, 3),
                         " en L_fosfatos \u2248 ", format(round(L_fosfatos_eq), big.mark = "."),
                         "  |  Punto observado 2023: L_fosfatos = ", format(L_fosfatos_0, big.mark = ".")),
       x = "Trabajo asignado a fosfatos (L_fosfatos)", y = "VPMgL (precio implícito × PMgL)",
       color = NULL,
       caption = paste0(
         "En el punto observado, VPMgL_fosfatos \u2248 ", round(vpmgl_fosfatos_obs, 2),
         " vs. VPMgL_automotor \u2248 ", round(vpmgl_auto_obs, 2), " M MAD/trabajador/año (brecha \u2248 ",
         round(brecha_vpmgl_obs, 1), "x). La distancia al equilibrio teórico refleja segmentación ",
         "institucional del mercado de trabajo (ver 2.6/Bloque 4), no un error de unidades (tabla_robustez_vpmgl)."
       )) +
  theme_tp1()
g_caja_asignacion
ggsave(paste0(ruta_graficos, "grafico_caja_asignacion.png"), g_caja_asignacion,
       width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.4.2 ¿Cobb-Douglas es la tecnología correcta para extrapolar esto? ---
aL_fosfatos_leontief <- L_fosfatos_0 / Q_fosfatos_0
Q_max_fosfatos <- max(fosfatos_produccion$produccion_mt)
L_capacidad_leontief <- aL_fosfatos_leontief * Q_max_fosfatos

comparacion_tecnologias <- tibble(
  L_fosfatos_observado_2023            = L_fosfatos_0,
  L_capacidad_leontief_a_pico_historico = round(L_capacidad_leontief),
  L_fosfatos_equilibrio_cobb_douglas   = round(L_fosfatos_eq)
)
comparacion_tecnologias
write_csv(comparacion_tecnologias, paste0(ruta_tablas, "comparacion_leontief_cobb_douglas.csv"))

leontief_vs_cd <- tibble(L_fosfatos = seq(1000, L_total - 1000, length.out = 300)) |>
  mutate(
    Q_cobb_douglas = A_fosfatos * L_fosfatos ^ (1 - alpha_fosfatos),
    Q_leontief     = pmin(L_fosfatos / aL_fosfatos_leontief, Q_max_fosfatos)
  )

g_leontief_vs_cd <- ggplot(leontief_vs_cd, aes(x = L_fosfatos)) +
  geom_line(aes(y = Q_cobb_douglas, color = "Cobb-Douglas (sustituible)"), linewidth = 1.1) +
  geom_line(aes(y = Q_leontief, color = "Leontief (coeficientes fijos)"), linewidth = 1.1) +
  geom_vline(xintercept = L_fosfatos_0, linetype = "dotted", color = "gray30") +
  geom_vline(xintercept = L_fosfatos_eq, linetype = "dashed", color = "gray30") +
  scale_color_manual(values = c("Cobb-Douglas (sustituible)" = "gray50",
                                "Leontief (coeficientes fijos)" = paleta_sectores[["Fosfatos"]])) +
  labs(title = "¿Qué tecnología describe mejor a la minería de fosfatos?",
       subtitle = "Producción de fosfatos bajo dos supuestos tecnológicos, calibrados en el mismo punto observado (2023)",
       x = "Trabajo asignado a fosfatos (L_fosfatos)", y = "Producción de fosfatos (miles de toneladas)",
       color = NULL,
       caption = paste0(
         "Línea punteada: empleo observado 2023 (", format(L_fosfatos_0, big.mark = "."),
         "). Línea rayada: equilibrio Cobb-Douglas (", format(round(L_fosfatos_eq), big.mark = "."),
         "). Bajo Leontief, la producción no supera el pico histórico (",
         format(Q_max_fosfatos, big.mark = "."), " mil t) sin importar cuánto trabajo se agregue."
       )) +
  theme_tp1()
g_leontief_vs_cd
ggsave(paste0(ruta_graficos, "grafico_leontief_vs_cobb_douglas.png"), g_leontief_vs_cd,
       width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.5 Shock de términos de intercambio ---
tot_marruecos <- WDI(country = "MA", indicator = "TT.PRI.MRCH.XD.WD",
                     start = 2015, end = 2025) |>
  as_tibble() |>
  select(year, tot = TT.PRI.MRCH.XD.WD) |>
  arrange(year)

g_tot <- ggplot(tot_marruecos, aes(x = year, y = tot)) +
  geom_line(linewidth = 1.1, color = paleta_sectores["Fosfatos"]) +
  geom_point(size = 2.2, color = paleta_sectores["Fosfatos"]) +
  labs(title = "Términos de intercambio de Marruecos",
       subtitle = "Índice de términos de intercambio de mercancías (2000=100)",
       x = "Año", y = "Índice ToT",
       caption = "Fuente: World Bank WDI, TT.PRI.MRCH.XD.WD. No CEPAL: no cubre países fuera de América Latina/Caribe.") +
  theme_tp1()
g_tot
ggsave(paste0(ruta_graficos, "grafico_tot.png"), g_tot, width = 10, height = 6, dpi = 300, bg = "white")

g_precios <- precios_internacionales |>
  pivot_longer(cols = c(dap_usd_mt, roca_fosforica_usd_mt), names_to = "producto", values_to = "precio") |>
  mutate(producto = recode(producto, dap_usd_mt = "DAP", roca_fosforica_usd_mt = "Roca fosfórica")) |>
  ggplot(aes(x = year, y = precio, color = producto)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  labs(title = "Precio internacional de fertilizantes",
       subtitle = "USD por tonelada métrica, f.o.b.",
       x = "Año", y = "USD/mt", color = NULL,
       caption = "Fuente: World Bank Commodity Markets (Pink Sheet), vía Excel de Gemini.") +
  theme_tp1()
g_precios
ggsave(paste0(ruta_graficos, "grafico_precios_internacionales.png"), g_precios,
       width = 10, height = 6, dpi = 300, bg = "white")

## --- 2.6 Efectos distributivos observados (salarios) ---
# salario_promedio_general y salarios_cnss_sector_2020 ya están cargados
# en el Bloque 0.5 - se usan acá para contrastar la predicción teórica de
# ganadores/perdedores del MFE (inciso d) y para explicar la brecha de
# 2.4.1/2.4.2 como segmentación institucional, no ausencia de rendimientos
# decrecientes.


#===============================================================================#
# BLOQUE 3: MODELO DE HECKSCHER-OHLIN (largo plazo)
#===============================================================================#

## --- 3.1 Justificación empírica de las intensidades factoriales ---
productividad_valor_fosfatos <- inner_join(
  ocp_financieros |> select(year, ingresos_mmad),
  ocp_empleo_serie, by = "year"
) |>
  mutate(valor_por_trabajador_mmad = ingresos_mmad / empleo, sector = "Fosfatos")

productividad_valor_auto <- inner_join(
  auto_exportaciones_oc |> select(year, total_mmad),
  auto_empleo_serie |> select(year, empleo), by = "year"
) |>
  mutate(valor_por_trabajador_mmad = total_mmad / empleo, sector = "Automotor")

brecha_valor_trabajador <- bind_rows(
  productividad_valor_fosfatos |> select(year, sector, valor_por_trabajador_mmad),
  productividad_valor_auto     |> select(year, sector, valor_por_trabajador_mmad)
) |>
  pivot_wider(names_from = sector, values_from = valor_por_trabajador_mmad) |>
  mutate(brecha_veces = Fosfatos / Automotor)

brecha_valor_trabajador

## --- 3.2 Dotación factorial relativa (Penn World Tables) ---
data("pwt10.01", package = "pwt10")
paises_benchmark <- c("Morocco", "Germany", "Spain", "France")

kl_benchmark <- pwt10.01 |>
  filter(country %in% paises_benchmark) |>
  mutate(kl = rnna / emp) |>
  select(country, year, kl, hc, labsh)

g_kl <- kl_benchmark |>
  filter(year >= 2000) |>
  ggplot(aes(x = year, y = kl, color = country)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = paleta_paises_ho) +
  labs(title = "Capital por trabajador (K/L)",
       subtitle = "Marruecos vs. Alemania, España y Francia",
       x = "Año", y = "K/L (PWT rnna/emp)", color = NULL,
       caption = "Fuente: Penn World Table 10.01.") +
  theme_tp1()
g_kl
ggsave(paste0(ruta_graficos, "grafico_kl_benchmark.png"), g_kl,
       width = 10, height = 6.5, dpi = 300, bg = "white")
# TODO: definir si el promedio UE es simple o ponderado (PBI/población).

g_labsh <- pwt10.01 |>
  filter(country == "Morocco") |>
  ggplot(aes(x = year, y = labsh)) +
  geom_line(linewidth = 1.1, color = paleta_sectores["Automotor"]) +
  labs(title = "Participación del trabajo en el ingreso — Marruecos",
       subtitle = "Insumo para testear Stolper-Samuelson de largo plazo (3d)",
       x = "Año", y = "labsh (PWT)",
       caption = "Fuente: Penn World Table 10.01.") +
  theme_tp1()
g_labsh
ggsave(paste0(ruta_graficos, "grafico_labsh_marruecos.png"), g_labsh,
       width = 10, height = 6, dpi = 300, bg = "white")

## --- 3.3 Requerimientos técnicos por rama (reutiliza io_coeficientes) ---
# Ver Bloque 0.5. No reemplazan una matriz insumo-producto real por rama
# individual (no existe en fuentes abiertas por secreto estadístico, Ley
# 371-71), pero son el máximo nivel de detalle técnico disponible para HO.

## --- 3.4 Origen del capital extranjero (movilidad de largo plazo) ---
g_ied_origen <- ggplot(ied_origen_pais_2020, aes(x = reorder(pais, stock_mmad), y = stock_mmad)) +
  geom_col(fill = paleta_sectores["Automotor"]) +
  coord_flip() +
  labs(title = "Stock de IED en Marruecos por país de origen",
       subtitle = "2020, millones de MAD",
       x = NULL, y = "Millones de MAD",
       caption = "Fuente: Office des Changes (PEG 2020) / DG Trésor / UNCTADstat, vía Excel de Gemini.") +
  theme_tp1()
g_ied_origen
ggsave(paste0(ruta_graficos, "grafico_ied_origen.png"), g_ied_origen,
       width = 9, height = 6, dpi = 300, bg = "white")


#===============================================================================#
# BLOQUE 4: SÍNTESIS Y POLÍTICA
#===============================================================================#
# hcp_va_crecimiento, hcp_va_total_mmad, caisse_compensacion y
# regimen_amdie_texto ya están cargados en el Bloque 0.5. El excedente de
# VPMgL que OCP no distribuye como salario se capta como dividendo del
# Estado (ocp_financieros$dividendos_mmad) y financia en parte la Caisse
# de Compensation - candidato natural para el inciso 4b junto con la
# brecha de renta de 2.4.1.


#===============================================================================#
# BLOQUE 5: LO QUE QUEDA GENUINAMENTE SIN RESOLVER (no insistir con Spark)
#===============================================================================#
# 1. Matriz insumo-producto desagregada a nivel de rama individual - no
#    existe en fuentes abiertas (Ley 371-71). io_coeficientes es el máximo
#    detalle disponible.
# 2. Salario CNSS específico de automotor (vs. "Industrie" agregada) -
#    mismo motivo estructural.
# 3. Variante "Opción B" sugerida por Gemini para 3.1 (OECD TiVA/ICIO) -
#    no implementada.
# 4. aL_fosfatos_leontief se calibra con un solo año y Q_max_fosfatos usa
#    el pico histórico de la serie como proxy de capacidad instalada - si
#    aparece la capacidad nominal real de OCP, reemplazar ese valor.
# 5. anio_ref_fpp está fijo en 2023 - es la única variable que hay que
#    cambiar para recalcular la FPP, la caja de asignación y la
#    comparación Leontief/Cobb-Douglas completas.
# 6. El parsing de la Sección 0.5 (case_when/str_starts/str_detect de
#    nombres de variable) depende de que el texto exacto de la columna
#    Variable del Excel no cambie - si Spark vuelve a tocar la planilla,
#    revisar el chequeo de cobertura al final de esa sección antes de
#    confiar en el resto del script.