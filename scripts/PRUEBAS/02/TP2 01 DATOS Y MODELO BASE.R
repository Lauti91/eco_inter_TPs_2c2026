#===============================================================================#
# TP2 - SCRIPT 1 de 2: DATOS Y MODELO BASE (MFE + HO con alpha de HCP)
#===============================================================================#
# CONSOLIDACIÓN: este archivo junta TEST_5 completo (carga desde Excel +
# Bloques 1-4 con alpha de HCP) en su versión más actualizada. Es la base
# "principal" del trabajo: usa el parámetro alpha de HCP (Escenario A) y
# construye la FPP, la caja de asignación y la comparación Leontief tal
# como quedaron validadas.
#
# EL SCRIPT 2 (02_TP2_oecd_correccion_y_robustez.R) DEPENDE DE ESTE - hay
# que correr este primero. El Script 2 agrega: la segunda fuente de alpha
# (OCDE), los perímetros corregidos (OCP no es solo minería; el complejo
# automotor no es solo ensamblaje), el módulo de predicción del salario, y
# la comparación de escenarios A/B.
#
# PENDIENTE DE RECONCILIAR (no resuelto en esta consolidación): la caja de
# asignación de este script (sub-bloque 2.4) usa precio implícito =
# ingresos/producción física, que es la fórmula original y algebraicamente
# válida (VPMgL = (1-alpha)*ingresos/L). El Script 2 usa una fórmula
# distinta (participación_trabajo * VA / L) para el PUNTO observado, que
# corrige el sesgo de mezclar valor agregado con producción bruta (ver
# Script 2, sección C.3). Ambas conviven hoy: la curva completa de la caja
# de asignación (este script) todavía no está recalculada con el enfoque
# de VA. Si se decide adoptar el enfoque de VA como definitivo, falta
# reconstruir fpp_grilla/caja_asignacion completas con esa fórmula - hoy
# solo el punto observado (Script 2) usa la versión corregida.
#
# No está corrido de punta a punta en esta consolidación - cada bloque ya
# corrió por separado en sesiones anteriores; revisar dependencias de
# objetos si se ejecuta de cero.

objetos_tp1 <- "output/tablas/01/objetos_heredados_tp1.RData"
objetos_necesarios <- c("vcr_mar", "iic_heatmap_completo", "mar_exp", "theme_tp1", "nombres_socio")

if (file.exists(objetos_tp1)) {
  load(objetos_tp1)
}
if (!all(objetos_necesarios %in% ls())) {
  source("scripts/FINAL/01/01_INDICADORES_FINAL.R", print.eval = FALSE)
}
# print.eval = FALSE evita "Viewport has zero dimension(s)" si el panel de
# Plots está colapsado durante el source(). No afecta los ggsave() del TP1.

paquetes_tp2 <- c("tidyverse", "WDI", "pwt10", "readxl", "broom", "scales", "ggrepel", "RColorBrewer")
for (pkg in paquetes_tp2) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(tidyverse); library(WDI); library(pwt10); library(readxl); library(broom)

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
# METODOLOGÍA: los datos se relevaron con un agente de IA (Gemini Spark)
# siguiendo instrucciones puntuales sobre qué fuentes primarias consultar
# (OCP, USGS, Office des Changes, HCP, CNSS, UNCTADstat, AMDIE) - no
# generados desde el conocimiento propio del modelo. Cada fila trae su
# fuente, URL exacta y fecha de consulta. Ver TP2_fuentes_de_datos.md y
# TP2_seguimiento_investigacion_ronda2.md.

ruta_investigacion <- "bases de datos/TP2_Economia_Internacional_Marruecos_Fuentes_de_Datos.xlsx"

leer_hoja_investigacion <- function(ruta, hoja) {
  read_excel(ruta, sheet = hoja) |>
    set_names(c("bloque", "punto", "variable", "valor_raw", "unidad",
                "anio", "fuente", "url", "fecha_consulta", "notas"))
}

parsear_valor <- function(valor_raw) {
  x <- str_trim(as.character(valor_raw))
  es_no_encontrado <- str_detect(x, regex("NO ENCONTRADO", ignore_case = TRUE))
  # Un rango tiene un dígito o "%" ANTES del guion ("75% - 85%"); un
  # negativo ("-4.2") tiene el guion al principio - así no se confunden
  # (bug real detectado y corregido: "-4.2" se perdía como NA antes).
  es_rango <- str_detect(x, "[%\\d]\\s*-\\s*\\d") & !es_no_encontrado
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
    valor = valor_unico, valor_min = valor_min, valor_max = valor_max,
    valor_medio = ifelse(es_rango, (valor_min + valor_max) / 2, valor_unico)
  )
}

datos_investigacion <- bind_rows(
  leer_hoja_investigacion(ruta_investigacion, "Ronda 1") |> mutate(ronda = 1),
  leer_hoja_investigacion(ruta_investigacion, "Ronda 2 - Profundización") |> mutate(ronda = 2),
  leer_hoja_investigacion(ruta_investigacion, "Ronda 3") |> mutate(ronda = 3)
) |>
  mutate(anio = as.integer(anio))
# Nota: la pestaña "Ronda 3" tiene la columna de fuente llamada "Fuente" en
# vez de "Fuente (nombre)" - no rompe nada porque leer_hoja_investigacion()
# renombra por posición, no por nombre.

datos_investigacion <- bind_cols(datos_investigacion, parsear_valor(datos_investigacion$valor_raw))

# Chequeo de cobertura: filas no convertidas a número, que no sean "NO
# ENCONTRADO" - debería quedar vacío salvo texto puramente descriptivo
# (régimen fiscal de AMDIE).
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

## --- OCP: financieros, empleo ---
ocp_financieros <- extraer_serie("Ingresos (Chiffre d'affaires)") |> rename(ingresos_mmad = valor)
ocp_financieros <- full_join(ocp_financieros,
                             extraer_serie("EBITDA") |> rename(ebitda_mmad = valor), by = "year")
ocp_financieros <- full_join(ocp_financieros,
                             extraer_serie("Gastos de capital (Capex)") |> rename(capex_mmad = valor), by = "year")
ocp_financieros <- full_join(ocp_financieros,
                             extraer_serie("Dividendos pagados al Tesoro marroquí") |> rename(dividendos_mmad = valor), by = "year") |>
  arrange(year)

ocp_empleo_serie <- extraer_serie("Empleo directo OCP Group (Fosfatos)") |> rename(empleo = valor)
ocp_empleo_consolidado_alt <- 20000  # chequeo de robustez, no serie principal

## --- USGS: reservas y producción física de roca fosfórica ---
fosfatos_reservas <- extraer_serie("Reservas de roca fosfórica (Marruecos)") |> rename(reservas_mt = valor)
unidad_reservas <- datos_investigacion |>
  filter(variable == "Reservas de roca fosfórica (Marruecos)") |> pull(unidad) |> unique() |> first()
# Confirmado: 5e+07 miles de toneladas métricas = 50 Gt (~70% mundial).

fosfatos_produccion <- extraer_serie("Producción física de roca fosfórica") |> rename(produccion_mt = valor)

## --- Automotor: producción física, empleo, exportaciones ---
auto_produccion <- extraer_serie("Producción de vehículos de motor") |> rename(unidades = valor)

auto_empleo_serie <- bind_rows(
  datos_investigacion |>
    filter(variable == "Empleo total sector automotor") |>
    transmute(year = anio, empleo = valor, nota = "declarado/observado"),
  datos_investigacion |>
    filter(variable == "Empleo total sector automotor (meta proyectada)") |>
    transmute(year = anio, empleo = valor, nota = "meta proyectada, no observada")
) |> arrange(year)

auto_exportaciones_oc <- extraer_serie("Exportaciones sector automotor (Total)") |> rename(total_mmad = valor)
auto_exportaciones_oc <- full_join(auto_exportaciones_oc,
                                   extraer_serie("Exportaciones automotrices - Segmento Câblage (Cableado)") |> rename(cableado_mmad = valor),
                                   by = "year")
auto_exportaciones_oc <- full_join(auto_exportaciones_oc,
                                   extraer_serie("Exportaciones automotrices - Segmento Construction (Ensamble)") |> rename(ensamblaje_mmad = valor),
                                   by = "year")
auto_exportaciones_oc <- full_join(auto_exportaciones_oc,
                                   extraer_serie("Exportaciones Phosphates et dérivés (Referencia cruce)") |> rename(fosfatos_ref_mmad = valor),
                                   by = "year") |> arrange(year)

## --- Coeficientes HCP (TRE Base 2014) - Escenario A de alpha ---
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

## --- Salarios (CNSS) ---
salario_promedio_general <- extraer_serie("Salario medio mensual declarado - Total cotizantes régimen general") |>
  rename(salario_mad_mes = valor)
salarios_cnss_sector_2020 <- datos_investigacion |>
  filter(str_starts(variable, "Salario medio declarado -"), anio == 2020) |>
  transmute(sector = str_remove(variable, "^Salario medio declarado - "), salario_mad_mes = valor)
# Solo 4 filas reales (Industrie, Agriculture, Transports, Financiero) - no
# 7 como en un intento manual anterior.

## --- IED: stock total, origen, inversión automotor ---
ied_stock_total <- extraer_serie("Stock de IED entrante en Marruecos (Inward FDI Stock)") |> rename(stock_musd = valor)

ied_origen_pais_2020 <- datos_investigacion |>
  filter(str_starts(variable, "Stock de IED entrante originario de"), anio == 2020) |>
  transmute(pais = str_remove(variable, "^Stock de IED entrante originario de "), stock_mmad = valor_medio)

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

ied_manufactura <- extraer_serie("Stock de IED en industria manufacturera") |> rename(stock_mmad = valor)
ied_manufactura_flujo <- datos_investigacion |>
  filter(variable == "Flujo neto anual de IED manufacturera") |> select(anio, valor_min, valor_max)

## --- HCP: valor agregado sectorial / Caisse de Compensation / AMDIE ---
hcp_va_crecimiento <- extraer_serie("Crecimiento volumen VA - Minería (B00)") |> rename(mineria_pct = valor)
hcp_va_crecimiento <- full_join(hcp_va_crecimiento,
                                extraer_serie("Crecimiento volumen VA - Manufactura (C00)") |> rename(manufactura_pct = valor),
                                by = "year") |> arrange(year)

hcp_va_total_mmad <- extraer_serie("Valor agregado total economía (precios básicos)") |> rename(total = valor)

caisse_compensacion <- bind_rows(
  datos_investigacion |>
    filter(variable == "Carga global de compensación (Caisse de Compensation)") |>
    transmute(year = anio, carga_mmad = valor, nota = NA_character_),
  datos_investigacion |>
    filter(variable == "Carga global de compensación presupuestada") |>
    transmute(year = anio, carga_mmad = valor, nota = "presupuestado, Loi de Finances")
) |> arrange(year)

regimen_amdie_texto <- datos_investigacion |>
  filter(variable == "Régimen de zonas francas e incentivos fiscales a la inversión automotriz") |>
  pull(valor_raw)

## --- Precios internacionales (Pink Sheet) ---
precios_internacionales <- extraer_serie("Precio internacional DAP (f.o.b. US Gulf)") |> rename(dap_usd_mt = valor)
precios_internacionales <- full_join(precios_internacionales,
                                     extraer_serie("Precio internacional roca fosfórica (f.o.b. North Africa)") |> rename(roca_fosforica_usd_mt = valor),
                                     by = "year")
precios_internacionales <- full_join(precios_internacionales,
                                     extraer_serie("Precio internacional gas natural (Europe TTF)") |> rename(gas_natural_usd_mmbtu = valor),
                                     by = "year") |> arrange(year)


#===============================================================================#
# BLOQUE 1: PUNTO DE PARTIDA
#===============================================================================#

vcrn_mfe <- vcr_mar |>
  filter(cuci %in% sectores_mfe) |>
  mutate(sector_desc = nombres_sectores_mfe[cuci],
         grupo = if_else(cuci %in% c("272", "562"), "Fosfatos", "Automotor"))

g_vcrn_mfe <- ggplot(vcrn_mfe, aes(x = year, y = vcrn, color = sector_desc, linetype = grupo)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "VCRN de fosfatos vs. automotor, 2021-2025",
       subtitle = "Ventaja comparativa revelada normalizada (Balassa)",
       x = "Año", y = "VCRN", color = "Producto", linetype = "Sector",
       caption = "Fuente: bases WITS del TP1 (vcr_mar).") +
  theme_tp1()
g_vcrn_mfe
ggsave(paste0(ruta_graficos, "grafico_vcrn_mfe.png"), g_vcrn_mfe, width = 10, height = 6.5, dpi = 300, bg = "white")

exportaciones_mfe_wits <- mar_exp |>
  filter(p == "WLD", cuci %in% sectores_mfe) |>
  mutate(sector_desc = nombres_sectores_mfe[cuci],
         grupo = if_else(cuci %in% c("272", "562"), "Fosfatos", "Automotor")) |>
  select(year, cuci, sector_desc, grupo, value, share)
# Pendiente de uso explícito: inciso 3c (contrastar predicción HO contra
# el patrón de comercio observado en el TP1) - todavía sin redactar.

iic_automotor <- iic_heatmap_completo |>
  filter(cuci %in% c("781", "784")) |>
  left_join(tibble(socio = names(nombres_socio), socio_nombre = nombres_socio), by = "socio")
iic_automotor


#===============================================================================#
# BLOQUE 2: MODELO DE FACTORES ESPECÍFICOS (corto plazo) - alpha de HCP
#===============================================================================#
# Factor específico de cada sector (para el inciso 2a, no "capital" en
# abstracto): fosfatos = tierra/reservas geológicas + infraestructura fija
# (tubería Khouribga-Jorf Lasfar, plantas químicas); automotor = capital
# instalado vía IED (plantas de Tánger y Kenitra). El factor móvil es el
# trabajo en ambos - y el hallazgo central de este bloque (2.4) es que ese
# supuesto de movilidad libre no se cumple en la práctica.

alpha_fosfatos <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
alpha_auto     <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]

## --- 2.2 Productividad media del trabajo en el tiempo ---
calcular_vpmgl_cobb_douglas <- function(produccion, empleo, alpha) {
  base <- inner_join(produccion, empleo, by = "year")
  col_prod <- names(produccion)[2]
  base |> mutate(producto_medio_trabajo = .data[[col_prod]] / empleo,
                 pmgl_relativo = (1 - alpha) * producto_medio_trabajo)
}

vpmgl_fosfatos <- calcular_vpmgl_cobb_douglas(fosfatos_produccion, ocp_empleo_serie, alpha_fosfatos)
vpmgl_automotor <- calcular_vpmgl_cobb_douglas(
  auto_produccion |> rename(unidades_producidas = unidades),
  auto_empleo_serie |> select(year, empleo), alpha_auto
)

g_productividad <- bind_rows(
  vpmgl_fosfatos |> mutate(sector = "Fosfatos"),
  vpmgl_automotor |> mutate(sector = "Automotor")
) |>
  ggplot(aes(x = year, y = producto_medio_trabajo, color = sector)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  scale_color_manual(values = paleta_sectores) + scale_y_log10() +
  labs(title = "Productividad media del trabajo (proxy de VPMgL)",
       subtitle = "Producción física / empleo, escala log",
       x = "Año", y = "Unidades físicas / empleado (log)", color = NULL,
       caption = "Calibrado con coeficientes de reparto del VA (HCP) como alpha de un Cobb-Douglas simple.") +
  theme_tp1()
g_productividad
ggsave(paste0(ruta_graficos, "grafico_productividad_mfe.png"), g_productividad, width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.3 FPP real (plano Q_fosfatos x Q_automotor) ---
anio_ref_fpp <- 2023

L_fosfatos_0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref_fpp]
L_auto_0     <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref_fpp]
L_total      <- L_fosfatos_0 + L_auto_0  # 247.000

Q_fosfatos_0 <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio_ref_fpp]
Q_auto_0     <- auto_produccion$unidades[auto_produccion$year == anio_ref_fpp]

A_fosfatos <- Q_fosfatos_0 / (L_fosfatos_0 ^ (1 - alpha_fosfatos))
A_auto     <- Q_auto_0     / (L_auto_0     ^ (1 - alpha_auto))

fpp_grilla <- tibble(L_fosfatos = seq(1000, L_total - 1000, length.out = 2000)) |>
  mutate(L_auto = L_total - L_fosfatos,
         Q_fosfatos = A_fosfatos * L_fosfatos ^ (1 - alpha_fosfatos),
         Q_auto     = A_auto     * L_auto     ^ (1 - alpha_auto))

fpp_observado <- inner_join(
  fosfatos_produccion |> rename(Q_fosfatos = produccion_mt),
  auto_produccion |> rename(Q_auto = unidades), by = "year"
)

g_fpp <- ggplot() +
  geom_path(data = fpp_grilla, aes(x = Q_fosfatos, y = Q_auto), linewidth = 1.1, color = "gray30") +
  geom_point(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, color = factor(year)), size = 3) +
  geom_text(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, label = year), vjust = -1, size = 3) +
  labs(title = "FPP empírica de Marruecos: fosfatos vs. automotor",
       subtitle = paste0("Curva calibrada con Cobb-Douglas sobre L = ", format(L_total, big.mark = "."),
                         " trabajadores (", anio_ref_fpp, ")"),
       x = "Producción de fosfatos (miles de toneladas)", y = "Producción automotriz (unidades)",
       color = "Año observado") +
  theme_tp1()
g_fpp
ggsave(paste0(ruta_graficos, "grafico_fpp_real.png"), g_fpp, width = 10, height = 7, dpi = 300, bg = "white")

anios_fpp_familia <- intersect(
  intersect(ocp_empleo_serie$year, auto_empleo_serie$year[auto_empleo_serie$nota == "declarado/observado"]),
  intersect(fosfatos_produccion$year, auto_produccion$year)
)
fpp_familia <- map_dfr(anios_fpp_familia, function(anio) {
  L_f <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio]
  L_a <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio]
  Q_f <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio]
  Q_a <- auto_produccion$unidades[auto_produccion$year == anio]
  A_f <- Q_f / (L_f ^ (1 - alpha_fosfatos)); A_a <- Q_a / (L_a ^ (1 - alpha_auto))
  tibble(year = anio, L_fosfatos = seq(1000, L_f + L_a - 1000, length.out = 200)) |>
    mutate(L_auto = (L_f + L_a) - L_fosfatos,
           Q_fosfatos = A_f * L_fosfatos ^ (1 - alpha_fosfatos),
           Q_auto     = A_a * L_auto     ^ (1 - alpha_auto))
})

g_fpp_desplazamiento <- ggplot() +
  geom_path(data = fpp_familia, aes(x = Q_fosfatos, y = Q_auto, color = factor(year), group = year), linewidth = 1) +
  geom_point(data = fpp_observado |> filter(year %in% anios_fpp_familia),
             aes(x = Q_fosfatos, y = Q_auto, color = factor(year)), size = 2.5) +
  labs(title = "Desplazamiento de la FPP de Marruecos, año a año",
       x = "Producción de fosfatos (miles de toneladas)", y = "Producción automotriz (unidades)", color = "Año") +
  theme_tp1()
g_fpp_desplazamiento
ggsave(paste0(ruta_graficos, "grafico_fpp_desplazamiento.png"), g_fpp_desplazamiento, width = 10, height = 7, dpi = 300, bg = "white")

## --- 2.4 Caja de asignación (precio implícito = ingresos/producción física) ---
precio_fosfatos <- ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp] / Q_fosfatos_0
precio_auto     <- auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp] / Q_auto_0

caja_asignacion <- fpp_grilla |>
  mutate(pmgl_fosfatos  = (1 - alpha_fosfatos) * A_fosfatos * L_fosfatos ^ (-alpha_fosfatos),
         pmgl_auto      = (1 - alpha_auto)     * A_auto     * L_auto     ^ (-alpha_auto),
         vpmgl_fosfatos = precio_fosfatos * pmgl_fosfatos,
         vpmgl_auto     = precio_auto     * pmgl_auto)

fila_equilibrio <- caja_asignacion |> mutate(brecha = abs(vpmgl_fosfatos - vpmgl_auto)) |> slice_min(brecha, n = 1)
w_equilibrio  <- mean(c(fila_equilibrio$vpmgl_fosfatos, fila_equilibrio$vpmgl_auto))
L_fosfatos_eq <- fila_equilibrio$L_fosfatos  # ~75.565 - ver Script 2 (F.2 Leontief) por qué no citar este número exacto

vpmgl_fosfatos_obs <- (1 - alpha_fosfatos) * (ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp] / L_fosfatos_0)
vpmgl_auto_obs     <- (1 - alpha_auto)     * (auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp] / L_auto_0)
brecha_vpmgl_obs   <- vpmgl_fosfatos_obs / vpmgl_auto_obs  # 3,87x (ver Script 2 para la versión sobre VA: 13,6x)

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
       subtitle = paste0("Equilibrio teórico: w* \u2248 ", round(w_equilibrio, 3),
                         " en L_fosfatos \u2248 ", format(round(L_fosfatos_eq), big.mark = "."),
                         "  |  Observado 2023: L_fosfatos = ", format(L_fosfatos_0, big.mark = ".")),
       x = "Trabajo asignado a fosfatos", y = "VPMgL (precio implícito × PMgL)", color = NULL,
       caption = paste0("Brecha en el punto observado: ", round(brecha_vpmgl_obs, 1),
                        "x (versión ingresos/producción; ver Script 2 para la versión sobre valor agregado).")) +
  theme_tp1()
g_caja_asignacion
ggsave(paste0(ruta_graficos, "grafico_caja_asignacion.png"), g_caja_asignacion, width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.5 Shock de términos de intercambio ---
tot_marruecos <- WDI(country = "MA", indicator = "TT.PRI.MRCH.XD.WD", start = 2015, end = 2025) |>
  as_tibble() |> select(year, tot = TT.PRI.MRCH.XD.WD) |> arrange(year)

g_tot <- ggplot(tot_marruecos, aes(x = year, y = tot)) +
  geom_line(linewidth = 1.1, color = paleta_sectores["Fosfatos"]) +
  geom_point(size = 2.2, color = paleta_sectores["Fosfatos"]) +
  labs(title = "Términos de intercambio de Marruecos",
       subtitle = "Índice de términos de intercambio de mercancías (2000=100)",
       x = "Año", y = "Índice ToT",
       caption = "Fuente: World Bank WDI. No CEPAL: no cubre países fuera de América Latina/Caribe.") +
  theme_tp1()
g_tot
ggsave(paste0(ruta_graficos, "grafico_tot.png"), g_tot, width = 10, height = 6, dpi = 300, bg = "white")

g_precios <- precios_internacionales |>
  pivot_longer(cols = c(dap_usd_mt, roca_fosforica_usd_mt), names_to = "producto", values_to = "precio") |>
  mutate(producto = recode(producto, dap_usd_mt = "DAP", roca_fosforica_usd_mt = "Roca fosfórica")) |>
  ggplot(aes(x = year, y = precio, color = producto)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  labs(title = "Precio internacional de fertilizantes", subtitle = "USD por tonelada métrica, f.o.b.",
       x = "Año", y = "USD/mt", color = NULL, caption = "Fuente: World Bank Commodity Markets (Pink Sheet).") +
  theme_tp1()
g_precios
ggsave(paste0(ruta_graficos, "grafico_precios_internacionales.png"), g_precios, width = 10, height = 6, dpi = 300, bg = "white")


#===============================================================================#
# BLOQUE 3: MODELO DE HECKSCHER-OHLIN (largo plazo)
#===============================================================================#

## --- 3.1 Justificación empírica de intensidades (brecha de valor por trabajador) ---
productividad_valor_fosfatos <- inner_join(ocp_financieros |> select(year, ingresos_mmad), ocp_empleo_serie, by = "year") |>
  mutate(valor_por_trabajador_mmad = ingresos_mmad / empleo, sector = "Fosfatos")
productividad_valor_auto <- inner_join(auto_exportaciones_oc |> select(year, total_mmad),
                                       auto_empleo_serie |> select(year, empleo), by = "year") |>
  mutate(valor_por_trabajador_mmad = total_mmad / empleo, sector = "Automotor")

brecha_valor_trabajador <- bind_rows(
  productividad_valor_fosfatos |> select(year, sector, valor_por_trabajador_mmad),
  productividad_valor_auto     |> select(year, sector, valor_por_trabajador_mmad)
) |>
  pivot_wider(names_from = sector, values_from = valor_por_trabajador_mmad) |>
  mutate(brecha_veces = Fosfatos / Automotor)
brecha_valor_trabajador

## --- 3.2 Dotación factorial relativa (PWT) ---
data("pwt10.01", package = "pwt10")
paises_benchmark <- c("Morocco", "Germany", "Spain", "France")

kl_benchmark <- pwt10.01 |> filter(country %in% paises_benchmark) |>
  mutate(kl = rnna / emp) |> select(country, year, kl, hc, labsh)

g_kl <- kl_benchmark |> filter(year >= 2000) |>
  ggplot(aes(x = year, y = kl, color = country)) +
  geom_line(linewidth = 1.1) + scale_color_manual(values = paleta_paises_ho) +
  labs(title = "Capital por trabajador (K/L)", subtitle = "Marruecos vs. Alemania, España y Francia",
       x = "Año", y = "K/L (PWT rnna/emp)", color = NULL,
       caption = "Fuente: Penn World Table 10.01 (cobertura hasta 2019).") +
  theme_tp1()
g_kl
ggsave(paste0(ruta_graficos, "grafico_kl_benchmark.png"), g_kl, width = 10, height = 6.5, dpi = 300, bg = "white")
# TODO: definir si el promedio UE es simple o ponderado (PBI/población).

g_labsh <- pwt10.01 |> filter(country == "Morocco") |>
  ggplot(aes(x = year, y = labsh)) +
  geom_line(linewidth = 1.1, color = paleta_sectores["Automotor"]) +
  labs(title = "Participación del trabajo en el ingreso — Marruecos",
       subtitle = "Insumo para testear Stolper-Samuelson de largo plazo (3d)",
       x = "Año", y = "labsh (PWT)", caption = "Fuente: Penn World Table 10.01 (cobertura hasta 2019).") +
  theme_tp1()
g_labsh
ggsave(paste0(ruta_graficos, "grafico_labsh_marruecos.png"), g_labsh, width = 10, height = 6, dpi = 300, bg = "white")

## --- 3.4 Origen del capital extranjero ---
g_ied_origen <- ggplot(ied_origen_pais_2020, aes(x = reorder(pais, stock_mmad), y = stock_mmad)) +
  geom_col(fill = paleta_sectores["Automotor"]) + coord_flip() +
  labs(title = "Stock de IED en Marruecos por país de origen", subtitle = "2020, millones de MAD",
       x = NULL, y = "Millones de MAD",
       caption = "Fuente: Office des Changes (PEG 2020) / DG Trésor / UNCTADstat.") +
  theme_tp1()
g_ied_origen
ggsave(paste0(ruta_graficos, "grafico_ied_origen.png"), g_ied_origen, width = 9, height = 6, dpi = 300, bg = "white")


#===============================================================================#
# BLOQUE 4: SÍNTESIS Y POLÍTICA
#===============================================================================#
# hcp_va_crecimiento, hcp_va_total_mmad, caisse_compensacion,
# regimen_amdie_texto y auto_inversion ya están cargados arriba. El
# excedente de VPMgL que OCP no distribuye como salario se capta como
# dividendo del Estado (ocp_financieros$dividendos_mmad) y financia en
# parte la Caisse de Compensation - ver Script 2, sección G.2, para el
# chequeo cuantitativo de que la renta implícita cabe en el EBITDA y es
# del orden de los dividendos efectivamente girados.


#===============================================================================#
# QUÉ SIGUE EN EL SCRIPT 2
#===============================================================================#
# 1. Segunda fuente de alpha (OCDE, SUT_USEVA) y su descarga.
# 2. Perímetros corregidos: OCP no es solo minería (agrega C20 química);
#    el complejo automotor no es solo ensamblaje (agrega C27 cableado).
# 3. VPMgL sobre valor agregado (corrige el sesgo de este script de
#    mezclar VA con producción bruta) - brecha final: 13,6x.
# 4. Comparación de escenarios A (HCP) vs. B (OCDE corregido) con la misma
#    función calibrar_mfe() para ambos.
# 5. Comparación Cobb-Douglas vs. Leontief - por qué NO citar L_fosfatos_eq
#    (75.565) como predicción operativa.
# 6. Convergencia de intensidades factoriales 2014-2021 (versión corregida,
#    con la retractación documentada de la versión con perímetro estrecho).
# 7. Módulo de predicción del salario (monopsonio / reparto de renta) y
#    validación contra EBITDA y dividendos de OCP.