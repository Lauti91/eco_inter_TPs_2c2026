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
# RECONCILIACIÓN DE LA FÓRMULA DE VPMgL (resuelta): convivían tres
# versiones - (a) este script, Cobb-Douglas puro sobre ingresos/producción
# física; (b) Script 2 sección E (calibrar_mfe()), que pese a estar "aguas
# abajo" de la corrección seguía usando la misma fórmula bruta que (a); y
# (c) Script 2 sección D, un punto suelto (no una curva) con
# participación_trabajo observada * VA/L. Se decidió explícitamente (no
# por default) adoptar (c) generalizada a curva completa como fórmula
# ÚNICA y definitiva: VPMgL(L) = participación laboral observada * VA(L)/L.
# Motivo: usa valor agregado (no ingresos/exportaciones brutas, que mezclan
# producción física con consumo intermedio) y usa el reparto del ingreso
# tal como surge de cuentas nacionales, no derivado como (1-alpha) teórico.
# Implementada en la función compartida calibrar_mfe_va() (sub-bloque 2.1),
# usada por la caja de asignación de este script (2.4, Escenario A - HCP) y
# por el Escenario B (OCDE) en el Script 2, que ahora reutiliza la misma
# función en vez de tener su propia fórmula paralela.
#
# No está corrido de punta a punta en esta consolidación - cada bloque ya
# corrió por separado en sesiones anteriores; revisar dependencias de
# objetos si se ejecuta de cero.
#
# RONDA 4 (agregada acá): extensión histórica 1990-2018/2021 de fosfatos
# (producción física, precios internacionales, reservas). Amplía
# fosfatos_produccion, precios_internacionales y fosfatos_reservas sin
# tocar código de cálculo aguas abajo (mismos nombres de objeto/columna).
# El sector automotor no se extiende: no existe antes de 2012
# (Renault-Tánger). Ver sección 2.6 (reservas) para la advertencia sobre
# el salto de 2010-2011.

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
                "anio", "fuente", "url", "fecha_consulta", "notas")) |>
    # Se fuerza anio a character ACÁ, hoja por hoja, antes del bind_rows().
    # Motivo: la Ronda 4 trae una fila con "1990-2007" (rango, no año
    # puntual) en la columna Año; eso hace que read_excel() infiera esa
    # columna como texto SOLO en esa hoja, mientras en las demás es
    # numérica. Sin este cast, bind_rows() rompe por tipos incompatibles
    # (double vs. character) entre hojas. El cast de vuelta a entero pasa
    # después del bind_rows (ver más abajo), ya con todas las hojas
    # unificadas.
    mutate(anio = as.character(anio))
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
  leer_hoja_investigacion(ruta_investigacion, "Ronda 3") |> mutate(ronda = 3),
  leer_hoja_investigacion(ruta_investigacion, "Ronda 4") |> mutate(ronda = 4)
) |>
  mutate(
    # La Ronda 4 (extensión histórica 1990-2018/2021) usó un nombre de
    # variable ligeramente distinto para el precio de la roca fosfórica
    # ("f.a.s. Casablanca / North Africa") que la Ronda 1 ("f.o.b. North
    # Africa"). Es el mismo precio de referencia (Pink Sheet, Banco
    # Mundial) sin superposición de años entre rondas (R4: 1990-2021: R1:
    # 2022-2024) - se normaliza acá, en el script, para que extraer_serie()
    # las trate como una sola serie continua. No se edita el Excel.
    variable = if_else(
      variable == "Precio internacional roca fosfórica (f.a.s. Casablanca / North Africa)",
      "Precio internacional roca fosfórica (f.o.b. North Africa)",
      variable
    ),
    # La fila NO ENCONTRADO de "Estados financieros ... (pre-2008)" trae
    # "1990-2007" como Año (un rango, no un año puntual) - as.integer() la
    # vuelve NA con un warning esperado; no afecta ninguna serie numérica
    # porque el valor de esa fila ya es NO ENCONTRADO.
    anio = suppressWarnings(as.integer(anio))
  )
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

# Ronda 4 buscó explícitamente estados financieros consolidados de OCP
# pre-2008 (para extender ocp_financieros/ocp_empleo_serie hacia atrás) y
# quedó documentado como NO ENCONTRADO con una justificación institucional
# sólida: OCP operaba como régie d'État / EPIC hasta la Ley 46-07 (2008) y
# no tenía obligación de publicar balances consolidados auditados bajo
# IFRS. La serie de OCP se mantiene entonces desde 2021 (no se fuerza una
# extensión sin datos reales detrás).
justificacion_ocp_pre2008 <- datos_investigacion |>
  filter(variable == "Estados financieros auditados consolidados OCP (pre-2008)") |>
  pull(notas)
justificacion_ocp_pre2008

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

# io_coeficientes ya trae, del HCP, todo lo necesario para construir el
# VPMgL sobre VALOR AGREGADO (no solo el alpha teórico de Cobb-Douglas):
# ci_produccion_va_min/max (consumo intermedio / producción) -> de ahí
# sale VA/producción; y trabajo_va_medio (participación laboral OBSERVADA
# en el VA, de cuentas nacionales, no derivada como 1-alpha). Con esto se
# puede construir el Escenario A (HCP) de la fórmula "definitiva" sin
# depender de la OCDE (que se agrega recién en el Script 2, Escenario B).
io_coeficientes <- io_coeficientes |>
  mutate(ci_produccion_va_medio = (ci_produccion_va_min + ci_produccion_va_max) / 2,
         va_sobre_produccion_medio = 1 - ci_produccion_va_medio)

va_sobre_produccion_fosfatos_A   <- io_coeficientes$va_sobre_produccion_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
va_sobre_produccion_auto_A       <- io_coeficientes$va_sobre_produccion_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]
participacion_laboral_fosfatos_A <- io_coeficientes$trabajo_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
participacion_laboral_auto_A     <- io_coeficientes$trabajo_va_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]

## --- FUNCIÓN DEFINITIVA DE VPMgL, SOBRE VALOR AGREGADO ---
# DECISIÓN METODOLÓGICA (tomada explícitamente, no por default - ver
# cabecera del script): VPMgL(L) = participación_laboral_OBSERVADA ×
# VA(L)/L, con VA(L) = (VA/Producción) × precio_bruto × Q(L). Reemplaza a
# las DOS fórmulas viejas que convivían sin resolverse: (a) la de este
# script (Cobb-Douglas puro sobre ingresos brutos, alpha teórico) y (b) la
# del Script 2 sección D vieja (participación observada, pero calculada
# como un punto suelto, no como curva). Se adopta (b) generalizada a curva
# completa porque: (1) usa VALOR AGREGADO, no ingresos/exportaciones
# brutas - evita mezclar producción física con consumo intermedio; (2)
# usa la participación laboral tal como está en cuentas nacionales, la
# misma lógica ya usada para validar el salario real de OCP en el inciso
# 2d - mantiene todo el trabajo internamente consistente.
#
# El alpha de Cobb-Douglas (y por lo tanto A, la constante de escala) se
# sigue usando SOLO para dar forma a la curva de producción FÍSICA Q(L)
# (la FPP, que no cambia) - no interviene en la conversión a valor. Son
# dos objetos distintos: alpha es un parámetro tecnológico (elasticidad
# física), participación_laboral es un dato contable observado (reparto
# del ingreso). No hace falta que coincidan.
calibrar_mfe_va <- function(alpha_f, alpha_a, share_f, share_a, va_ratio_f, va_ratio_a,
                            etiqueta, anio_ref = anio_ref_fpp) {
  L_f0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref]
  L_a0 <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref]
  L_tot <- L_f0 + L_a0
  Q_f0 <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio_ref]
  Q_a0 <- auto_produccion$unidades[auto_produccion$year == anio_ref]
  ingresos_f <- ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref]
  export_a   <- auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref]
  
  A_f <- Q_f0 / (L_f0 ^ (1 - alpha_f)); A_a <- Q_a0 / (L_a0 ^ (1 - alpha_a))
  precio_f <- ingresos_f / Q_f0; precio_a <- export_a / Q_a0  # precio bruto implícito, por unidad física
  
  grilla <- tibble(L_fosfatos = seq(1000, L_tot - 1000, length.out = 2000)) |>
    mutate(
      L_auto = L_tot - L_fosfatos,
      Q_fosfatos = A_f * L_fosfatos ^ (1 - alpha_f), Q_auto = A_a * L_auto ^ (1 - alpha_a),
      va_fosfatos = va_ratio_f * precio_f * Q_fosfatos, va_auto = va_ratio_a * precio_a * Q_auto,
      vpmgl_fosfatos = share_f * va_fosfatos / L_fosfatos,
      vpmgl_auto     = share_a * va_auto     / L_auto,
      escenario = etiqueta
    )
  
  eq <- grilla |> mutate(brecha = abs(vpmgl_fosfatos - vpmgl_auto)) |> slice_min(brecha, n = 1)
  aL_leontief <- L_f0 / Q_f0; Q_max <- max(fosfatos_produccion$produccion_mt, na.rm = TRUE)
  
  vpmgl_f_obs <- share_f * va_ratio_f * precio_f * (Q_f0 / L_f0)
  vpmgl_a_obs <- share_a * va_ratio_a * precio_a * (Q_a0 / L_a0)
  
  list(etiqueta = etiqueta, alpha_f = alpha_f, alpha_a = alpha_a,
       share_f = share_f, share_a = share_a, va_ratio_f = va_ratio_f, va_ratio_a = va_ratio_a,
       L_f0 = L_f0, L_a0 = L_a0, L_tot = L_tot, A_f = A_f, A_a = A_a, grilla = grilla,
       w_eq = mean(c(eq$vpmgl_fosfatos, eq$vpmgl_auto)), L_f_eq = eq$L_fosfatos,
       aL_leontief = aL_leontief, Q_max = Q_max, L_cap_leontief = aL_leontief * Q_max,
       vpmgl_f_obs = vpmgl_f_obs, vpmgl_a_obs = vpmgl_a_obs, brecha_obs = vpmgl_f_obs / vpmgl_a_obs)
}

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

## --- 2.4 Caja de asignación: VPMgL sobre VALOR AGREGADO (fórmula definitiva) ---
# RECONCILIACIÓN (ver nota en la cabecera del script y la función
# calibrar_mfe_va() más arriba): esto reemplaza a la vieja versión
# ingresos/producción física. Ya no conviven dos fórmulas - esta es la
# única "caja de asignación" del trabajo en todo el TP. El Escenario B
# (OCDE) se calcula en el Script 2 con la misma función.
res_A <- calibrar_mfe_va(
  alpha_fosfatos, alpha_auto,
  participacion_laboral_fosfatos_A, participacion_laboral_auto_A,
  va_sobre_produccion_fosfatos_A, va_sobre_produccion_auto_A,
  "A - HCP"
)

g_caja_asignacion <- ggplot(res_A$grilla, aes(x = L_fosfatos)) +
  geom_line(aes(y = vpmgl_fosfatos, color = "Fosfatos"), linewidth = 1.1) +
  geom_line(aes(y = vpmgl_auto, color = "Automotor"), linewidth = 1.1) +
  geom_vline(xintercept = res_A$L_f_eq, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = res_A$w_eq, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = res_A$L_f0, linetype = "dotted", color = "gray20", linewidth = 0.8) +
  annotate("point", x = res_A$L_f0, y = res_A$vpmgl_f_obs, size = 3, color = paleta_sectores["Fosfatos"]) +
  annotate("point", x = res_A$L_f0, y = res_A$vpmgl_a_obs, size = 3, color = paleta_sectores["Automotor"]) +
  scale_color_manual(values = paleta_sectores) +
  labs(title = "Caja de asignación del trabajo — MFE",
       subtitle = paste0("Equilibrio teórico: w* \u2248 ", round(res_A$w_eq, 3),
                         " en L_fosfatos \u2248 ", format(round(res_A$L_f_eq), big.mark = "."),
                         "  |  Observado ", anio_ref_fpp, ": L_fosfatos = ", format(res_A$L_f0, big.mark = ".")),
       x = "Trabajo asignado a fosfatos", y = "VPMgL (millones de MAD por trabajador/año, sobre valor agregado)", color = NULL,
       caption = paste0("Brecha en el punto observado: ", round(res_A$brecha_obs, 1),
                        "x - Escenario A (HCP). Fórmula: participación laboral observada x VA/L.")) +
  theme_tp1()
g_caja_asignacion
ggsave(paste0(ruta_graficos, "grafico_caja_asignacion.png"), g_caja_asignacion, width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.5 Shock de términos de intercambio ---
# El indicador WDI TT.PRI.MRCH.XD.WD para Marruecos NO tiene cobertura real
# hasta 1990 (se pidió start=1990 pero WDI devuelve NA para los años sin
# dato - confirmado por el aviso "Removed 16 rows" al graficar). En vez de
# sostener un subtítulo que promete una serie que no existe, se descarta
# el tramo sin dato y el subtítulo se arma dinámicamente con el primer año
# realmente disponible (no hardcodeado).
tot_marruecos_raw <- WDI(country = "MA", indicator = "TT.PRI.MRCH.XD.WD", start = 1990, end = 2025) |>
  as_tibble() |> select(year, tot = TT.PRI.MRCH.XD.WD) |> arrange(year)
tot_marruecos <- tot_marruecos_raw |> filter(!is.na(tot))
anio_min_tot <- min(tot_marruecos$year)

g_tot <- ggplot(tot_marruecos, aes(x = year, y = tot)) +
  geom_line(linewidth = 1.1, color = paleta_sectores["Fosfatos"]) +
  geom_point(size = 2.2, color = paleta_sectores["Fosfatos"]) +
  labs(title = "Términos de intercambio de Marruecos",
       subtitle = paste0("Índice de términos de intercambio de mercancías (2000=100) — cobertura real desde ", anio_min_tot),
       x = "Año", y = "Índice ToT",
       caption = paste0("Fuente: World Bank WDI (TT.PRI.MRCH.XD.WD). Sin dato antes de ", anio_min_tot,
                        " para Marruecos en este indicador - no CEPAL, no cubre países fuera de América Latina/Caribe.")) +
  theme_tp1()
g_tot
ggsave(paste0(ruta_graficos, "grafico_tot.png"), g_tot, width = 10, height = 6, dpi = 300, bg = "white")

g_precios <- precios_internacionales |>
  pivot_longer(cols = c(dap_usd_mt, roca_fosforica_usd_mt), names_to = "producto", values_to = "precio") |>
  mutate(producto = recode(producto, dap_usd_mt = "DAP", roca_fosforica_usd_mt = "Roca fosfórica")) |>
  ggplot(aes(x = year, y = precio, color = producto)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  labs(title = "Precio internacional de fertilizantes",
       subtitle = "USD por tonelada métrica, f.o.b. — serie ampliada desde 1990 (Ronda 4)",
       x = "Año", y = "USD/mt", color = NULL,
       caption = "Fuente: World Bank Commodity Markets (Pink Sheet). Shock de 2007-2008: la roca fosfórica se multiplicó por 8x.") +
  theme_tp1()
g_precios
ggsave(paste0(ruta_graficos, "grafico_precios_internacionales.png"), g_precios, width = 10, height = 6, dpi = 300, bg = "white")

## --- 2.6 Reservas de roca fosfórica: reclasificación geológica, no descubrimiento ---
# El salto de 5,7 Gt a 50 Gt en 2010-2011 no es un shock de dotación
# factorial real (no se "descubrió" fosfato nuevo): fue una reclasificación
# de recursos marginales a reservas económicamente explotables (auditoría
# IFDC 2010, incorporada por USGS en 2011), habilitada por el shock de
# precios de 2007-2008 y la tecnología de flotación. Se deja como
# advertencia explícita: si en algún punto del TP se interpreta esta serie
# como una expansión de dotación factorial (al estilo Rybczynski), hay que
# aclarar que es un artefacto de clasificación, no un cambio físico.
fosfatos_reservas
# Ni el step ni la escala log arreglan el problema de fondo: esto no es
# una serie temporal (no hay una "trayectoria" entre 1995 y 2025), son dos
# vintages de estimación oficial separados por una reclasificación
# puntual. Forzarlo a un gráfico de línea/tiempo es engañoso aunque sea
# técnicamente preciso. Se resume como lo que es: un antes/después, con
# el quiebre agrupado automáticamente (no se hardcodea el año del corte -
# se detecta como el punto donde el valor deja de ser el mínimo de la
# serie).
reservas_eras <- fosfatos_reservas |>
  arrange(year, reservas_mt) |>
  mutate(
    reservas_gt = reservas_mt / 1e6,
    era = if_else(reservas_mt == min(reservas_mt), "pre", "post")
  ) |>
  group_by(era) |>
  summarise(
    gt = first(reservas_gt),
    anio_min = min(year), anio_max = max(year),
    .groups = "drop"
  ) |>
  mutate(
    etiqueta_periodo = paste0(anio_min, "-", anio_max),
    etiqueta_valor = paste0(round(gt, 1), " Gt"),
    era = factor(era, levels = c("pre", "post"))
  )

variacion_pct_reservas <- with(reservas_eras,
                               round((gt[era == "post"] / gt[era == "pre"] - 1) * 100))

g_reservas <- ggplot(reservas_eras, aes(x = etiqueta_periodo, y = gt)) +
  geom_col(fill = paleta_sectores["Fosfatos"], width = 0.55) +
  geom_text(aes(label = etiqueta_valor), vjust = -0.6, size = 4.2, fontface = "bold") +
  annotate("segment", x = 1, xend = 2,
           y = max(reservas_eras$gt) * 1.15, yend = max(reservas_eras$gt) * 1.15,
           arrow = arrow(length = unit(0.2, "cm")), color = "gray40") +
  annotate("text", x = 1.5, y = max(reservas_eras$gt) * 1.25,
           label = paste0("+", variacion_pct_reservas, "% (reclasificación IFDC/USGS, no descubrimiento físico)"),
           size = 3.2, color = "gray30") +
  scale_y_continuous(limits = c(0, max(reservas_eras$gt) * 1.4)) +
  labs(title = "Reservas de roca fosfórica reportadas — Marruecos",
       subtitle = "Dos vintages de estimación oficial, no una trayectoria continua",
       x = NULL, y = "Reservas reportadas (Gt)",
       caption = "Fuente: USGS Mineral Commodity Summaries; IFDC (Van Kauwenbergh, 2010).") +
  theme_tp1()
g_reservas
ggsave(paste0(ruta_graficos, "grafico_reservas_fosfatos.png"), g_reservas, width = 10, height = 6, dpi = 300, bg = "white")


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
  mutate(kl = rnna / emp) |> select(country, year, kl, hc, labsh) |>
  arrange(country, year)

g_kl <- kl_benchmark |>
  ggplot(aes(x = year, y = kl, color = country)) +
  geom_line(linewidth = 1.1) + scale_color_manual(values = paleta_paises_ho) +
  labs(title = "Capital por trabajador (K/L)",
       subtitle = "Marruecos vs. Alemania, España y Francia — serie completa PWT",
       x = "Año", y = "K/L (PWT rnna/emp)", color = NULL,
       caption = "Fuente: Penn World Table 10.01 (1950-2019, cobertura completa del paquete).") +
  theme_tp1()
g_kl
ggsave(paste0(ruta_graficos, "grafico_kl_benchmark.png"), g_kl, width = 10, height = 6.5, dpi = 300, bg = "white")
# TODO: definir si el promedio UE es simple o ponderado (PBI/población).
# Nota: rnna (capital) SÍ tiene variación año a año genuina en todo el
# rango 1950-2019 para los 4 países (chequeado) - a diferencia de labsh
# (ver más abajo), la extensión de K/L a la serie completa es válida tal
# cual.

## --- 3.3 labsh: PWT imputa un valor "plano" para países sin datos
## desagregados de renta factorial - hay que detectarlo y descartarlo ---
# HALLAZGO (al extender la serie a 1950-2019): para varios países, PWT
# repite un mismo valor de labsh año tras año durante décadas al principio
# de la serie - no es que "no varió", es un placeholder que la propia PWT
# usa cuando no hay datos desagregados de compensación laboral. Se detecta
# automáticamente (no se asume ningún año a mano) buscando, país por país,
# el primer año en que labsh deja de ser idéntico al valor inicial.
detectar_primer_anio_real <- function(anios, valores) {
  valor_inicial <- valores[1]
  distinto <- abs(valores - valor_inicial) > 1e-6
  if (!any(distinto)) return(anios[1])  # sin imputación detectable
  anios[which(distinto)[1]]
}

anios_labsh_real <- kl_benchmark |>
  group_by(country) |>
  summarise(anio_real_labsh = detectar_primer_anio_real(year, labsh), .groups = "drop")
anios_labsh_real
# Resultado (chequeado): Marruecos recién tiene labsh real desde 1999
# (plano en 0,5077 desde 1950); Alemania desde 1992; España desde 1996;
# Francia no tiene el problema (variación real desde 1950). O sea: NO es
# un problema específico de Marruecos, pero SÍ invalida cualquier
# comparación (gráfico o regresión) que use la serie completa 1950-2019
# para Alemania/España tal como venía calibrado antes de esta ronda -
# antes de esos años, la "variación" no es un dato real. Si en algún
# momento se corrió una regresión de labsh contra el tiempo con el rango
# completo para los benchmarks, hay que rehacerla restringida a partir del
# año real de cada país (o, para comparar los 4 países en una ventana
# común, desde 1999 en adelante).

kl_benchmark <- kl_benchmark |>
  left_join(anios_labsh_real, by = "country") |>
  mutate(labsh_real = if_else(year >= anio_real_labsh, labsh, NA_real_))

g_labsh <- kl_benchmark |>
  filter(country == "Morocco", !is.na(labsh_real)) |>
  ggplot(aes(x = year, y = labsh_real)) +
  geom_line(linewidth = 1.1, color = paleta_sectores["Automotor"]) +
  labs(title = "Participación del trabajo en el ingreso — Marruecos",
       subtitle = paste0("Insumo para testear Stolper-Samuelson de largo plazo (3d) — ",
                         "serie real PWT desde ",
                         anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Morocco"]),
       x = "Año", y = "labsh (PWT)",
       caption = paste0("Fuente: Penn World Table 10.01. Se excluye el tramo previo (",
                        min(pwt10.01$year[pwt10.01$country == "Morocco"]), "-",
                        anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Morocco"] - 1,
                        "): PWT repite un valor plano imputado sin dato real detrás.")) +
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
#    mezclar VA con producción bruta) - brecha final: ver
#    comparacion_escenarios$brecha_vpmgl_observada (Escenarios A y B).
# 4. Comparación de escenarios A (HCP) vs. B (OCDE corregido) con la misma
#    función calibrar_mfe_va() para ambos.
# 5. Comparación Cobb-Douglas vs. Leontief - por qué NO citar L_fosfatos_eq
#    (127.728 en el Escenario A, corrido más reciente) como predicción
#    operativa.
# 6. Convergencia de intensidades factoriales 2014-2021 (versión corregida,
#    con la retractación documentada de la versión con perímetro estrecho).
# 7. Módulo de predicción del salario (monopsonio / reparto de renta) y
#    validación contra EBITDA y dividendos de OCP.