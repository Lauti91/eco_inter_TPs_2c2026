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
# FÓRMULA DE VPMgL: historia de la reconciliación y de su corrección.
#
# Primero convivían TRES versiones sin resolver: (a) este script,
# Cobb-Douglas sobre ingresos/producción física; (b) Script 2 sección E,
# que pese a estar "aguas abajo" de la corrección usaba la misma fórmula
# bruta que (a); y (c) Script 2 sección D, un punto suelto (no una curva)
# con participación_trabajo observada * VA/L. Se unificó todo en una sola
# función compartida, calibrar_mfe_va() (sub-bloque 2.1), adoptando la
# forma de (c): participación laboral observada * VA(L)/L.
#
# UNA AUDITORÍA POSTERIOR encontró que esa elección mezclaba dos decisiones
# independientes y que solo una de las dos era correcta:
#
#   - Calcular sobre VALOR AGREGADO y no sobre ingresos brutos: CORRECTO,
#     se mantiene. Era el error de fondo de (a) y (b).
#
#   - Usar la participación laboral observada como multiplicador en vez de
#     (1-alpha): INCORRECTO, se revierte. Motivos: (i) no es el producto
#     marginal de la Cobb-Douglas que este mismo script usa para la FPP -
#     si Q(L)=A*L^(1-alpha), la derivada del valor es (1-alpha)*VA/L por
#     construcción; (ii) volvía ALGEBRAICAMENTE CIRCULAR el test salarial
#     del inciso 2d: w/VPMgL colapsaba a (participación real de OCP) /
#     (participación supuesta), o sea no testeaba nada sobre productos
#     marginales. Se verificó numéricamente: el 1,31 que reportaba esa
#     versión era exactamente 0,2103/0,1600.
#
# FÓRMULA VIGENTE: VPMgL(L) = (1-alpha) * VA(L)/L, con VA(L) =
# (VA/Producción) * precio_bruto * Q(L). La participación laboral observada
# se conserva como BENCHMARK independiente contra el cual contrastar
# (1-alpha) - ver parametros_escenario_A y la Sección H del Script 2.
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
# Marruecos (#c1272d) y España (#AA151B) estaban a distancia RGB ~34: en el
# gráfico de K/L las dos líneas salían del mismo rojo y no se distinguían
# en la leyenda. Se conserva el rojo para Marruecos (país foco) y se pasa a
# España al amarillo/oro de su bandera, moviendo a Alemania al negro - las
# cuatro siguen siendo colores de bandera, pero ahora separables.
paleta_paises_ho <- c("Morocco" = "#c1272d", "Germany" = "#2b2b2b",
                      "Spain" = "#E8A33D", "France" = "#0055A4")

sectores_mfe <- c("272", "562", "781", "784")
nombres_sectores_mfe <- c(
  "272" = "Fertilizantes crudos", "562" = "Fertilizantes manufacturados",
  "781" = "Autos de pasajeros",   "784" = "Autopartes"
)

# Formateo de miles al estilo local (1.234.567). format(x, big.mark = ".")
# a secas dispara el aviso "'big.mark' y 'decimal.mark' son ambos '.'"
# porque R no puede distinguir cuál de los dos puntos es cuál; fijando
# decimal.mark explícitamente el aviso desaparece y el resultado es el mismo.
fmt_mil <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)


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

# AUDITORÍA: estructura completa de la base unificada (4 rondas) - para
# verificar tipos de columna, NAs y volumen de filas por ronda antes de
# seguir. glimpse() en vez de print() porque son ~10 columnas y conviene
# ver el tipo de cada una, no solo las primeras filas.
glimpse(datos_investigacion)
datos_investigacion |> count(ronda, bloque)

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
ocp_financieros  # AUDITORÍA

ocp_empleo_serie <- extraer_serie("Empleo directo OCP Group (Fosfatos)") |> rename(empleo = valor)
ocp_empleo_serie  # AUDITORÍA
ocp_empleo_consolidado_alt <- 20000  # chequeo de robustez, no serie principal

# AUDITORÍA DE PLAUSIBILIDAD: el empleo de OCP es el denominador de todo el
# Bloque 2, así que conviene marcar valores que parezcan redondeados de
# fuente periodística en vez de headcount reportado. Se señalan los años
# cuyo valor es múltiplo exacto de 1.000 y/o que se repiten idénticos en
# años no consecutivos (2019 y 2024 traen los dos el mismo 20.000, que es
# además el valor de ocp_empleo_consolidado_alt - sospechoso de ser un
# "~20 mil" de prensa y no un dato de plantilla).
ocp_empleo_serie |>
  filter(!is.na(empleo)) |>
  mutate(redondo_a_mil = empleo %% 1000 == 0,
         valor_repetido = duplicated(empleo) | duplicated(empleo, fromLast = TRUE),
         revisar_fuente = redondo_a_mil | valor_repetido)

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
unidad_reservas  # AUDITORÍA: confirmar unidad tal cual figura en la fuente
# Confirmado: 5e+07 miles de toneladas métricas = 50 Gt (~70% mundial).

fosfatos_produccion <- extraer_serie("Producción física de roca fosfórica") |> rename(produccion_mt = valor)
fosfatos_produccion  # AUDITORÍA

## --- Automotor: producción física, empleo, exportaciones ---
auto_produccion <- extraer_serie("Producción de vehículos de motor") |> rename(unidades = valor)
auto_produccion  # AUDITORÍA

auto_empleo_serie <- bind_rows(
  datos_investigacion |>
    filter(variable == "Empleo total sector automotor") |>
    transmute(year = anio, empleo = valor, nota = "declarado/observado"),
  datos_investigacion |>
    filter(variable == "Empleo total sector automotor (meta proyectada)") |>
    transmute(year = anio, empleo = valor, nota = "meta proyectada, no observada")
) |> arrange(year)
auto_empleo_serie  # AUDITORÍA: confirmar qué años son "declarado/observado" vs. "meta proyectada"

# SEPARACIÓN OBSERVADO / PROYECTADO (endurecido tras auditoría).
# auto_empleo_serie mezcla a propósito los dos tipos de fila, porque para
# describir el sector conviene ver la meta declarada al lado de los datos.
# Pero el MODELO no puede usar una meta como si fuera un dato: se define
# acá una serie separada, solo con lo efectivamente observado, y es la que
# usan la FPP, la caja de asignación y las productividades. Hoy ningún año
# tiene las dos filas a la vez (2025 solo trae la meta), pero si una ronda
# futura agregara una proyección para un año que ya tiene dato observado,
# sin este filtro el modelo tomaría un vector de largo 2 y fallaría - o,
# peor, usaría la proyección sin avisar.
auto_empleo_observado <- auto_empleo_serie |> filter(nota == "declarado/observado")
stopifnot("Hay años duplicados en el empleo automotor observado" =
            !any(duplicated(auto_empleo_observado$year)))
auto_empleo_observado  # AUDITORÍA: esta es la serie que entra al modelo

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
auto_exportaciones_oc  # AUDITORÍA

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
io_coeficientes  # AUDITORÍA (versión HCP base, antes de agregar ci_produccion_va_medio en Bloque 2)

## --- Salarios (CNSS) ---
salario_promedio_general <- extraer_serie("Salario medio mensual declarado - Total cotizantes régimen general") |>
  rename(salario_mad_mes = valor)
salario_promedio_general  # AUDITORÍA
salarios_cnss_sector_2020 <- datos_investigacion |>
  filter(str_starts(variable, "Salario medio declarado -"), anio == 2020) |>
  transmute(sector = str_remove(variable, "^Salario medio declarado - "), salario_mad_mes = valor)
salarios_cnss_sector_2020  # AUDITORÍA
# Solo 4 filas reales (Industrie, Agriculture, Transports, Financiero) - no
# 7 como en un intento manual anterior.

## --- IED: stock total, origen, inversión automotor ---
ied_stock_total <- extraer_serie("Stock de IED entrante en Marruecos (Inward FDI Stock)") |> rename(stock_musd = valor)
ied_stock_total  # AUDITORÍA

ied_origen_pais_2020 <- datos_investigacion |>
  filter(str_starts(variable, "Stock de IED entrante originario de"), anio == 2020) |>
  transmute(pais = str_remove(variable, "^Stock de IED entrante originario de "), stock_mmad = valor_medio)
ied_origen_pais_2020  # AUDITORÍA

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
auto_inversion  # AUDITORÍA

ied_manufactura <- extraer_serie("Stock de IED en industria manufacturera") |> rename(stock_mmad = valor)
ied_manufactura  # AUDITORÍA
ied_manufactura_flujo <- datos_investigacion |>
  filter(variable == "Flujo neto anual de IED manufacturera") |> select(anio, valor_min, valor_max)
ied_manufactura_flujo  # AUDITORÍA

## --- HCP: valor agregado sectorial / Caisse de Compensation / AMDIE ---
hcp_va_crecimiento <- extraer_serie("Crecimiento volumen VA - Minería (B00)") |> rename(mineria_pct = valor)
hcp_va_crecimiento <- full_join(hcp_va_crecimiento,
                                extraer_serie("Crecimiento volumen VA - Manufactura (C00)") |> rename(manufactura_pct = valor),
                                by = "year") |> arrange(year)
hcp_va_crecimiento  # AUDITORÍA

hcp_va_total_mmad <- extraer_serie("Valor agregado total economía (precios básicos)") |> rename(total = valor)
hcp_va_total_mmad  # AUDITORÍA

caisse_compensacion <- bind_rows(
  datos_investigacion |>
    filter(variable == "Carga global de compensación (Caisse de Compensation)") |>
    transmute(year = anio, carga_mmad = valor, nota = NA_character_),
  datos_investigacion |>
    filter(variable == "Carga global de compensación presupuestada") |>
    transmute(year = anio, carga_mmad = valor, nota = "presupuestado, Loi de Finances")
) |> arrange(year)
caisse_compensacion  # AUDITORÍA

regimen_amdie_texto <- datos_investigacion |>
  filter(variable == "Régimen de zonas francas e incentivos fiscales a la inversión automotriz") |>
  pull(valor_raw)
regimen_amdie_texto  # AUDITORÍA

## --- Precios internacionales (Pink Sheet) ---
precios_internacionales <- extraer_serie("Precio internacional DAP (f.o.b. US Gulf)") |> rename(dap_usd_mt = valor)
precios_internacionales <- full_join(precios_internacionales,
                                     extraer_serie("Precio internacional roca fosfórica (f.o.b. North Africa)") |> rename(roca_fosforica_usd_mt = valor),
                                     by = "year")
precios_internacionales <- full_join(precios_internacionales,
                                     extraer_serie("Precio internacional gas natural (Europe TTF)") |> rename(gas_natural_usd_mmbtu = valor),
                                     by = "year") |> arrange(year)
precios_internacionales  # AUDITORÍA


#===============================================================================#
# BLOQUE 1: PUNTO DE PARTIDA
#===============================================================================#

vcrn_mfe <- vcr_mar |>
  filter(cuci %in% sectores_mfe) |>
  mutate(sector_desc = nombres_sectores_mfe[cuci],
         grupo = if_else(cuci %in% c("272", "562"), "Fosfatos", "Automotor"))
vcrn_mfe  # AUDITORÍA

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
exportaciones_mfe_wits  # AUDITORÍA
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
io_coeficientes  # AUDITORÍA (versión completa, con VA/producción ya agregado)

va_sobre_produccion_fosfatos_A   <- io_coeficientes$va_sobre_produccion_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
va_sobre_produccion_auto_A       <- io_coeficientes$va_sobre_produccion_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]
participacion_laboral_fosfatos_A <- io_coeficientes$trabajo_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
participacion_laboral_auto_A     <- io_coeficientes$trabajo_va_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]

# AUDITORÍA: todos los parámetros del Escenario A en una sola tabla, para
# verificar de un vistazo que ningún valor esté fuera de [0,1] y que
# capital_va_medio + trabajo_va_medio no supere 1 (el residual son
# impuestos netos a la producción, que HCP no reparte entre capital y
# trabajo - un residual negativo o >15-20% señalaría un error de fuente).
#
# La columna elasticidad_trabajo = 1 - alpha es la que efectivamente
# multiplica al VA medio para obtener el VPMgL (ver calibrar_mfe_va()).
# Bajo Cobb-Douglas con mercados de factores competitivos, esa elasticidad
# debería COINCIDIR con la participación laboral observada; el cociente
# entre ambas es, por lo tanto, un diagnóstico y no un supuesto: mide
# cuánto se aparta el reparto observado del que predice la tecnología
# calibrada. Se reporta acá, a la vista, en vez de quedar enterrado dentro
# de la fórmula del VPMgL (que es donde estaba antes de la auditoría, y
# donde volvía circular el test salarial del inciso 2d).
parametros_escenario_A <- tibble(
  rama = c("Fosfatos", "Automotor"),
  alpha_capital = c(alpha_fosfatos, alpha_auto),
  participacion_laboral = c(participacion_laboral_fosfatos_A, participacion_laboral_auto_A),
  va_sobre_produccion = c(va_sobre_produccion_fosfatos_A, va_sobre_produccion_auto_A)
) |>
  mutate(
    elasticidad_trabajo = 1 - alpha_capital,
    residual_capital_mas_trabajo = 1 - (alpha_capital + participacion_laboral),
    share_sobre_elasticidad = participacion_laboral / elasticidad_trabajo
  )
parametros_escenario_A
write_csv(parametros_escenario_A, paste0(ruta_tablas, "parametros_escenario_A.csv"))
# Lectura: share_sobre_elasticidad cercano a 1 => el reparto observado del
# ingreso es consistente con una Cobb-Douglas competitiva con ese alpha.
# Lejos de 1 => o el alpha no describe bien la tecnología, o los mercados
# de factores no son competitivos, o ambas. Es una pregunta empírica del
# trabajo, no algo que se pueda imponer por construcción.

## --- FUNCIÓN DEFINITIVA DE VPMgL, SOBRE VALOR AGREGADO ---
# DECISIÓN METODOLÓGICA (corregida tras auditoría - ver cabecera). La
# fórmula es:
#
#     VPMgL(L) = d[VA(L)]/dL = (1 - alpha) * VA(L)/L
#     con VA(L) = (VA/Producción) * precio_bruto * Q(L),  Q(L) = A * L^(1-alpha)
#
# DOS COSAS SE DECIDIERON POR SEPARADO, y conviene no volver a mezclarlas:
#
# (1) El VALOR sobre el que se calcula es el VALOR AGREGADO, no los
#     ingresos/exportaciones brutas. Esto se mantiene: era el error de
#     fondo de las versiones viejas (mezclaban producción física con
#     consumo intermedio, y el ratio VA/producción difiere mucho entre los
#     dos sectores - ~0,60 en fosfatos vs. ~0,27 en automotor -, así que el
#     sesgo no se cancelaba).
#
# (2) El MULTIPLICADOR que convierte VA medio en VA marginal es (1-alpha),
#     NO la participación laboral observada. Una versión anterior usaba la
#     participación observada acá; eso tenía dos problemas:
#       - no era el producto marginal de la Cobb-Douglas que este mismo
#         script usa para dibujar la FPP: si Q(L) = A*L^(1-alpha), entonces
#         d(VA)/dL = (1-alpha)*VA/L por construcción, con (1-alpha) y no
#         con otro coeficiente;
#       - volvía CIRCULAR el test del inciso 2d. Al comparar el salario
#         observado contra un "VPMgL" construido con una participación
#         laboral supuesta, el cociente w/VPMgL colapsaba algebraicamente a
#         (participación laboral real de OCP) / (participación supuesta) -
#         es decir, no medía si el salario iguala al producto marginal,
#         solo medía la discrepancia entre dos participaciones. Con la
#         fórmula corregida el test recupera contenido: (1-alpha) viene del
#         coeficiente de capital de HCP/OCDE (una fuente) y la
#         participación laboral efectiva de OCP sale de su propia masa
#         salarial (otra fuente independiente).
#
# La participación laboral observada NO desaparece: pasa a ser el
# BENCHMARK contra el cual se contrasta (1-alpha) - ver la columna
# share_sobre_elasticidad de parametros_escenario_A (más arriba) y el
# objeto test_reparto_ingreso en la Sección H del Script 2.
# Bajo Cobb-Douglas con mercados competitivos ambas deberían coincidir;
# cuánto difieren es justamente lo que el trabajo quiere medir, así que no
# puede suponerse de entrada.
calibrar_mfe_va <- function(alpha_f, alpha_a, share_f, share_a, va_ratio_f, va_ratio_a,
                            etiqueta, anio_ref = anio_ref_fpp) {
  L_f0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref]
  L_a0 <- auto_empleo_observado$empleo[auto_empleo_observado$year == anio_ref]
  L_tot <- L_f0 + L_a0
  Q_f0 <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio_ref]
  Q_a0 <- auto_produccion$unidades[auto_produccion$year == anio_ref]
  ingresos_f <- ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref]
  export_a   <- auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref]
  
  A_f <- Q_f0 / (L_f0 ^ (1 - alpha_f)); A_a <- Q_a0 / (L_a0 ^ (1 - alpha_a))
  precio_f <- ingresos_f / Q_f0; precio_a <- export_a / Q_a0  # precio bruto implícito, por unidad física
  
  # Elasticidad producto-trabajo de cada sector = multiplicador del VA medio
  elast_f <- 1 - alpha_f; elast_a <- 1 - alpha_a
  
  grilla <- tibble(L_fosfatos = seq(1000, L_tot - 1000, length.out = 2000)) |>
    mutate(
      L_auto = L_tot - L_fosfatos,
      Q_fosfatos = A_f * L_fosfatos ^ (1 - alpha_f), Q_auto = A_a * L_auto ^ (1 - alpha_a),
      va_fosfatos = va_ratio_f * precio_f * Q_fosfatos, va_auto = va_ratio_a * precio_a * Q_auto,
      # VPMgL = derivada del valor agregado respecto del trabajo
      vpmgl_fosfatos = elast_f * va_fosfatos / L_fosfatos,
      vpmgl_auto     = elast_a * va_auto     / L_auto,
      # Referencia contable, solo para inspección: cuánto daría usar la
      # participación laboral observada en vez de la elasticidad. NO
      # interviene en el equilibrio ni en ningún gráfico - queda en la
      # grilla para poder comparar las dos convenciones a mano si hace
      # falta, y para dejar constancia de cuál era la fórmula anterior.
      w_implicito_fosfatos = share_f * va_fosfatos / L_fosfatos,
      w_implicito_auto     = share_a * va_auto     / L_auto,
      escenario = etiqueta
    )
  
  eq <- grilla |> mutate(brecha = abs(vpmgl_fosfatos - vpmgl_auto)) |> slice_min(brecha, n = 1)
  aL_leontief <- L_f0 / Q_f0
  # Q_max es el pico de producción sobre TODA la serie disponible de
  # fosfatos, que desde la Ronda 4 va de 1990 a 2025 (antes 2019-2025). Se
  # deja explícito el rango para que "capacidad pico" no cambie de
  # significado en silencio si la serie se vuelve a extender.
  Q_max <- max(fosfatos_produccion$produccion_mt, na.rm = TRUE)
  anio_Q_max <- fosfatos_produccion$year[which.max(fosfatos_produccion$produccion_mt)]
  rango_Q <- paste0(min(fosfatos_produccion$year), "-", max(fosfatos_produccion$year))
  
  vpmgl_f_obs <- elast_f * va_ratio_f * precio_f * (Q_f0 / L_f0)
  vpmgl_a_obs <- elast_a * va_ratio_a * precio_a * (Q_a0 / L_a0)
  
  list(etiqueta = etiqueta, alpha_f = alpha_f, alpha_a = alpha_a,
       elast_f = elast_f, elast_a = elast_a,
       share_f = share_f, share_a = share_a, va_ratio_f = va_ratio_f, va_ratio_a = va_ratio_a,
       L_f0 = L_f0, L_a0 = L_a0, L_tot = L_tot, A_f = A_f, A_a = A_a, grilla = grilla,
       anio_ref = anio_ref, ingresos_f = ingresos_f, export_a = export_a,
       va_f_obs = va_ratio_f * ingresos_f, va_a_obs = va_ratio_a * export_a,
       w_eq = mean(c(eq$vpmgl_fosfatos, eq$vpmgl_auto)), L_f_eq = eq$L_fosfatos,
       aL_leontief = aL_leontief, Q_max = Q_max, anio_Q_max = anio_Q_max, rango_Q = rango_Q,
       L_cap_leontief = aL_leontief * Q_max,
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
vpmgl_fosfatos  # AUDITORÍA
vpmgl_automotor <- calcular_vpmgl_cobb_douglas(
  auto_produccion |> rename(unidades_producidas = unidades),
  auto_empleo_observado |> select(year, empleo), alpha_auto
)
vpmgl_automotor  # AUDITORÍA

# CORRECCIÓN (auditoría): la versión anterior graficaba las dos series de
# productividad media en un mismo eje log. Eso era engañoso porque están en
# UNIDADES DISTINTAS - fosfatos en miles de toneladas por trabajador (~1,9)
# y automotor en vehículos por trabajador (~2,3) -, así que sus NIVELES no
# son comparables y el cruce que aparecía en 2022 era un artefacto de las
# unidades, no un hecho económico. Se pasa a números índice con base 100 en
# el primer año de cada serie: eso sí es comparable, porque compara
# EVOLUCIONES y no niveles. Los niveles en unidades originales quedan a la
# vista en las tablas vpmgl_fosfatos / vpmgl_automotor de más arriba.
productividad_indexada <- bind_rows(
  vpmgl_fosfatos  |> mutate(sector = "Fosfatos"),
  vpmgl_automotor |> mutate(sector = "Automotor")
) |>
  filter(!is.na(producto_medio_trabajo)) |>
  group_by(sector) |>
  arrange(year, .by_group = TRUE) |>
  mutate(anio_base = first(year),
         indice = 100 * producto_medio_trabajo / first(producto_medio_trabajo)) |>
  ungroup()
productividad_indexada  # AUDITORÍA

g_productividad <- ggplot(productividad_indexada, aes(x = year, y = indice, color = sector)) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray50") +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  scale_color_manual(values = paleta_sectores) +
  labs(title = "Productividad media del trabajo, evolución comparada",
       subtitle = paste0("Producción física por trabajador, índice base 100 = primer año de cada serie (",
                         paste(sort(unique(productividad_indexada$anio_base)), collapse = " y "), ")"),
       x = "Año", y = "Índice (base 100)", color = NULL,
       caption = paste0("Fosfatos en miles de toneladas/trabajador, automotor en vehículos/trabajador: ",
                        "los NIVELES no son comparables entre sectores (unidades físicas distintas), ",
                        "por eso se comparan las evoluciones y no los valores absolutos.")) +
  theme_tp1()
g_productividad
ggsave(paste0(ruta_graficos, "grafico_productividad_mfe.png"), g_productividad, width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.3 FPP real (plano Q_fosfatos x Q_automotor) ---
# ELECCIÓN DEL AÑO DE REFERENCIA (justificada en código tras auditoría).
# Antes 2023 estaba puesto a mano, sin explicación, y resultó ser el año
# con el MÍNIMO de empleo de OCP (17.000, el más bajo de toda la serie) y
# el MÍNIMO de producción física del período reciente (33.000). Eso no es
# neutral: infla la brecha de VPMgL y afloja el techo Leontief. Ahora el
# año no se fija a mano sino que se DERIVA con un criterio explícito - el
# año MÁS RECIENTE en el que están disponibles TODAS las series que el
# modelo necesita, incluyendo el salario efectivo de OCP, que es el que
# permite el contraste del inciso 2d - y se acompaña de una tabla de
# sensibilidad sobre todos los años viables (sub-bloque 2.4b). Con los
# datos actuales ese criterio vuelve a dar 2023, pero ahora por una razón
# auditable y no por un número escrito a mano.
series_requeridas <- list(
  empleo_ocp       = ocp_empleo_serie |> filter(!is.na(empleo)) |> pull(year),
  empleo_auto      = auto_empleo_observado |> filter(!is.na(empleo)) |> pull(year),
  produccion_fosf  = fosfatos_produccion |> filter(!is.na(produccion_mt)) |> pull(year),
  produccion_auto  = auto_produccion |> filter(!is.na(unidades)) |> pull(year),
  ingresos_ocp     = ocp_financieros |> filter(!is.na(ingresos_mmad)) |> pull(year),
  export_auto      = auto_exportaciones_oc |> filter(!is.na(total_mmad)) |> pull(year)
)
anios_viables_modelo <- reduce(series_requeridas, intersect) |> sort()
anios_viables_modelo  # AUDITORÍA: años con TODAS las series del modelo disponibles

anios_con_salario_ocp <- datos_investigacion |>
  filter(variable == "Salario medio efectivo anual (Plantilla directa) - OCP Group",
         !is.na(valor)) |>
  pull(anio) |> sort()
anios_con_salario_ocp  # AUDITORÍA

anios_candidatos <- intersect(anios_viables_modelo, anios_con_salario_ocp)
stopifnot("No hay ningún año con todas las series del modelo y salario de OCP" =
            length(anios_candidatos) > 0)
anio_ref_fpp <- max(anios_candidatos)
anio_ref_fpp  # AUDITORÍA: último año con series completas + salario OCP observado

L_fosfatos_0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref_fpp]
L_auto_0     <- auto_empleo_observado$empleo[auto_empleo_observado$year == anio_ref_fpp]
L_total      <- L_fosfatos_0 + L_auto_0

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
fpp_observado  # AUDITORÍA

g_fpp <- ggplot() +
  geom_path(data = fpp_grilla, aes(x = Q_fosfatos, y = Q_auto), linewidth = 1.1, color = "gray30") +
  geom_point(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, color = factor(year)), size = 3) +
  geom_text(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, label = year), vjust = -1, size = 3) +
  labs(title = "FPP empírica de Marruecos: fosfatos vs. automotor",
       subtitle = paste0("Curva calibrada con Cobb-Douglas sobre L = ", fmt_mil(L_total),
                         " trabajadores (", anio_ref_fpp, ")"),
       x = "Producción de fosfatos (miles de toneladas)", y = "Producción automotriz (unidades)",
       color = "Año observado") +
  theme_tp1()
g_fpp
ggsave(paste0(ruta_graficos, "grafico_fpp_real.png"), g_fpp, width = 10, height = 7, dpi = 300, bg = "white")

anios_fpp_familia <- intersect(
  intersect(ocp_empleo_serie$year, auto_empleo_observado$year),
  intersect(fosfatos_produccion$year, auto_produccion$year)
)
fpp_familia <- map_dfr(anios_fpp_familia, function(anio) {
  L_f <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio]
  L_a <- auto_empleo_observado$empleo[auto_empleo_observado$year == anio]
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

## --- 2.4 Caja de asignación: VPMgL sobre VALOR AGREGADO ---
# Una sola "caja de asignación" en todo el TP, con la fórmula corregida
# VPMgL = (1-alpha)*VA(L)/L (ver cabecera del script y calibrar_mfe_va()).
# El Escenario B (OCDE) se calcula en el Script 2 con la misma función.
res_A <- calibrar_mfe_va(
  alpha_fosfatos, alpha_auto,
  participacion_laboral_fosfatos_A, participacion_laboral_auto_A,
  va_sobre_produccion_fosfatos_A, va_sobre_produccion_auto_A,
  "A - HCP"
)

# AUDITORÍA: resumen escalar de res_A (todo menos la grilla de 2000 filas),
# para tener los números clave de la caja de asignación en una sola tabla
# en vez de tener que ir componente por componente con res_A$....
res_A_resumen <- as_tibble(res_A[setdiff(names(res_A), "grilla")])
res_A_resumen
write_csv(res_A_resumen, paste0(ruta_tablas, "res_A_resumen.csv"))

# CHEQUEO DE REGRESIÓN (auditoría del código, no del dato): calibrar_mfe_va()
# construye vpmgl_fosfatos = k_f * L_fosfatos^(-alpha_f) y vpmgl_auto =
# k_a * L_auto^(-alpha_a) para constantes k_f, k_a - una forma cerrada. Si
# la implementación es correcta, una regresión log-log de vpmgl contra L
# sobre la propia grilla calculada DEBE recuperar la pendiente -alpha
# exacta (R² prácticamente 1, salvo error de precisión de punto flotante).
# No es un test estadístico del dato: es un test de que el código no tiene
# un bug algebraico (exponente mal puesto, división en el orden
# equivocado, etc.) - si la pendiente no coincide con -alpha, hay un error
# en la función, no en los datos.
audit_pendiente_fosfatos_A <- lm(log(vpmgl_fosfatos) ~ log(L_fosfatos), data = res_A$grilla)
audit_pendiente_auto_A     <- lm(log(vpmgl_auto)     ~ log(L_auto),     data = res_A$grilla)

audit_regresion_caja_A <- bind_rows(
  tidy(audit_pendiente_fosfatos_A) |> filter(term != "(Intercept)") |>
    transmute(sector = "Fosfatos", pendiente_estimada = estimate, alpha_esperado = -alpha_fosfatos),
  tidy(audit_pendiente_auto_A) |> filter(term != "(Intercept)") |>
    transmute(sector = "Automotor", pendiente_estimada = estimate, alpha_esperado = -alpha_auto)
) |>
  left_join(
    bind_rows(glance(audit_pendiente_fosfatos_A), glance(audit_pendiente_auto_A)) |>
      transmute(r.squared) |> mutate(sector = c("Fosfatos", "Automotor")),
    by = "sector"
  ) |>
  mutate(
    diferencia_abs = abs(pendiente_estimada - alpha_esperado),
    ok = diferencia_abs < 1e-6 & r.squared > 0.9999
  )
audit_regresion_caja_A  # ok = TRUE en ambas filas -> la función implementa la fórmula sin error algebraico
write_csv(audit_regresion_caja_A, paste0(ruta_tablas, "audit_regresion_caja_A.csv"))

# NOTA DE DISEÑO (corregida tras feedback): en escala lineal, este gráfico
# salía ilegíble. La razón no es un bug: con rendimientos decrecientes
# (Cobb-Douglas), VPMgL(L) diverge cuando el empleo de un sector tiende a
# cero - ambas curvas se disparan cerca de sus respectivos bordes de la
# caja (L_fosfatos->0 para fosfatos, L_fosfatos->L_tot, o sea L_auto->0,
# para automotor) y son casi planas en el medio, donde el empleo total de
# los dos sectores es un orden de magnitud mayor que el empleo realmente
# observado en fosfatos. En lineal, esa disparidad de escala
# aplasta todo contra el cero salvo los dos picos de borde. Se pasa a
# escala log en el eje Y - es la forma estándar de mostrar una función que
# varía en órdenes de magnitud sin perder la parte "plana" donde vive el
# punto observado y el cruce de equilibrio.
g_caja_asignacion <- ggplot(res_A$grilla, aes(x = L_fosfatos)) +
  geom_line(aes(y = vpmgl_fosfatos, color = "Fosfatos"), linewidth = 1.1) +
  geom_line(aes(y = vpmgl_auto, color = "Automotor"), linewidth = 1.1) +
  geom_vline(xintercept = res_A$L_f_eq, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = res_A$w_eq, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = res_A$L_f0, linetype = "dotted", color = "gray20", linewidth = 0.8) +
  annotate("point", x = res_A$L_f0, y = res_A$vpmgl_f_obs, size = 3, color = paleta_sectores["Fosfatos"]) +
  annotate("point", x = res_A$L_f0, y = res_A$vpmgl_a_obs, size = 3, color = paleta_sectores["Automotor"]) +
  annotate("text", x = res_A$L_f0, y = res_A$vpmgl_f_obs, label = "Observado",
           vjust = -1, hjust = -0.05, size = 3.2, color = "gray30") +
  scale_color_manual(values = paleta_sectores) +
  scale_y_log10(labels = scales::label_number(decimal.mark = ",", big.mark = ".")) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Caja de asignación del trabajo — MFE",
       subtitle = paste0("Equilibrio teórico: w* \u2248 ", fmt_mil(round(res_A$w_eq * 1e6 / 12)),
                         " MAD/mes en L_fosfatos \u2248 ", fmt_mil(round(res_A$L_f_eq)),
                         "  |  Observado ", anio_ref_fpp, ": L_fosfatos = ", fmt_mil(res_A$L_f0)),
       x = "Trabajo asignado a fosfatos", y = "VPMgL, escala log (millones de MAD por trabajador/año, sobre valor agregado)", color = NULL,
       caption = paste0("Brecha en el punto observado: ", round(res_A$brecha_obs, 1),
                        "x - Escenario A (HCP). Fórmula: (1-alpha) x VA/L, es decir la derivada del valor agregado. ",
                        "Eje Y en escala logarítmica: VPMgL diverge cerca de los bordes de la caja por rendimientos decrecientes.")) +
  theme_tp1()
g_caja_asignacion
ggsave(paste0(ruta_graficos, "grafico_caja_asignacion.png"), g_caja_asignacion, width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.4b Sensibilidad al año de referencia ---
# Toda la calibración del MFE (brecha, equilibrio, techo Leontief) cuelga
# de un único año. La auditoría mostró que esa elección NO es inocua: 2023
# es el mínimo de empleo y de producción de fosfatos, así que la brecha
# calculada ahí es de las más altas del rango viable. Acá se recalcula todo
# el modelo para cada año con datos completos, con la misma función, para
# poder citar un RANGO en vez de un número puntual.
sensibilidad_anio_ref <- map_dfr(anios_viables_modelo, function(anio) {
  r <- calibrar_mfe_va(
    alpha_fosfatos, alpha_auto,
    participacion_laboral_fosfatos_A, participacion_laboral_auto_A,
    va_sobre_produccion_fosfatos_A, va_sobre_produccion_auto_A,
    "A - HCP", anio_ref = anio
  )
  tibble(
    anio_ref = anio, es_base = anio == anio_ref_fpp,
    L_fosfatos_obs = r$L_f0, Q_fosfatos_obs = fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio],
    vpmgl_f_mad_mes = r$vpmgl_f_obs * 1e6 / 12, vpmgl_a_mad_mes = r$vpmgl_a_obs * 1e6 / 12,
    brecha_obs = r$brecha_obs, L_equilibrio = r$L_f_eq,
    eq_sobre_obs = r$L_f_eq / r$L_f0, techo_leontief = r$L_cap_leontief
  )
})
sensibilidad_anio_ref
write_csv(sensibilidad_anio_ref, paste0(ruta_tablas, "sensibilidad_anio_referencia.csv"))

rango_brecha_A <- range(sensibilidad_anio_ref$brecha_obs)
rango_techo_A  <- range(sensibilidad_anio_ref$techo_leontief)
tibble(
  indicador = c("Brecha VPMgL observada (veces)", "Techo técnico Leontief (trabajadores)",
                "Equilibrio / empleo observado (veces)"),
  minimo = c(rango_brecha_A[1], rango_techo_A[1], min(sensibilidad_anio_ref$eq_sobre_obs)),
  valor_ano_base = c(res_A$brecha_obs, res_A$L_cap_leontief, res_A$L_f_eq / res_A$L_f0),
  maximo = c(rango_brecha_A[2], rango_techo_A[2], max(sensibilidad_anio_ref$eq_sobre_obs))
)
# LECTURA PARA LA REDACCIÓN: citar el rango, no el punto. El resultado
# CUALITATIVO (el equilibrio queda muy por encima del empleo observado y
# por encima del techo técnico) se sostiene en todos los años; la cifra
# puntual de la brecha no, y cambia de forma apreciable según el año que
# se elija como base.

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
# AUDITORÍA: cuántas filas se descartaron por NA y desde cuándo hay dato
# real, para verificar la afirmación del comentario de arriba con un
# número, no de memoria.
tibble(filas_totales = nrow(tot_marruecos_raw), filas_con_dato = nrow(tot_marruecos),
       filas_na_descartadas = sum(is.na(tot_marruecos_raw$tot)), primer_anio_con_dato = anio_min_tot)
tot_marruecos  # AUDITORÍA

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
# 2010 es el año del cambio de vintage: USGS publicó ese año la cifra
# vieja y la nueva, así que aparece en los dos grupos. Etiquetar los
# períodos como "1995-2010" y "2010-2025" hacía ver un solapamiento que
# confunde; se etiqueta el corte una sola vez, indicando que 2010 es el
# año de la revisión y no un rango compartido.
reservas_eras <- reservas_eras |>
  arrange(era) |>   # deja "pre" primero y "post" segundo, según los niveles del factor
  mutate(etiqueta_periodo = paste0(
    if_else(era == "pre", "Antes de la revisión", "Después de la revisión"),
    "\n(", anio_min, "-", anio_max, ")")) |>
  mutate(etiqueta_periodo = factor(etiqueta_periodo, levels = etiqueta_periodo))
reservas_eras  # AUDITORÍA

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
productividad_valor_fosfatos  # AUDITORÍA
productividad_valor_auto <- inner_join(auto_exportaciones_oc |> select(year, total_mmad),
                                       auto_empleo_observado |> select(year, empleo), by = "year") |>
  mutate(valor_por_trabajador_mmad = total_mmad / empleo, sector = "Automotor")
productividad_valor_auto  # AUDITORÍA

brecha_valor_trabajador <- bind_rows(
  productividad_valor_fosfatos |> select(year, sector, valor_por_trabajador_mmad),
  productividad_valor_auto     |> select(year, sector, valor_por_trabajador_mmad)
) |>
  pivot_wider(names_from = sector, values_from = valor_por_trabajador_mmad) |>
  mutate(brecha_veces = Fosfatos / Automotor)
brecha_valor_trabajador

# CHEQUEO DE REGRESIÓN: ¿la brecha de valor por trabajador tiene una
# tendencia significativa 2021-2024, o es ruido con pocos años? Con solo 4
# puntos (Fosfatos tiene dato completo únicamente 2021-2024) el test tiene
# poca potencia - se reporta igual, con esa salvedad explícita, para no
# sobre-interpretar una tendencia que podría ser puro ruido muestral.
audit_tendencia_brecha <- lm(brecha_veces ~ year, data = filter(brecha_valor_trabajador, !is.na(brecha_veces)))
audit_tendencia_brecha_resumen <- bind_cols(
  tidy(audit_tendencia_brecha) |> filter(term == "year") |> select(pendiente_anual = estimate, p.value),
  glance(audit_tendencia_brecha) |> select(r.squared, nobs)
)
audit_tendencia_brecha_resumen  # nobs bajo (4) -> tratar como descriptivo, no como test concluyente

## --- 3.2 Dotación factorial relativa (PWT) ---
data("pwt10.01", package = "pwt10")
paises_benchmark <- c("Morocco", "Germany", "Spain", "France")

# as_tibble() es necesario: pwt10.01 es un data.frame clásico, así que sin
# convertir, imprimir kl_benchmark vuelca las ~280 filas en consola y choca
# con max.print (en el corrido anterior omitió 80 filas). Como tibble se
# trunca solo a 10 filas.
kl_benchmark <- pwt10.01 |> filter(country %in% paises_benchmark) |>
  mutate(kl = rnna / emp) |> select(country, year, kl, hc, labsh) |>
  arrange(country, year) |> as_tibble()
kl_benchmark  # AUDITORÍA

# CHEQUEO DE REGRESIÓN: acumulación de capital de largo plazo (predicción
# HO/Rybczynski) - tendencia de K/L contra el tiempo, por país, sobre la
# serie completa 1950-2019 (rnna no tiene el problema de imputación de
# labsh, así que acá sí es válido usar el rango completo).
#
# CORRECCIÓN (auditoría): se estima en LOGARITMOS, no en niveles. En
# niveles la pendiente sale en unidades de K/L por año, y comparar 1.084
# (Marruecos) contra 6.583 (Alemania) no dice nada porque los niveles de
# partida difieren en un orden de magnitud - la pendiente en niveles mide
# cuántos dólares de capital por trabajador se agregan por año, no a qué
# ritmo crece el stock. En logs, el coeficiente ES la tasa de crecimiento
# anual y sí es comparable entre países. Se dejan las dos especificaciones
# lado a lado para que se vea la diferencia.
estimar_tendencia_pais <- function(datos, formula_lm, etiqueta) {
  m <- lm(formula_lm, data = datos)
  bind_cols(
    tidy(m) |> filter(term == "year") |> select(pendiente_anual = estimate, p.value),
    glance(m) |> select(r.squared, nobs)
  ) |> mutate(especificacion = etiqueta, .before = 1)
}

audit_tendencia_kl <- kl_benchmark |>
  group_by(country) |>
  group_modify(~ bind_rows(
    estimar_tendencia_pais(.x, kl ~ year,      "niveles (K/L por año)"),
    estimar_tendencia_pais(.x, log(kl) ~ year, "log (tasa de crecimiento anual)")
  )) |>
  ungroup() |>
  mutate(tasa_anual_pct = if_else(especificacion == "log (tasa de crecimiento anual)",
                                  100 * pendiente_anual, NA_real_))
audit_tendencia_kl
write_csv(audit_tendencia_kl, paste0(ruta_tablas, "audit_tendencia_kl.csv"))
# Lectura: en la fila log, tasa_anual_pct es directamente comparable entre
# países. Marruecos acumula capital por trabajador a un ritmo claramente
# menor que los tres europeos, así que la brecha de dotación factorial NO
# se está cerrando - dato relevante para el inciso de HO de largo plazo.

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

# CHEQUEO DE REGRESIÓN: tendencia de labsh (participación del trabajo en
# el ingreso) por país, restringida a cada ventana de dato REAL (desde
# anio_real_labsh, no desde 1950) - es el insumo empírico para testear
# Stolper-Samuelson de largo plazo en el inciso 3d. Corrida sobre el tramo
# imputado, esta regresión daría una pendiente de exactamente cero por
# construcción (el valor está pisado, no midiendo nada) - de ahí la
# importancia de filtrar antes de correrla.
audit_tendencia_labsh <- kl_benchmark |>
  filter(!is.na(labsh_real)) |>
  group_by(country) |>
  group_modify(~ bind_cols(
    tidy(lm(labsh_real ~ year, data = .x)) |> filter(term == "year") |> select(pendiente_anual = estimate, p.value),
    glance(lm(labsh_real ~ year, data = .x)) |> select(r.squared, nobs)
  )) |>
  ungroup()
audit_tendencia_labsh
write_csv(audit_tendencia_labsh, paste0(ruta_tablas, "audit_tendencia_labsh.csv"))

# CORRECCIÓN (auditoría): la versión anterior dejaba que ggplot eligiera el
# eje Y, que quedaba entre 0,48 y 0,51. En ese rango de 3 centésimas, el
# ruido año a año se ve como una montaña rusa dramática - justo lo
# contrario de lo que dice la regresión de arriba, que para Marruecos no
# encuentra tendencia significativa (p = 0,60, R² = 0,01). Un gráfico no
# debería contradecir visualmente al test que lo acompaña. Se corrige con
# tres cosas: eje Y anclado en un rango interpretable (0,40-0,60), la recta
# ajustada con su intervalo de confianza superpuesta, y el resultado del
# test escrito en el subtítulo.
labsh_marruecos <- kl_benchmark |> filter(country == "Morocco", !is.na(labsh_real))
tendencia_labsh_mar <- audit_tendencia_labsh |> filter(country == "Morocco")

g_labsh <- ggplot(labsh_marruecos, aes(x = year, y = labsh_real)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray35", fill = "gray70",
              linewidth = 0.9, alpha = 0.25) +
  geom_line(linewidth = 1.1, color = paleta_sectores["Automotor"]) +
  geom_point(size = 1.8, color = paleta_sectores["Automotor"]) +
  scale_y_continuous(limits = c(0.40, 0.60), breaks = seq(0.40, 0.60, 0.05)) +
  labs(title = "Participación del trabajo en el ingreso — Marruecos",
       subtitle = paste0("Insumo para testear Stolper-Samuelson de largo plazo (3d) — serie real PWT desde ",
                         anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Morocco"],
                         ". Sin tendencia estadísticamente significativa: pendiente ",
                         signif(tendencia_labsh_mar$pendiente_anual, 2),
                         " por año, p = ", round(tendencia_labsh_mar$p.value, 2),
                         ", R² = ", round(tendencia_labsh_mar$r.squared, 3)),
       x = "Año", y = "labsh (PWT)",
       caption = paste0("Fuente: Penn World Table 10.01. Se excluye el tramo previo (",
                        min(pwt10.01$year[pwt10.01$country == "Morocco"]), "-",
                        anios_labsh_real$anio_real_labsh[anios_labsh_real$country == "Morocco"] - 1,
                        "): PWT repite un valor plano imputado sin dato real detrás. ",
                        "El eje Y se fija en 0,40-0,60 a propósito: con un eje ajustado a los datos, ",
                        "una serie plana con ruido de centésimas parece una tendencia marcada.")) +
  theme_tp1()
g_labsh
ggsave(paste0(ruta_graficos, "grafico_labsh_marruecos.png"), g_labsh, width = 10, height = 6, dpi = 300, bg = "white")

# Comparación de las cuatro tendencias, que es donde está el hallazgo
# sustantivo para el inciso 3d: Marruecos es el ÚNICO de los cuatro países
# sin caída significativa de la participación del trabajo.
audit_tendencia_labsh |>
  mutate(significativa_5pct = p.value < 0.05,
         lectura = if_else(significativa_5pct,
                           if_else(pendiente_anual < 0, "cae significativamente", "sube significativamente"),
                           "sin tendencia significativa")) |>
  select(country, pendiente_anual, p.value, r.squared, nobs, lectura)

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
# excedente que OCP no distribuye como salario se capta como dividendo del
# Estado (ocp_financieros$dividendos_mmad) y financia en parte la Caisse de
# Compensation - ver renta_real_vs_ebitda en la Sección H del Script 2, que
# pone lado a lado EBITDA, dividendos girados y masa salarial.


#===============================================================================#
# QUÉ SIGUE EN EL SCRIPT 2
#===============================================================================#
# 1. Segunda fuente de alpha (OCDE, SUT_USEVA) y su descarga.
# 2. Perímetros corregidos: OCP no es solo minería (agrega C20 química);
#    el complejo automotor no es solo ensamblaje (agrega C27 cableado).
# 3. Comparación de escenarios A (HCP) vs. B (OCDE corregido) con la misma
#    función calibrar_mfe_va() para ambos - ver comparacion_escenarios y
#    conclusiones_robustez (que ahora reporta magnitudes, no solo TRUE/FALSE).
# 4. Test NO circular del reparto del ingreso (test_reparto_ingreso): la
#    elasticidad (1-alpha) de cuentas nacionales contra la participación
#    laboral que OCP efectivamente paga. Es el sustento del inciso 2d.
# 5. Comparación Cobb-Douglas vs. Leontief - por qué NO citar L_fosfatos_eq
#    como predicción operativa (ver también sensibilidad_anio_ref, 2.4b).
# 6. Convergencia de intensidades factoriales 2014-2021 (versión corregida,
#    con la retractación documentada de la versión con perímetro estrecho).
# 7. Módulo de predicción del salario (monopsonio / reparto de renta) y
#    validación contra EBITDA y dividendos de OCP.