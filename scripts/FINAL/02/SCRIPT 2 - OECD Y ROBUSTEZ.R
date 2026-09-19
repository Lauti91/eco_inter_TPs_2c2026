#===============================================================================#
# TP2 - SCRIPT 2 de 2: OCDE, PERÍMETROS CORREGIDOS Y MÓDULO DE SALARIOS
#===============================================================================#
# CONSOLIDACIÓN: junta TEST_5_2 (escenarios A/B) + TEST_7 (VA, alpha
# mezclado OCP, salarios) + TEST_10 (perímetro automotor corregido,
# retractación) en su versión final y correcta. Se omiten deliberadamente
# los intentos intermedios con errores (el alpha B08/C29 sin mezclar, el
# ajuste de empleo con el bug algebraico de TEST_8/9) - quedan documentados
# como historia en TP2_historia_completa.md e TP2_informe_de_avance.tex,
# no repetidos acá como código activo.
#
# DEPENDE DE: TP2_01_datos_y_modelo_base.R (correrlo primero). Usa
# datos_investigacion y extraer_serie(), ocp_empleo_serie,
# auto_empleo_observado, fosfatos_produccion, auto_produccion,
# ocp_financieros, auto_exportaciones_oc, salarios_cnss_sector_2020,
# alpha_fosfatos/alpha_auto, anio_ref_fpp, L_fosfatos_0, las paletas, la
# función calibrar_mfe_va(), el objeto res_A y el helper fmt_mil().
#
# FÓRMULA DE VPMgL (ver cabecera del Script 1 para la historia completa):
# una sola función compartida, calibrar_mfe_va(), definida en Script 1
# sub-bloque 2.1, con VPMgL = (1-alpha)·VA(L)/L. Acá se la reutiliza para
# construir el Escenario B (OCDE, perímetros corregidos) con el alpha,
# share_trabajo y va_sobre_output que se calculan más abajo (Secciones B y
# C); el Escenario A se reusa directamente del objeto res_A que ya viene
# calculado desde Script 1, no se recalcula acá.
#
# La participación laboral observada NO entra en la fórmula del VPMgL (eso
# la volvía circular - ver Script 1). Entra como benchmark independiente en
# el test del inciso 2d, Sección H de este script.
#
# No está corrido de punta a punta en esta consolidación.

if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr", repos = "https://cloud.r-project.org")


#===============================================================================#
# A - DESCARGA DE LA SERIE OCDE (SUT_USEVA, Marruecos 2014-2021)
#===============================================================================#
# El endpoint SDMX con filtro de país (vía rsdmx) devuelve 403; el CSV
# completo sin filtro funciona y se cachea en disco.

ruta_cache_oecd <- paste0(ruta_tablas, "oecd_useva_marruecos.csv")

if (file.exists(ruta_cache_oecd)) {
  oecd_mar <- read_csv(ruta_cache_oecd, show_col_types = FALSE)
} else {
  url_useva <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NASU@DF_USEVA_T1600,1.0/all?format=csvfilewithlabels"
  datos_useva <- read_csv(url_useva, show_col_types = FALSE)
  oecd_mar <- datos_useva |> filter(REF_AREA == "MAR")
  write_csv(oecd_mar, ruta_cache_oecd)
  rm(datos_useva)
}
# AUDITORÍA: glimpse() en vez de print() porque es la tabla cruda de la
# OCDE sin filtrar (todas las actividades/transacciones de Marruecos) -
# conviene ver la estructura y confirmar que ACTIVITY/TRANSACTION/
# TIME_PERIOD/OBS_VALUE están como se espera, no las miles de filas.
glimpse(oecd_mar)
oecd_mar |> distinct(ACTIVITY, `Economic activity`) |> arrange(ACTIVITY)

# Códigos ISIC confirmados contra el dataset real:
#   B08 = Other mining and quarrying <- fosfatos (mineral NO metálico;
#         B07 es plata/cobalto, no OCP)
#   C20 = Manufacture of chemicals and chemical products <- fertilizantes
#         PROCESADOS (la mayor parte de los ingresos de OCP, no roca cruda)
#   C29 = Manufacture of motor vehicles (ensamblaje)
#   C27 = Manufacture of electrical equipment <- câblage (cableado)
#   B1G = Value added, gross | D1 = Compensation of employees
#   B2A3G = Operating surplus and mixed income, gross | P1 = Output


#===============================================================================#
# B - ALPHA MEZCLADO PARA FOSFATOS (OCP: B08 minería + C20 química)
#===============================================================================#
# ERROR DETECTADO Y CORREGIDO: aplicar el alpha de minería pura (B08) a los
# ingresos TOTALES de OCP implicaba un VA de OCP 2,93 veces el VA de todo
# el sector minero - imposible. Causa: OCP está integrada verticalmente y
# la mayor parte de sus ingresos viene de fertilizantes procesados (C20),
# no de roca cruda (B08).

alpha_oecd_extendido <- oecd_mar |>
  filter(ACTIVITY %in% c("B08", "C20", "C29", "C27"),
         TRANSACTION %in% c("D1", "B1G", "B2A3G", "P1")) |>
  select(ACTIVITY, `Economic activity`, TRANSACTION, TIME_PERIOD, OBS_VALUE) |>
  pivot_wider(names_from = TRANSACTION, values_from = OBS_VALUE) |>
  mutate(alpha_capital = B2A3G / B1G, share_trabajo = D1 / B1G, va_sobre_output = B1G / P1) |>
  arrange(ACTIVITY, TIME_PERIOD)
alpha_oecd_extendido  # AUDITORÍA
write_csv(alpha_oecd_extendido, paste0(ruta_tablas, "alpha_oecd_extendido.csv"))

# CHEQUEO DE IDENTIDAD CONTABLE: alpha_capital (B2A3G/B1G) + share_trabajo
# (D1/B1G) no tienen por qué sumar exactamente 1 - el residual son
# impuestos netos a la producción y otros ajustes que el VA (B1G) incluye
# pero que no son ni excedente de capital ni compensación al trabajo. Un
# residual chico (unos pocos puntos porcentuales) es esperable; uno grande
# o negativo señalaría un problema en las columnas ACTIVITY/TRANSACTION
# filtradas.
audit_identidad_oecd <- alpha_oecd_extendido |>
  mutate(residual_capital_mas_trabajo = 1 - (alpha_capital + share_trabajo)) |>
  select(ACTIVITY, TIME_PERIOD, alpha_capital, share_trabajo, residual_capital_mas_trabajo)
audit_identidad_oecd
write_csv(audit_identidad_oecd, paste0(ruta_tablas, "audit_identidad_oecd.csv"))

alpha_ocp_mezclado_v0 <- alpha_oecd_extendido |>
  filter(ACTIVITY %in% c("B08", "C20")) |>
  group_by(TIME_PERIOD) |>
  summarise(B1G_total = sum(B1G, na.rm = TRUE), D1_total = sum(D1, na.rm = TRUE),
            B2A3G_total = sum(B2A3G, na.rm = TRUE), P1_total = sum(P1, na.rm = TRUE),
            share_trabajo = D1_total / B1G_total, alpha_capital = B2A3G_total / B1G_total,
            va_sobre_output = B1G_total / P1_total, .groups = "drop")
alpha_ocp_mezclado_v0  # AUDITORÍA (versión previa, ponderada por VA agregado - dejada para comparar contra la vigente)
# v0 quedaba ponderado por el VA AGREGADO de cada rama en toda la economía
# marroquí - un supuesto razonable a falta de algo mejor, pero no reflejaba
# necesariamente cómo se reparte el propio negocio de OCP entre roca y
# química. La Ronda 3 trajo la composición real de ingresos de OCP por
# segmento, así que se reconstruye ponderando por eso en vez de por el VA
# sectorial agregado.

## --- Composición real de ingresos de OCP por segmento (Ronda 3) ---
composicion_ocp_ingresos <- extraer_serie("Ingresos por segmento: Roche de phosphate (Roca fosfórica cruda)") |>
  rename(roca_mmad = valor)
composicion_ocp_ingresos <- full_join(composicion_ocp_ingresos,
                                      extraer_serie("Ingresos por segmento: Engrais phosphatés (Fertilizantes)") |> rename(fertilizantes_mmad = valor),
                                      by = "year")
composicion_ocp_ingresos <- full_join(composicion_ocp_ingresos,
                                      extraer_serie("Ingresos por segmento: Acide phosphorique (Ácido fosfórico)") |> rename(acido_mmad = valor),
                                      by = "year")
composicion_ocp_ingresos <- full_join(composicion_ocp_ingresos,
                                      extraer_serie("Ingresos por segmento: Autres / Divers") |> rename(otros_mmad = valor), by = "year") |>
  mutate(
    quimica_mmad = fertilizantes_mmad + acido_mmad,        # DAP/MAP/TSP/NPK + ácido fosfórico -> C20
    total_clasificado = roca_mmad + quimica_mmad,           # se excluye "otros" (sin ISIC claro) del peso
    peso_B08 = roca_mmad / total_clasificado,
    peso_C20 = quimica_mmad / total_clasificado
  )
write_csv(composicion_ocp_ingresos, paste0(ruta_tablas, "composicion_ocp_ingresos.csv"))
# Confirma con creces la decisión de mezclar B08+C20 en vez de usar B08
# solo: la roca cruda es apenas 12-14% de la facturación de OCP en
# 2021-2023: peso_B08 ronda 0,12-0,14, no 1,00.

## --- Alpha mezclado v1, ponderado por el peso REAL de cada segmento en OCP ---
# La composición de ingresos (Ronda 3) solo cubre 2021-2023, mientras que
# la OCDE cubre 2014-2021 - la intersección exacta es un único año (2021).
# Un inner_join colapsaría la serie a un solo punto (así salió en la
# primera corrida) y rompería el gráfico de convergencia. En vez de eso:
# se usa el peso REAL en 2021 (el único año con ambos datos), y para
# 2014-2020 (sin composición de ingresos propia) se aplica el PROMEDIO de
# los tres años disponibles (2021-2023, bastante estable: 11,7%-14,0% de
# roca) - una extrapolación hacia atrás explícita, no un dato real, pero
# más razonable que perder siete años de serie.
peso_B08_promedio <- mean(composicion_ocp_ingresos$peso_B08, na.rm = TRUE)
peso_C20_promedio <- mean(composicion_ocp_ingresos$peso_C20, na.rm = TRUE)

alpha_ocp_mezclado <- alpha_oecd_extendido |>
  filter(ACTIVITY %in% c("B08", "C20")) |>
  select(ACTIVITY, TIME_PERIOD, alpha_capital, share_trabajo, va_sobre_output) |>
  pivot_wider(names_from = ACTIVITY, values_from = c(alpha_capital, share_trabajo, va_sobre_output)) |>
  left_join(composicion_ocp_ingresos |> select(TIME_PERIOD = year, peso_B08, peso_C20), by = "TIME_PERIOD") |>
  mutate(
    fuente_peso = if_else(!is.na(peso_B08), "real (Ronda 3)", "promedio 2021-2023 (extrapolado hacia atrás)"),
    peso_B08 = coalesce(peso_B08, peso_B08_promedio),
    peso_C20 = coalesce(peso_C20, peso_C20_promedio),
    # CORRECCIÓN (auditoría): los pesos peso_B08/peso_C20 son
    # participaciones en los INGRESOS de OCP. Eso es el ponderador correcto
    # para va_sobre_output (que es un ratio VA/producción: el agregado de
    # VA/P con pesos de producción da el VA/P del conjunto). Pero NO es el
    # ponderador correcto para alpha_capital ni share_trabajo, que son
    # participaciones SOBRE EL VALOR AGREGADO: agregarlas requiere pesos de
    # VA, no de ingresos. Se derivan los pesos de VA a partir de los de
    # ingresos multiplicando por el respectivo VA/producción y
    # renormalizando. El efecto numérico es chico (share_trabajo 2021 pasa
    # de 0,2135 a ~0,2154) pero la inconsistencia conceptual desaparece.
    va_unit_B08 = peso_B08 * va_sobre_output_B08,
    va_unit_C20 = peso_C20 * va_sobre_output_C20,
    peso_va_B08 = va_unit_B08 / (va_unit_B08 + va_unit_C20),
    peso_va_C20 = va_unit_C20 / (va_unit_B08 + va_unit_C20),
    alpha_capital   = peso_va_B08 * alpha_capital_B08 + peso_va_C20 * alpha_capital_C20,
    share_trabajo   = peso_va_B08 * share_trabajo_B08 + peso_va_C20 * share_trabajo_C20,
    va_sobre_output = peso_B08    * va_sobre_output_B08 + peso_C20    * va_sobre_output_C20
  ) |>
  select(TIME_PERIOD, alpha_capital, share_trabajo, va_sobre_output,
         peso_B08, peso_C20, peso_va_B08, peso_va_C20, fuente_peso) |>
  # B1G_total se mantiene disponible (del cálculo v0) para el chequeo de
  # consistencia de más abajo, que pregunta algo distinto (¿el VA de OCP
  # cabe dentro de B08+C20 combinados?) - no depende de CÓMO se ponderan.
  left_join(alpha_ocp_mezclado_v0 |> select(TIME_PERIOD, B1G_total), by = "TIME_PERIOD")
alpha_ocp_mezclado  # confirmar: 8 filas (2014-2021), no 1 - y fuente_peso
# marca cuál es el único año con peso real (2021)
write_csv(alpha_ocp_mezclado, paste0(ruta_tablas, "alpha_ocp_mezclado.csv"))
# Solo hay intersección real con los años de la OCDE (2014-2021) donde
# también hay composición de ingresos (2021-2023): en la práctica, esto
# deja SOLO 2021 con ambos insumos disponibles - suficiente porque
# anio_param = 2021 es el año de referencia que ya se usa en todo el
# Script 2. Comparar contra alpha_ocp_mezclado_v0 (arriba) para ver cuánto
# cambia el resultado al pasar del supuesto sectorial al peso real.

# Chequeo de consistencia (el que destapó el error): ¿el VA de OCP cabe
# dentro del sector que la contiene?
chequeo_consistencia_ocp <- tibble(TIME_PERIOD = alpha_ocp_mezclado$TIME_PERIOD) |>
  left_join(ocp_financieros |> select(TIME_PERIOD = year, ingresos_mmad), by = "TIME_PERIOD") |>
  left_join(alpha_ocp_mezclado |> select(TIME_PERIOD, B1G_B08_C20 = B1G_total, va_sobre_output), by = "TIME_PERIOD") |>
  left_join(alpha_oecd_extendido |> filter(ACTIVITY == "B08") |> select(TIME_PERIOD, B1G_solo_B08 = B1G), by = "TIME_PERIOD") |>
  filter(!is.na(ingresos_mmad)) |>
  mutate(va_ocp_estimado = ingresos_mmad * va_sobre_output,
         ratio_vs_solo_B08 = va_ocp_estimado / B1G_solo_B08,      # ~2,9 -> imposible, confirma el error
         ratio_vs_B08_mas_C20 = va_ocp_estimado / B1G_B08_C20,    # <1  -> consistente
         consistente = ratio_vs_B08_mas_C20 < 1)
# NOTA: esta tabla tiene UNA sola fila (2021). No es un error: es el único
# año donde se solapan los estados financieros de OCP (desde 2021) con la
# cobertura de la OCDE (hasta 2021). El chequeo es concluyente igual - un
# ratio de 2,9 es imposible con cualquier margen de error -, pero conviene
# no describirlo como si se hubiera verificado en varios años.
chequeo_consistencia_ocp
write_csv(chequeo_consistencia_ocp, paste0(ruta_tablas, "chequeo_consistencia_ocp_sector.csv"))


#===============================================================================#
# C - ALPHA MEZCLADO PARA AUTOMOTOR (C29 ensamblaje + C27 cableado)
#===============================================================================#
# Error simétrico al de OCP: las exportaciones automotrices totales
# superaban la producción de C29 sola (ratio 1,07 en 2021) - imposible.
# Causa: una porción sustancial es câblage (arneses), que en ISIC es C27
# (equipo eléctrico), no C29.

ramas_automotor <- c("C29", "C27")

alpha_auto_mezclado <- oecd_mar |>
  filter(ACTIVITY %in% ramas_automotor, TRANSACTION %in% c("D1", "B1G", "B2A3G", "P1")) |>
  select(ACTIVITY, TRANSACTION, TIME_PERIOD, OBS_VALUE) |>
  pivot_wider(names_from = TRANSACTION, values_from = OBS_VALUE) |>
  group_by(TIME_PERIOD) |>
  summarise(B1G_total = sum(B1G, na.rm = TRUE), D1_total = sum(D1, na.rm = TRUE),
            B2A3G_total = sum(B2A3G, na.rm = TRUE), P1_total = sum(P1, na.rm = TRUE),
            share_trabajo = D1_total / B1G_total, alpha_capital = B2A3G_total / B1G_total,
            va_sobre_output = B1G_total / P1_total, .groups = "drop")
write_csv(alpha_auto_mezclado, paste0(ruta_tablas, "alpha_auto_mezclado.csv"))

chequeo_consistencia_auto <- alpha_auto_mezclado |>
  select(TIME_PERIOD, P1_solo_ramas = P1_total, va_sobre_output) |>
  left_join(oecd_mar |> filter(ACTIVITY == "C29", TRANSACTION == "P1") |> select(TIME_PERIOD, P1_solo_C29 = OBS_VALUE),
            by = "TIME_PERIOD") |>
  left_join(auto_exportaciones_oc |> select(TIME_PERIOD = year, exportaciones = total_mmad), by = "TIME_PERIOD") |>
  filter(!is.na(exportaciones)) |>
  mutate(ratio_export_sobre_C29 = exportaciones / P1_solo_C29,           # > 1 en los 3 años -> confirma el error
         ratio_export_sobre_C29_C27 = exportaciones / P1_solo_ramas,     # < 1 en los 3 años -> consistente
         consistente = ratio_export_sobre_C29_C27 < 1)
chequeo_consistencia_auto  # consistente = TRUE en 2019, 2020 y 2021
write_csv(chequeo_consistencia_auto, paste0(ruta_tablas, "chequeo_consistencia_automotor.csv"))
# Si en algún momento este chequeo diera > 1 de nuevo, el candidato natural
# a agregar es C25 (estampado) o C28 (powertrain) - no hizo falta esta vez.


#===============================================================================#
# D - PARÁMETROS OCDE (perímetros corregidos) PARA EL ESCENARIO B
#===============================================================================#
# Ya no se calcula acá una VPMgL de punto aparte (esa era la tercera
# fórmula que convivía sin resolverse - ver cabecera). alpha_capital,
# share_trabajo y va_sobre_output de este bloque (par_fosfatos/par_auto)
# son los insumos que la Sección E le pasa a calibrar_mfe_va() (Script 1)
# para construir el Escenario B como curva completa, con la misma fórmula
# que el Escenario A: VPMgL(L) = (1-alpha) * VA(L)/L. El share_trabajo NO
# entra en esa fórmula (ver cabecera): se pasa para que quede registrado en
# el objeto res_B y poder contrastarlo contra (1-alpha) en la Sección H.

anio_param <- 2021  # último año con datos OCDE observados

par_fosfatos <- alpha_ocp_mezclado  |> filter(TIME_PERIOD == anio_param)
par_auto     <- alpha_auto_mezclado |> filter(TIME_PERIOD == anio_param)
# AUDITORÍA: estos son, literalmente, los share_trabajo/va_sobre_output
# que la Sección E le pasa a calibrar_mfe_va() para el Escenario B -
# verlos acá antes de que se usen, no solo dentro de comparacion_escenarios.
par_fosfatos
par_auto

# TEST DE CONSISTENCIA DEL PERÍMETRO AUTOMOTOR (renombrado tras auditoría).
# Antes este bloque se llamaba "test de sanidad de la VPMgL" y su resultado
# se guardaba como vpmgl_auto_mad_mes. Era un nombre equivocado: lo que
# calcula, share_trabajo × VA / L, es D1/L, o sea la REMUNERACIÓN MEDIA por
# trabajador que implican las cuentas nacionales, no un producto marginal.
# La fórmula está bien para lo que este test quiere hacer - contrastar la
# remuneración media implícita en el perímetro C29+C27 contra el salario
# industrial efectivamente declarado a CNSS, que es una validación del
# PERÍMETRO y del ajuste de empleo, no del modelo MFE -, pero el nombre
# inducía a confundirlo con el VPMgL del Bloque 2. Se renombra; la cuenta
# no cambia.
#
# Se usa el VA REAL de C29+C27 en 2021 (no reconstruido vía exportaciones) y
# el empleo del ecosistema de ESE mismo año, escalado por la fracción de
# exportaciones que corresponde a cableado+ensamblaje (proxy de qué parte
# del empleo pertenece a esos dos segmentos, ya que la OCDE no publica
# empleo).
fraccion_empleo_c29_c27 <- auto_exportaciones_oc |>
  filter(year == anio_param) |>
  mutate(fraccion = (cableado_mmad + ensamblaje_mmad) / total_mmad) |> pull(fraccion)

L_ecosistema_2021 <- auto_empleo_observado$empleo[auto_empleo_observado$year == anio_param]
L_auto_ajustado   <- L_ecosistema_2021 * fraccion_empleo_c29_c27
VA_c29_c27_2021   <- par_auto$B1G_total

# D1/L: remuneración media por trabajador implícita en las cuentas OCDE
remuneracion_media_implicita <- par_auto$share_trabajo * VA_c29_c27_2021 / L_auto_ajustado

test_consistencia_perimetro_auto <- tibble(
  fraccion_empleo_c29_c27, L_ecosistema_2021, L_auto_ajustado, VA_c29_c27_2021,
  remuneracion_implicita_mad_mes = remuneracion_media_implicita * 1e6 / 12,
  salario_cnss_mad_mes = salarios_cnss_sector_2020$salario_mad_mes[grepl("Industrie", salarios_cnss_sector_2020$sector)]
) |>
  mutate(coincide_con_cnss = abs(remuneracion_implicita_mad_mes / salario_cnss_mad_mes - 1) < 0.25,
         cociente_veces = remuneracion_implicita_mad_mes / salario_cnss_mad_mes)
test_consistencia_perimetro_auto
write_csv(test_consistencia_perimetro_auto, paste0(ruta_tablas, "test_consistencia_perimetro_auto.csv"))
# Lectura: la remuneración media implícita (~4.700 MAD/mes) y el salario
# CNSS declarado (5.002 MAD/mes) coinciden dentro de ~6%. Eso valida que el
# perímetro C29+C27 y el ajuste de empleo son razonables - que es todo lo
# que este test pretende. No se sigue ampliando el perímetro para acercar
# más el cociente a 1: sería sobreajuste, y además las dos cifras vienen de
# años distintos (CNSS 2020, OCDE 2021).


#===============================================================================#
# E - ESCENARIOS A (HCP) vs. B (OCDE, perímetros corregidos)
#===============================================================================#
# res_A NO se recalcula acá: es el mismo objeto que ya construyó Script 1
# (sub-bloque 2.4) con calibrar_mfe_va() y el alpha/participación/VA-ratio
# de HCP. Acá se agrega res_B, con la MISMA función, pasándole el alpha,
# share_trabajo y va_sobre_output que salen de la OCDE (perímetros
# corregidos, año 2021 - único año con ambos insumos disponibles).

alpha_fosfatos_B <- alpha_ocp_mezclado$alpha_capital[alpha_ocp_mezclado$TIME_PERIOD == anio_param]
alpha_auto_B     <- alpha_auto_mezclado$alpha_capital[alpha_auto_mezclado$TIME_PERIOD == anio_param]

escenarios <- tibble(
  escenario = c("A - HCP (TRE Base 2014)", "B - OCDE mezclado (2021 observado)"),
  alpha_fosfatos = c(alpha_fosfatos, alpha_fosfatos_B),
  alpha_auto     = c(alpha_auto, alpha_auto_B)
)
escenarios

res_B <- calibrar_mfe_va(
  alpha_fosfatos_B, alpha_auto_B,
  par_fosfatos$share_trabajo, par_auto$share_trabajo,
  par_fosfatos$va_sobre_output, par_auto$va_sobre_output,
  "B - OCDE mezclado"
)

# AUDITORÍA: mismo tratamiento que res_A_resumen en Script 1 - resumen
# escalar de res_B (sin la grilla) para revisar los números del Escenario
# B de un vistazo.
res_B_resumen <- as_tibble(res_B[setdiff(names(res_B), "grilla")])
res_B_resumen
write_csv(res_B_resumen, paste0(ruta_tablas, "res_B_resumen.csv"))

# CHEQUEO DE REGRESIÓN (auditoría del código, mismo criterio que en
# Script 1 para res_A): la pendiente log-log de vpmgl contra L debe
# recuperar exactamente -alpha - si no, calibrar_mfe_va() tiene un bug
# algebraico que también afectaría al Escenario A, así que conviene
# chequear ambos escenarios y no asumir que "ya se validó una vez alcanza".
audit_pendiente_fosfatos_B <- lm(log(vpmgl_fosfatos) ~ log(L_fosfatos), data = res_B$grilla)
audit_pendiente_auto_B     <- lm(log(vpmgl_auto)     ~ log(L_auto),     data = res_B$grilla)

audit_regresion_caja_B <- bind_rows(
  tidy(audit_pendiente_fosfatos_B) |> filter(term != "(Intercept)") |>
    transmute(sector = "Fosfatos", pendiente_estimada = estimate, alpha_esperado = -alpha_fosfatos_B),
  tidy(audit_pendiente_auto_B) |> filter(term != "(Intercept)") |>
    transmute(sector = "Automotor", pendiente_estimada = estimate, alpha_esperado = -alpha_auto_B)
) |>
  left_join(
    bind_rows(glance(audit_pendiente_fosfatos_B), glance(audit_pendiente_auto_B)) |>
      transmute(r.squared) |> mutate(sector = c("Fosfatos", "Automotor")),
    by = "sector"
  ) |>
  mutate(
    diferencia_abs = abs(pendiente_estimada - alpha_esperado),
    ok = diferencia_abs < 1e-6 & r.squared > 0.9999
  )
audit_regresion_caja_B  # ok = TRUE en ambas filas -> Escenario B también libre de error algebraico
write_csv(audit_regresion_caja_B, paste0(ruta_tablas, "audit_regresion_caja_B.csv"))

comparacion_escenarios <- tibble(
  escenario = c(res_A$etiqueta, res_B$etiqueta),
  alpha_fosfatos = c(res_A$alpha_f, res_B$alpha_f), alpha_automotor = c(res_A$alpha_a, res_B$alpha_a),
  elasticidad_trabajo_fosfatos = c(res_A$elast_f, res_B$elast_f),
  share_trabajo_fosfatos = c(res_A$share_f, res_B$share_f), share_trabajo_automotor = c(res_A$share_a, res_B$share_a),
  L_fosfatos_equilibrio = c(res_A$L_f_eq, res_B$L_f_eq), L_fosfatos_observado = c(res_A$L_f0, res_B$L_f0),
  L_capacidad_leontief = c(res_A$L_cap_leontief, res_B$L_cap_leontief),
  brecha_vpmgl_observada = c(res_A$brecha_obs, res_B$brecha_obs)
) |>
  mutate(eq_sobre_observado_veces = L_fosfatos_equilibrio / L_fosfatos_observado,
         eq_sobre_capacidad_veces = L_fosfatos_equilibrio / L_capacidad_leontief)
comparacion_escenarios
write_csv(comparacion_escenarios, paste0(ruta_tablas, "comparacion_escenarios_alpha.csv"))

# Chequeo de robustez del hallazgo central a la elección de escenario.
#
# CORRECCIÓN (auditoría): la versión anterior solo reportaba TRUE/FALSE en
# dos preguntas binarias con umbrales laxos, y de ahí se concluía
# "robusto". Eso confunde robustez del SIGNO con robustez de la MAGNITUD:
# las respuestas binarias no se mueven, pero la brecha y el equilibrio sí
# cambian mucho entre escenarios. Se agregan las magnitudes al lado de cada
# respuesta para que la conclusión que se escriba en el TP sea la correcta:
# el hallazgo CUALITATIVO es robusto, la cifra puntual no lo es.
conclusiones_robustez <- tibble(
  pregunta = c("Equilibrio muy por encima del empleo observado (>2x)?",
               "Equilibrio por encima del techo técnico Leontief?",
               "Brecha de VPMgL en el punto observado (veces)"),
  escenario_A = c(res_A$L_f_eq / res_A$L_f0 > 2, res_A$L_f_eq > res_A$L_cap_leontief, NA),
  escenario_B = c(res_B$L_f_eq / res_B$L_f0 > 2, res_B$L_f_eq > res_B$L_cap_leontief, NA),
  magnitud_A  = c(res_A$L_f_eq / res_A$L_f0, res_A$L_f_eq / res_A$L_cap_leontief, res_A$brecha_obs),
  magnitud_B  = c(res_B$L_f_eq / res_B$L_f0, res_B$L_f_eq / res_B$L_cap_leontief, res_B$brecha_obs)
) |>
  mutate(dispersion_B_sobre_A = magnitud_B / magnitud_A)
conclusiones_robustez
write_csv(conclusiones_robustez, paste0(ruta_tablas, "conclusiones_robustez_escenarios.csv"))
# Lectura: las respuestas binarias no cambian (robustez cualitativa), pero
# dispersion_B_sobre_A muestra cuánto se mueve cada magnitud al cambiar de
# fuente de parámetros. Al redactar, citar rango o ambos escenarios; NO un
# único número como si fuera invariante.

## --- Sensibilidad adicional: exportaciones vs. producción del automotor ---
# El valor del producto automotor se aproxima con EXPORTACIONES, pero el
# propio chequeo_consistencia_auto muestra que las exportaciones son ~87%
# de la producción de C29+C27 (el resto va al mercado interno). Como el
# ratio va_sobre_output está definido sobre PRODUCCIÓN, usar exportaciones
# subestima el VA del automotor y por lo tanto INFLA la brecha. Acá se
# cuantifica ese sesgo con el ratio de cobertura efectivamente observado.
cobertura_export_auto <- chequeo_consistencia_auto |>
  summarise(cobertura_media = mean(ratio_export_sobre_C29_C27, na.rm = TRUE),
            cobertura_min = min(ratio_export_sobre_C29_C27, na.rm = TRUE),
            cobertura_max = max(ratio_export_sobre_C29_C27, na.rm = TRUE),
            anios = paste(range(TIME_PERIOD), collapse = "-"))
cobertura_export_auto

sensibilidad_cobertura_auto <- tibble(
  supuesto = c("Base: valor del producto automotor = exportaciones",
               "Corregido: exportaciones escaladas a producción (cobertura media observada)"),
  factor_escala = c(1, 1 / cobertura_export_auto$cobertura_media),
  brecha_escenario_A = res_A$brecha_obs / c(1, 1 / cobertura_export_auto$cobertura_media),
  brecha_escenario_B = res_B$brecha_obs / c(1, 1 / cobertura_export_auto$cobertura_media)
)
sensibilidad_cobertura_auto
write_csv(sensibilidad_cobertura_auto, paste0(ruta_tablas, "sensibilidad_cobertura_automotor.csv"))
# No se adopta la corrección como base porque el ratio de cobertura solo
# se observa para 2019-2021 (la OCDE no llega a 2023) y extrapolarlo al año
# base sería inventar un dato. Se deja como cota: la brecha "verdadera" es
# probablemente algo MENOR que la reportada, no mayor.


#===============================================================================#
# F - COBB-DOUGLAS vs. LEONTIEF: ¿hay margen técnico para el equilibrio?
#===============================================================================#
# El equilibrio teórico L_fosfatos_eq es una extrapolación de varias veces
# el empleo observado en fosfatos (ver comparacion_escenarios y, para el
# rango según año de calibración, sensibilidad_anio_ref en el Script 1).
# Acá se lo compara contra una tecnología de coeficientes fijos, calibrada
# en el mismo punto observado, para mostrar que ni siquiera hay margen
# TÉCNICO para acercarse a esa cifra.

leontief_vs_cd <- tibble(L_fosfatos = seq(1000, res_A$L_tot - 1000, length.out = 2000)) |>
  mutate(Q_cobb_douglas = res_A$A_f * L_fosfatos ^ (1 - res_A$alpha_f),
         Q_leontief     = pmin(L_fosfatos / res_A$aL_leontief, res_A$Q_max))

g_leontief_vs_cd <- ggplot(leontief_vs_cd, aes(x = L_fosfatos)) +
  geom_line(aes(y = Q_cobb_douglas, color = "Cobb-Douglas (sustituible)"), linewidth = 1.1) +
  geom_line(aes(y = Q_leontief, color = "Leontief (coeficientes fijos)"), linewidth = 1.1) +
  geom_vline(xintercept = res_A$L_f0, linetype = "dotted", color = "gray30") +
  geom_vline(xintercept = res_A$L_f_eq, linetype = "dashed", color = "gray30") +
  scale_color_manual(values = c("Cobb-Douglas (sustituible)" = "gray50",
                                "Leontief (coeficientes fijos)" = paleta_sectores[["Fosfatos"]])) +
  labs(title = "¿Qué tecnología describe mejor a la minería de fosfatos?",
       subtitle = paste0("Producción de fosfatos bajo dos supuestos tecnológicos, ",
                         "calibrados en el mismo punto observado (", res_A$anio_ref, ")"),
       x = "Trabajo asignado a fosfatos", y = "Producción de fosfatos (miles de toneladas)", color = NULL,
       caption = paste0("Empleo observado: ", fmt_mil(res_A$L_f0),
                        ". Equilibrio Cobb-Douglas: ", fmt_mil(round(res_A$L_f_eq)),
                        ". Techo Leontief a capacidad pico (", res_A$anio_Q_max, ", serie ", res_A$rango_Q,
                        "): ", fmt_mil(round(res_A$L_cap_leontief)), ".")) +
  theme_tp1()
g_leontief_vs_cd
ggsave(paste0(ruta_graficos, "grafico_leontief_vs_cobb_douglas.png"), g_leontief_vs_cd, width = 10, height = 6.5, dpi = 300, bg = "white")
# Conclusión: no hay margen técnico bajo coeficientes fijos. El hallazgo
# cualitativo (brecha institucional) se sostiene; la cifra puntual de
# equilibrio no es una predicción operativa creíble.


#===============================================================================#
# G - CONVERGENCIA DE INTENSIDADES FACTORIALES (retractación documentada)
#===============================================================================#
# HALLAZGO INICIAL (con perímetro estrecho, solo B08): la intensidad de
# capital en fosfatos caía significativamente 2014-2021 mientras la de
# automotor subía - una "convergencia" que se iba a usar como evidencia de
# cambio estructural de largo plazo (HO).
#
# RETRACTADO: al corregir el perímetro de fosfatos (B08+C20), la caída
# desaparece. Era un artefacto de mirar solo el segmento minero de OCP, no
# su negocio completo (que incluye la química, más intensiva en capital).

serie_convergencia <- bind_rows(
  alpha_ocp_mezclado  |> mutate(sector = "Fosfatos (B08+C20)"),
  alpha_auto_mezclado |> mutate(sector = "Automotor (C29+C27)")
) |> select(TIME_PERIOD, sector, alpha_capital)

ajustar_tendencia <- function(datos, etiqueta) {
  m <- lm(alpha_capital ~ TIME_PERIOD, data = datos)
  bind_cols(tidy(m) |> filter(term == "TIME_PERIOD") |> select(pendiente_anual = estimate, p.value),
            glance(m) |> select(r.squared, nobs)) |> mutate(serie = etiqueta, .before = 1)
}

tendencias <- bind_rows(
  ajustar_tendencia(filter(serie_convergencia, sector == "Fosfatos (B08+C20)"), "Fosfatos, perímetro CORRECTO (B08+C20)"),
  ajustar_tendencia(filter(serie_convergencia, sector == "Automotor (C29+C27)"), "Automotor, perímetro CORRECTO (C29+C27)"),
  ajustar_tendencia(filter(alpha_oecd_extendido, ACTIVITY == "B08"), "Fosfatos, perímetro ESTRECHO (solo B08) - RETRACTADO"),
  ajustar_tendencia(filter(alpha_oecd_extendido, ACTIVITY == "C29"), "Automotor, perímetro ESTRECHO (solo C29) - referencia")
)
tendencias
write_csv(tendencias, paste0(ruta_tablas, "tendencias_alpha_convergencia.csv"))
# Resultado: fosfatos correcto es plano (pendiente ~0,0003, p=0,95);
# fosfatos estrecho caía (-0,0251, p=0,002) - retractado. Automotor sube en
# ambos perímetros (+0,010 a +0,011, p<0,01) - confirmado, no retractado.

brecha_alpha <- serie_convergencia |>
  pivot_wider(names_from = sector, values_from = alpha_capital) |>
  rename(fosfatos = `Fosfatos (B08+C20)`, automotor = `Automotor (C29+C27)`) |>
  mutate(brecha_alpha = fosfatos - automotor) |> arrange(TIME_PERIOD)
brecha_alpha
write_csv(brecha_alpha, paste0(ruta_tablas, "brecha_alpha_convergencia.csv"))
# La brecha SÍ se estrecha entre 2014 y 2021 (ver la columna brecha_alpha
# de la tabla de arriba, que se imprime con los valores vigentes), pero
# exclusivamente porque automotor sube, no porque fosfatos baje. NOTA: una
# versión anterior de este comentario citaba "0,249 -> 0,196", cifras del
# perímetro v2 que ya no corresponden al cálculo actual - por eso ahora se
# remite a la tabla en vez de repetir números a mano.

g_convergencia <- ggplot(serie_convergencia, aes(x = TIME_PERIOD, y = alpha_capital, color = sector)) +
  geom_point(size = 2.5) + geom_smooth(method = "lm", se = TRUE, linewidth = 1, alpha = 0.15) +
  scale_color_manual(values = c("Fosfatos (B08+C20)" = unname(paleta_sectores["Fosfatos"]),
                                "Automotor (C29+C27)" = unname(paleta_sectores["Automotor"]))) +
  scale_x_continuous(breaks = 2014:2021) +
  labs(title = "Intensidad factorial por sector, 2014-2021",
       subtitle = "Participación del capital en el VA, con los perímetros sectoriales corregidos",
       x = "Año", y = "Alpha (participación del capital)", color = NULL,
       caption = "Fuente: OECD SUT_USEVA (Marruecos). Peso real de roca/química en OCP solo disponible para 2021 (Ronda 3); 2014-2020 usan el promedio 2021-2023 como aproximación. La brecha se estrecha porque automotor sube, no porque fosfatos baje.") +
  theme_tp1()
g_convergencia
ggsave(paste0(ruta_graficos, "grafico_convergencia_alpha.png"), g_convergencia, width = 10, height = 6.5, dpi = 300, bg = "white")


#===============================================================================#
# H - SALARIO REAL DE OCP (Ronda 3): retracta el encuadre de monopsonio
#===============================================================================#
# HALLAZGO CENTRAL: el salario efectivo que paga OCP está esencialmente EN
# LÍNEA con el producto marginal del trabajo que predice el modelo (w/VPMgL
# ~ 1,05 en el Escenario A). Eso retracta la narrativa de monopsonio - OCP
# no subpaga explotando su posición de empleador dominante - y la reemplaza
# por una de RESTRICCIÓN DE CAPACIDAD: el enigma no es "por qué paga tan
# poco" (paga 11x el salario industrial de CNSS), sino "por qué no hay más
# gente empleada ahí", y eso lo responde el techo técnico de Leontief de la
# Sección F.
#
# POR QUÉ ESTE TEST AHORA SÍ TIENE CONTENIDO (corrección de auditoría).
# Una versión anterior del script comparaba el salario contra un "VPMgL"
# construido multiplicando el VA medio por la PARTICIPACIÓN LABORAL
# observada del sector. Ese test era circular: desarrollando el álgebra,
#
#     w / VPMgL = (masa salarial / ingresos) / (participación × VA/producción)
#
# de modo que el cociente solo medía la distancia entre la participación
# laboral REAL de OCP y la que se le había SUPUESTO. Con los números de
# 2023 eso daba exactamente 0,2103/0,1600 = 1,31 - el "hallazgo" de que
# OCP pagaba 31% por encima de su producto marginal era, literalmente, el
# error de medición de la participación laboral supuesta.
#
# Con la fórmula corregida (VPMgL = (1-alpha)·VA/L, ver Script 1) el test
# deja de ser circular porque los dos lados vienen de fuentes
# independientes:
#   - (1-alpha) sale del coeficiente de capital de las cuentas nacionales
#     (HCP para el escenario A, OCDE para el B) - no usa nada de OCP;
#   - la participación laboral efectiva de OCP sale de su propia masa
#     salarial y sus propios ingresos - no usa nada de las cuentas
#     nacionales.
# Que ambas coincidan dentro de ~5% es un resultado empírico real, no una
# identidad contable: dice que el reparto del ingreso dentro de OCP es el
# que predeciría una Cobb-Douglas competitiva calibrada con el alpha
# sectorial.

salario_ocp_real <- extraer_serie("Salario medio efectivo anual (Plantilla directa) - OCP Group") |>
  rename(salario_mad_anio = valor) |>
  mutate(salario_mad_mes = salario_mad_anio / 12)
salario_ocp_real
write_csv(salario_ocp_real, paste0(ruta_tablas, "salario_ocp_real.csv"))

w_ocp_2023_mad_mes <- salario_ocp_real$salario_mad_mes[salario_ocp_real$year == anio_ref_fpp]
# VPMgL de fosfatos, punto observado (anio_ref_fpp), en ambos escenarios.
# Viene de res_A / res_B (calibrar_mfe_va(), Script 1), no de un cálculo
# aparte. vpmgl_f_obs está en millones de MAD por trabajador/año; *1e6/12
# lo pasa a MAD por trabajador/mes, igual que el resto de esta sección.
vpmgl_f_mad_mes   <- res_A$vpmgl_f_obs * 1e6 / 12
vpmgl_f_mad_mes_B <- res_B$vpmgl_f_obs * 1e6 / 12

## --- Masa salarial de OCP, derivada en el script (antes hardcodeada) ---
# OJO CON LA INDEPENDENCIA DE LAS FUENTES: la masa salarial y el salario
# medio de OCP NO son dos datos independientes. Se cumple exactamente
# masa_salarial = empleo × salario_medio_anual (verificado al centésimo
# para 2021-2023), así que una de las dos series se derivó de la otra en el
# relevamiento. Se calcula acá a partir de las dos series ya cargadas, en
# vez de transcribir números a mano, y se deja la identidad explícita para
# que nadie la use después como si fueran dos confirmaciones separadas.
masa_salarial_ocp <- inner_join(
  salario_ocp_real |> select(year, salario_mad_anio),
  ocp_empleo_serie |> select(year, empleo), by = "year"
) |>
  mutate(masa_salarial_mmad = empleo * salario_mad_anio / 1e6)
masa_salarial_ocp
write_csv(masa_salarial_ocp, paste0(ruta_tablas, "masa_salarial_ocp.csv"))

## --- EL TEST NO CIRCULAR: elasticidad tecnológica vs. reparto observado ---
# Lado izquierdo (tecnología): 1-alpha, del coeficiente de capital de las
# cuentas nacionales. Lado derecho (contabilidad de OCP): masa salarial
# sobre el VA estimado de OCP. Son dos mediciones independientes de la
# misma magnitud teórica - bajo Cobb-Douglas competitiva deberían coincidir.
va_ocp_estimado_ref <- res_A$va_f_obs   # = va_sobre_produccion × ingresos, año base
masa_salarial_ref   <- masa_salarial_ocp$masa_salarial_mmad[masa_salarial_ocp$year == anio_ref_fpp]

test_reparto_ingreso <- tibble(
  fuente = c("Elasticidad producto-trabajo, 1-alpha (Escenario A, HCP)",
             "Elasticidad producto-trabajo, 1-alpha (Escenario B, OCDE)",
             "Participación laboral efectiva de OCP (masa salarial / VA estimado)"),
  valor = c(res_A$elast_f, res_B$elast_f, masa_salarial_ref / va_ocp_estimado_ref)
) |>
  mutate(cociente_vs_observado = valor / valor[3])
test_reparto_ingreso
write_csv(test_reparto_ingreso, paste0(ruta_tablas, "test_reparto_ingreso.csv"))
# Lectura: las tres cifras caen en una banda estrecha (~0,20-0,22). La
# participación que OCP efectivamente paga coincide, dentro del margen de
# error de este tipo de calibración, con la elasticidad que predice la
# tecnología estimada desde cuentas nacionales. Eso es lo que sostiene el
# reencuadre del inciso 2d, y es un resultado, no un supuesto.

validacion_salario_real <- tibble(
  concepto = c("VPMgL predicha (Escenario A, HCP)", "VPMgL predicha (Escenario B, OCDE)",
               "Salario real efectivo de OCP",
               "Salario industrial CNSS (Industrie)", "SMIG (salario mínimo legal, referencia)"),
  mad_mes = c(vpmgl_f_mad_mes, vpmgl_f_mad_mes_B, w_ocp_2023_mad_mes,
              salarios_cnss_sector_2020$salario_mad_mes[grepl("Industrie", salarios_cnss_sector_2020$sector)],
              3120)  # SMIG de referencia, Ronda 3
) |>
  mutate(veces_vs_cnss_industrie = mad_mes / mad_mes[4],
         w_sobre_vpmgl = w_ocp_2023_mad_mes / mad_mes)
validacion_salario_real
write_csv(validacion_salario_real, paste0(ruta_tablas, "validacion_salario_real.csv"))
# Lectura: w/VPMgL queda cerca de 1 en los DOS escenarios (mirar la columna
# w_sobre_vpmgl en las dos primeras filas). OCP paga aproximadamente el
# producto marginal que predice el modelo; no hay brecha salarial "hacia
# abajo" que explicar con poder de monopsonio. El salario real es ~11 veces
# el salario industrial de CNSS y ~18 veces el SMIG.

## --- Reinterpretación: ¿qué explica entonces que no haya más empleo? ---
# Si el salario ya está en línea con la VPMgL, un modelo de monopsonio
# (Sección H de versiones anteriores, con eps/phi implícitos) deja de ser
# la explicación relevante - no hay una brecha salarial "hacia abajo" que
# explicar con poder de mercado. La restricción está del lado de la
# CANTIDAD de empleo posible, no del PRECIO que se paga por él. Esto es
# exactamente lo que muestra el gráfico Cobb-Douglas vs. Leontief de la
# Sección F: el techo técnico está apenas por encima del empleo actual, muy
# por debajo de cualquier expansión significativa - sin importar cuánto
# pague OCP, no hay mucho más margen para contratar.

L_capacidad_leontief <- res_A$L_cap_leontief

resumen_reencuadre_2d <- tibble(
  pregunta_original = "¿Por qué OCP paga poco y no hay migración laboral hacia fosfatos?",
  pregunta_corregida = "¿Por qué OCP paga muy bien (11x CNSS) y aun así no emplea mucha más gente?",
  respuesta = "Restricción tecnológica de capacidad (Leontief), no poder de monopsonio en el precio del trabajo",
  evidencia_precio = paste0("w/VPMgL = ", round(w_ocp_2023_mad_mes / vpmgl_f_mad_mes, 2),
                            " (esc. A) y ", round(w_ocp_2023_mad_mes / vpmgl_f_mad_mes_B, 2),
                            " (esc. B): el salario paga aproximadamente el producto marginal"),
  evidencia_cantidad = paste0("Techo Leontief = ", round(L_capacidad_leontief),
                              " vs. empleo observado = ", L_fosfatos_0),
  salvedad = paste0("El techo Leontief depende del año de calibración: ver ",
                    "sensibilidad_anio_ref (Script 1, 2.4b) para el rango.")
)
resumen_reencuadre_2d
write_csv(resumen_reencuadre_2d, paste0(ruta_tablas, "resumen_reencuadre_2d.csv"))

## --- La renta SÍ existe, pero no es "no pagada al trabajo" - es la renta del recurso ---
# Con el salario real ya en línea con la VPMgL del trabajo, la renta del
# factor específico (tierra/reservas geológicas) no se está "reteniendo"
# a costa del salario de los trabajadores actuales - es un excedente que
# existe PORQUE la producción está limitada por la dotación geológica/de
# capacidad, no porque se le pague poco a la gente. Ese excedente (EBITDA)
# se capta vía dividendos al Tesoro y financia la Caisse de Compensation -
# el circuito de la Sección 4b sigue siendo válido, solo cambia la
# explicación de POR QUÉ existe el excedente.

# La masa salarial ya no se transcribe a mano (antes iba hardcodeada como
# c(10550, 11615, 11518)): sale de masa_salarial_ocp, calculada más arriba
# a partir de las series de empleo y salario medio ya cargadas.
renta_real_vs_ebitda <- ocp_financieros |>
  select(year, ebitda_mmad, dividendos_mmad) |>
  left_join(masa_salarial_ocp |> select(year, masa_salarial_mmad), by = "year") |>
  mutate(masa_salarial_sobre_ebitda = masa_salarial_mmad / ebitda_mmad,
         dividendos_sobre_ebitda = dividendos_mmad / ebitda_mmad)
renta_real_vs_ebitda
write_csv(renta_real_vs_ebitda, paste0(ruta_tablas, "renta_real_vs_ebitda.csv"))


#===============================================================================#
# H bis - VALIDACIÓN DE LOS RATIOS CI/P CONTRA LA RONDA 3 (documentación)
#===============================================================================#
# La Ronda 3 trajo el dato DEFINITIVO de HCP (TRE Base 2014, año base) y
# confirmó los rangos que ya se venían usando de io_coeficientes - buena
# validación cruzada, no cambia ningún cálculo.

validacion_ci_p <- datos_investigacion |>
  filter(str_starts(variable, "Ratio Consumo Intermedio")) |>
  select(variable, valor, valor_min, valor_max, unidad, anio, fuente) |>
  arrange(variable, anio)
validacion_ci_p
write_csv(validacion_ci_p, paste0(ruta_tablas, "validacion_ci_p_ronda3.csv"))
# Minería: HCP 2014 definitivo = 43,52%; HCP 2019 = 40,5-43,5%; OCDE 2021 =
# 43,1%. Automotor: HCP 2014 = 75,0%; HCP 2019 = 72,5-76,2%; OCDE 2021 =
# 76,8%. Los tres coinciden dentro de 1-2 puntos - io_coeficientes (35-45%
# minería, 68-78% automotor) queda validado, no hace falta ajustarlo.


#===============================================================================#
# I - TRAZABILIDAD DE LAS CORRECCIONES
#===============================================================================#

trazabilidad <- tribble(
  ~version, ~parametro_fosfatos, ~parametro_automotor, ~problema_detectado,
  "v0 - HCP", "0,80 (TRE Base 2014)", "0,55 (TRE Base 2014)", "Ninguno conocido; foto fija sin serie temporal",
  "v1 - OCDE sin mezclar", "0,539 (B08, 2021)", "0,568 (C29, 2021)",
  "Perímetros mal especificados: VA de OCP 2,9x el sector; exportaciones auto > producción C29",
  "v2 - OCDE mezclado, peso sectorial (superado)", "0,755 (B08+C20, 2021)", "0,559 (C29+C27, 2021)",
  "Ponderaba por VA agregado de cada rama en la economía, no por el peso real de OCP",
  "v3 - OCDE mezclado, peso real de OCP (vigente)", "0,782 (B08+C20, 2021, Ronda 3)", "0,559 (C29+C27, 2021)",
  "Ponderado por la composición real de ingresos de OCP (roca 14% / química 86%, Ronda 3)"
)
trazabilidad
write_csv(trazabilidad, paste0(ruta_tablas, "trazabilidad_correcciones.csv"))

# Qué queda retractado de versiones anteriores:
#  1. "Convergencia por caída del alpha de fosfatos" -> FALSO con el
#     perímetro correcto. Lo que se sostiene: la brecha se estrecha, pero
#     solo porque automotor sube.
#  2. Extrapolar alpha a 2023 con la tendencia estimada -> descartado
#     (atravesaría el shock de precios 2022-2023 sin datos que lo respalden).


#===============================================================================#
# QUÉ MIRAR AL CORRER ESTE SCRIPT
#===============================================================================#
# 1. chequeo_consistencia_ocp / chequeo_consistencia_auto -> confirman que
#    los perímetros mezclados son internamente consistentes. El primero
#    tiene una sola fila (2021, único año con solapamiento OCP-OCDE).
# 2. composicion_ocp_ingresos -> el peso real de roca (12-14%) vs. química
#    (82-85%) que pondera alpha_ocp_mezclado, en vez del supuesto sectorial
#    agregado (alpha_ocp_mezclado_v0, dejado para comparar). Ojo: los pesos
#    de INGRESOS agregan va_sobre_output, y los pesos de VA (peso_va_*)
#    agregan alpha_capital y share_trabajo - son ponderadores distintos a
#    propósito.
# 3. test_consistencia_perimetro_auto -> la remuneración media implícita en
#    C29+C27 coincide con el salario CNSS declarado dentro de ~6%, lo que
#    valida el perímetro y el ajuste de empleo. (Antes se llamaba
#    test_sanidad_auto y su resultado se etiquetaba como VPMgL, que era un
#    nombre equivocado: es D1/L, una remuneración media, no un producto
#    marginal.)
# 4. comparacion_escenarios + conclusiones_robustez -> la brecha de VPMgL en
#    los dos escenarios. CITAR RANGO, NO PUNTO: la columna
#    dispersion_B_sobre_A muestra cuánto se mueve cada magnitud al cambiar
#    la fuente de parámetros, y sensibilidad_anio_ref (Script 1, 2.4b)
#    cuánto se mueve al cambiar el año base. Lo robusto es el signo y el
#    orden de magnitud, no la cifra exacta.
# 5. sensibilidad_cobertura_auto -> cota del sesgo por usar exportaciones
#    en vez de producción como valor del producto automotor: la brecha
#    "verdadera" es probablemente algo MENOR que la reportada.
# 6. tendencias / brecha_alpha -> la retractación documentada de la
#    convergencia, lado a lado con la versión que se retracta.
# 7. test_reparto_ingreso + validacion_salario_real -> EL RESULTADO CENTRAL
#    del inciso 2d, ahora con un test NO circular: la elasticidad
#    producto-trabajo estimada desde cuentas nacionales (1-alpha) y la
#    participación laboral que OCP efectivamente paga (masa salarial / VA)
#    son dos mediciones independientes que coinciden dentro de ~5%. De ahí
#    que w/VPMgL quede cerca de 1 en ambos escenarios. Retracta el encuadre
#    de monopsonio; el inciso 2d se reencuadra como restricción TÉCNICA de
#    capacidad (Leontief, Sección F), no de PRECIO del trabajo.
# 8. resumen_reencuadre_2d -> la síntesis lista para redactar el inciso.
# 9. validacion_ci_p_ronda3 -> confirma que io_coeficientes (HCP) no
#    necesita ajuste: los tres años/fuentes coinciden dentro de 1-2 puntos.