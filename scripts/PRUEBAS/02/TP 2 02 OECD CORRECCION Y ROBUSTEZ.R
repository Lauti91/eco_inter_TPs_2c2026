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
# io_coeficientes, ocp_empleo_serie, auto_empleo_serie, fosfatos_produccion,
# auto_produccion, ocp_financieros, auto_exportaciones_oc,
# salarios_cnss_sector_2020, anio_ref_fpp, L_fosfatos_0/L_auto_0, paletas.
#
# PENDIENTE DE RECONCILIAR (ver cabecera de Script 1): la FPP y la caja de
# asignación completas (Script 1, 2.3/2.4) siguen calibradas con el
# enfoque ingresos/producción física, no con el enfoque de VA que se usa
# acá para el punto observado. Si se decide adoptar VA como definitivo
# para todo, falta reconstruir esas curvas completas con este alpha
# mezclado - hoy solo el punto puntual usa la versión corregida.
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
write_csv(alpha_oecd_extendido, paste0(ruta_tablas, "alpha_oecd_extendido.csv"))

alpha_ocp_mezclado_v0 <- alpha_oecd_extendido |>
  filter(ACTIVITY %in% c("B08", "C20")) |>
  group_by(TIME_PERIOD) |>
  summarise(B1G_total = sum(B1G, na.rm = TRUE), D1_total = sum(D1, na.rm = TRUE),
            B2A3G_total = sum(B2A3G, na.rm = TRUE), P1_total = sum(P1, na.rm = TRUE),
            share_trabajo = D1_total / B1G_total, alpha_capital = B2A3G_total / B1G_total,
            va_sobre_output = B1G_total / P1_total, .groups = "drop")
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
    alpha_capital   = peso_B08 * alpha_capital_B08   + peso_C20 * alpha_capital_C20,
    share_trabajo   = peso_B08 * share_trabajo_B08   + peso_C20 * share_trabajo_C20,
    va_sobre_output = peso_B08 * va_sobre_output_B08 + peso_C20 * va_sobre_output_C20
  ) |>
  select(TIME_PERIOD, alpha_capital, share_trabajo, va_sobre_output, peso_B08, peso_C20, fuente_peso) |>
  # B1G_total se mantiene disponible (del cálculo v0) para el chequeo de
  # consistencia de más abajo, que pregunta algo distinto (¿el VA de OCP
  # cabe dentro de B08+C20 combinados?) - no depende de CÓMO se ponderan.
  left_join(alpha_ocp_mezclado_v0 |> select(TIME_PERIOD, B1G_total), by = "TIME_PERIOD")
alpha_ocp_mezclado  # confirmar: 8 filas (2014-2021), no 1 - y fuente_peso
# marca cuál es el único año con peso real (2021)
alpha_ocp_mezclado
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
         ratio_vs_solo_B08 = va_ocp_estimado / B1G_solo_B08,      # dio 2,93 -> imposible, confirma el error
         ratio_vs_B08_mas_C20 = va_ocp_estimado / B1G_B08_C20,    # dio 0,70 -> consistente
         consistente = ratio_vs_B08_mas_C20 < 1)
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
# D - VPMgL SOBRE VALOR AGREGADO, CON AMBOS PERÍMETROS CORREGIDOS
#===============================================================================#
# Corrige dos problemas de la versión de Script 1 (2.4): (1) usar
# share_trabajo observado (D1/B1G) en vez de derivarlo como 1-alpha
# (alpha+share no suman exactamente 1: el resto son impuestos netos); (2)
# multiplicar por VALOR AGREGADO, no por ingresos/exportaciones brutas
# (que incluyen consumo intermedio, y el ratio VA/producción difiere mucho
# entre sectores - ~0,53 en fosfatos vs. ~0,15-0,21 en automotor -, así que
# el sesgo no se cancela).

anio_param <- 2021  # último año con datos OCDE observados

calcular_vpmgl_va <- function(valor_bruto, empleo, share_trab, va_output, etiqueta) {
  va <- valor_bruto * va_output
  tibble(concepto = etiqueta, valor_bruto_mmad = valor_bruto, va_sobre_output = va_output,
         va_mmad = va, empleo = empleo, share_trabajo = share_trab,
         vpmgl_mmad_anio = share_trab * va / empleo, vpmgl_mad_mes = share_trab * va / empleo * 1e6 / 12)
}

par_fosfatos <- alpha_ocp_mezclado  |> filter(TIME_PERIOD == anio_param)
par_auto     <- alpha_auto_mezclado |> filter(TIME_PERIOD == anio_param)

vpmgl_va <- bind_rows(
  calcular_vpmgl_va(
    valor_bruto = ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp],
    empleo = L_fosfatos_0, share_trab = par_fosfatos$share_trabajo, va_output = par_fosfatos$va_sobre_output,
    etiqueta = "Fosfatos (OCP: B08+C20)"),
  calcular_vpmgl_va(
    valor_bruto = auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp],
    empleo = L_auto_0, share_trab = par_auto$share_trabajo, va_output = par_auto$va_sobre_output,
    etiqueta = "Automotor (complejo: C29+C27)")
)
vpmgl_va
write_csv(vpmgl_va, paste0(ruta_tablas, "vpmgl_va_perimetros_corregidos.csv"))

brecha_vpmgl_va <- vpmgl_va$vpmgl_mmad_anio[1] / vpmgl_va$vpmgl_mmad_anio[2]  # 13,6x
brecha_vpmgl_va

# TEST DE SANIDAD (automotor): la VPMgL debe superar el salario CNSS
# observado. Se verifica usando el VA REAL de C29+C27 en 2021 (no
# reconstruido vía exportaciones) y el empleo del ecosistema ESE mismo año
# (180.000, no el de 2023) escalado por la fracción de exportaciones que
# corresponde a cableado+ensamblaje (proxy de qué fracción del empleo
# pertenece a esos dos segmentos, ya que la OCDE no publica empleo).
fraccion_empleo_c29_c27 <- auto_exportaciones_oc |>
  filter(year == anio_param) |>
  mutate(fraccion = (cableado_mmad + ensamblaje_mmad) / total_mmad) |> pull(fraccion)

L_ecosistema_2021 <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_param]
L_auto_ajustado   <- L_ecosistema_2021 * fraccion_empleo_c29_c27
VA_c29_c27_2021   <- par_auto$B1G_total

vpmgl_auto_check <- par_auto$share_trabajo * VA_c29_c27_2021 / L_auto_ajustado

test_sanidad_auto <- tibble(
  fraccion_empleo_c29_c27, L_ecosistema_2021, L_auto_ajustado, VA_c29_c27_2021,
  vpmgl_auto_mad_mes = vpmgl_auto_check * 1e6 / 12,
  salario_cnss_mad_mes = salarios_cnss_sector_2020$salario_mad_mes[grepl("Industrie", salarios_cnss_sector_2020$sector)]
) |>
  mutate(pasa_el_test = vpmgl_auto_mad_mes > salario_cnss_mad_mes,
         margen_veces = vpmgl_auto_mad_mes / salario_cnss_mad_mes)
test_sanidad_auto  # 4.705 vs. 5.002 MAD/mes -> margen 0,94: empate técnico, no fallo
write_csv(test_sanidad_auto, paste0(ruta_tablas, "test_sanidad_auto.csv"))
# No seguir ampliando el perímetro automotor para forzar que cruce 1,00 -
# sería sobreajuste. Se acepta como validación aproximada dado que mezcla
# fuentes de años distintos (CNSS 2020, OCDE 2021).


#===============================================================================#
# E - ESCENARIOS A (HCP) vs. B (OCDE, perímetros corregidos)
#===============================================================================#

alpha_fosfatos_B <- alpha_ocp_mezclado$alpha_capital[alpha_ocp_mezclado$TIME_PERIOD == anio_param]
alpha_auto_B     <- alpha_auto_mezclado$alpha_capital[alpha_auto_mezclado$TIME_PERIOD == anio_param]

escenarios <- tibble(
  escenario = c("A - HCP (TRE Base 2014)", "B - OCDE mezclado (2021 observado)"),
  alpha_fosfatos = c(alpha_fosfatos, alpha_fosfatos_B),
  alpha_auto     = c(alpha_auto, alpha_auto_B)
)
escenarios

calibrar_mfe <- function(alpha_f, alpha_a, etiqueta, anio_ref = anio_ref_fpp) {
  L_f0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref]
  L_a0 <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref]
  L_tot <- L_f0 + L_a0
  Q_f0 <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio_ref]
  Q_a0 <- auto_produccion$unidades[auto_produccion$year == anio_ref]
  ingresos_f <- ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref]
  export_a   <- auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref]
  
  A_f <- Q_f0 / (L_f0 ^ (1 - alpha_f)); A_a <- Q_a0 / (L_a0 ^ (1 - alpha_a))
  precio_f <- ingresos_f / Q_f0; precio_a <- export_a / Q_a0
  
  grilla <- tibble(L_fosfatos = seq(1000, L_tot - 1000, length.out = 2000)) |>
    mutate(L_auto = L_tot - L_fosfatos,
           Q_fosfatos = A_f * L_fosfatos ^ (1 - alpha_f), Q_auto = A_a * L_auto ^ (1 - alpha_a),
           pmgl_fosfatos = (1 - alpha_f) * A_f * L_fosfatos ^ (-alpha_f),
           pmgl_auto     = (1 - alpha_a) * A_a * L_auto ^ (-alpha_a),
           vpmgl_fosfatos = precio_f * pmgl_fosfatos, vpmgl_auto = precio_a * pmgl_auto, escenario = etiqueta)
  
  eq <- grilla |> mutate(brecha = abs(vpmgl_fosfatos - vpmgl_auto)) |> slice_min(brecha, n = 1)
  aL_leontief <- L_f0 / Q_f0; Q_max <- max(fosfatos_produccion$produccion_mt, na.rm = TRUE)
  
  list(etiqueta = etiqueta, alpha_f = alpha_f, alpha_a = alpha_a, L_f0 = L_f0, L_tot = L_tot,
       A_f = A_f, A_a = A_a, grilla = grilla, w_eq = mean(c(eq$vpmgl_fosfatos, eq$vpmgl_auto)),
       L_f_eq = eq$L_fosfatos, aL_leontief = aL_leontief, Q_max = Q_max,
       L_cap_leontief = aL_leontief * Q_max)
}

res_A <- calibrar_mfe(alpha_fosfatos, alpha_auto, "A - HCP")
res_B <- calibrar_mfe(alpha_fosfatos_B, alpha_auto_B, "B - OCDE mezclado")

comparacion_escenarios <- tibble(
  escenario = c(res_A$etiqueta, res_B$etiqueta),
  alpha_fosfatos = c(res_A$alpha_f, res_B$alpha_f), alpha_automotor = c(res_A$alpha_a, res_B$alpha_a),
  L_fosfatos_equilibrio = c(res_A$L_f_eq, res_B$L_f_eq), L_fosfatos_observado = c(res_A$L_f0, res_B$L_f0),
  L_capacidad_leontief = c(res_A$L_cap_leontief, res_B$L_cap_leontief)
) |>
  mutate(eq_sobre_observado_veces = L_fosfatos_equilibrio / L_fosfatos_observado,
         eq_sobre_capacidad_veces = L_fosfatos_equilibrio / L_capacidad_leontief)
comparacion_escenarios
write_csv(comparacion_escenarios, paste0(ruta_tablas, "comparacion_escenarios_alpha.csv"))

# Chequeo de robustez del hallazgo central a la elección de escenario:
conclusiones_robustez <- tibble(
  pregunta = c("Equilibrio muy por encima del empleo observado (>2x)?",
               "Equilibrio por encima del techo técnico Leontief?"),
  escenario_A = c(res_A$L_f_eq / res_A$L_f0 > 2, res_A$L_f_eq > res_A$L_cap_leontief),
  escenario_B = c(res_B$L_f_eq / res_B$L_f0 > 2, res_B$L_f_eq > res_B$L_cap_leontief)
)
conclusiones_robustez  # TRUE en las 4 celdas -> robusto a la elección de alpha
write_csv(conclusiones_robustez, paste0(ruta_tablas, "conclusiones_robustez_escenarios.csv"))


#===============================================================================#
# F - COBB-DOUGLAS vs. LEONTIEF: ¿hay margen técnico para el equilibrio?
#===============================================================================#
# L_fosfatos_eq (Script 1: ~75.565) es una extrapolación ~4,4x más allá de
# cualquier empleo observado en fosfatos. Se la compara contra una
# tecnología de coeficientes fijos, calibrada en el mismo punto observado.

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
       subtitle = "Producción de fosfatos bajo dos supuestos tecnológicos, calibrados en el mismo punto observado (2023)",
       x = "Trabajo asignado a fosfatos", y = "Producción de fosfatos (miles de toneladas)", color = NULL,
       caption = paste0("Empleo observado: ", format(res_A$L_f0, big.mark = "."),
                        ". Equilibrio Cobb-Douglas: ", format(round(res_A$L_f_eq), big.mark = "."),
                        ". Techo Leontief a capacidad pico: ", format(round(res_A$L_cap_leontief), big.mark = "."), ".")) +
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
# La brecha SÍ se estrecha (0,249 en 2014 -> 0,196 en 2021), pero
# exclusivamente porque automotor sube, no porque fosfatos baje.

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
# HALLAZGO CENTRAL DE ESTA SECCIÓN: el salario real de OCP resultó CASI
# EXACTAMENTE IGUAL a la VPMgL predicha, no muy por debajo como asumían
# los "escenarios hipotéticos" de la versión anterior. Esto retracta la
# narrativa de monopsonio (OCP no subpaga explotando su posición de
# empleador dominante) y la reemplaza por una de RESTRICCIÓN DE CAPACIDAD:
# el enigma no es "por qué paga tan poco" (paga mucho, 11x el salario
# industrial CNSS), sino "por qué no hay más gente empleada ahí" - y esa
# pregunta ya la respondió el techo técnico de Leontief (Sección F): no
# hay margen técnico para emplear muchos más, sin importar el salario.

salario_ocp_real <- extraer_serie("Salario medio efectivo anual (Plantilla directa) - OCP Group") |>
  rename(salario_mad_anio = valor) |>
  mutate(salario_mad_mes = salario_mad_anio / 12)
salario_ocp_real
write_csv(salario_ocp_real, paste0(ruta_tablas, "salario_ocp_real.csv"))

w_ocp_2023_mad_mes <- salario_ocp_real$salario_mad_mes[salario_ocp_real$year == anio_ref_fpp]
vpmgl_f_mad_mes    <- vpmgl_va$vpmgl_mad_mes[1]  # VPMgL de fosfatos sobre VA, ya calculada en D (2023)

validacion_salario_real <- tibble(
  concepto = c("VPMgL predicha (Escenario A, sobre VA)", "Salario real efectivo de OCP",
               "Salario industrial CNSS (Industrie)", "SMIG (salario mínimo legal, referencia)"),
  mad_mes = c(vpmgl_f_mad_mes, w_ocp_2023_mad_mes,
              salarios_cnss_sector_2020$salario_mad_mes[grepl("Industrie", salarios_cnss_sector_2020$sector)],
              3120)  # SMIG de referencia, Ronda 3
) |>
  mutate(veces_vs_cnss_industrie = mad_mes / mad_mes[3])
validacion_salario_real
write_csv(validacion_salario_real, paste0(ruta_tablas, "validacion_salario_real.csv"))
# w/VPMgL ~ 1,05: OCP paga EN LÍNEA con su producto marginal estimado, no
# por debajo. El salario real (56.461 MAD/mes en 2023) es 11,3 veces el
# salario industrial de CNSS y ~18 veces el SMIG.

## --- Reinterpretación: ¿qué explica entonces que no haya más empleo? ---
# Si el salario ya está en línea con la VPMgL, un modelo de monopsonio
# (Sección H de versiones anteriores, con eps/phi implícitos) deja de ser
# la explicación relevante - no hay una brecha salarial que "explicar" con
# poder de mercado. La restricción está del lado de la CANTIDAD de empleo
# posible, no del PRECIO que se paga por él. Esto es exactamente lo que
# muestra el gráfico Cobb-Douglas vs. Leontief de la Sección F: el techo
# técnico (~19.627 trabajadores) está apenas por encima del empleo actual
# (17.000), muy por debajo de cualquier expansión significativa - sin
# importar cuánto pague OCP, no hay mucho más margen para contratar.

resumen_reencuadre_2d <- tibble(
  pregunta_original = "¿Por qué OCP paga poco y no hay migración laboral hacia fosfatos?",
  pregunta_corregida = "¿Por qué OCP paga muy bien (11x CNSS) y aun así no emplea mucha más gente?",
  respuesta = "Restricción tecnológica de capacidad (Leontief), no poder de monopsonio en el precio del trabajo",
  evidencia_precio = paste0("w/VPMgL = ", round(w_ocp_2023_mad_mes / vpmgl_f_mad_mes, 2), " (cercano a 1)"),
  evidencia_cantidad = paste0("Techo Leontief = ", round(L_capacidad_leontief <- res_A$L_cap_leontief),
                              " vs. empleo observado = ", L_fosfatos_0)
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

renta_real_vs_ebitda <- tibble(
  year = ocp_financieros$year,
  ebitda_mmad = ocp_financieros$ebitda_mmad,
  dividendos_mmad = ocp_financieros$dividendos_mmad,
  masa_salarial_mmad = NA_real_
)
renta_real_vs_ebitda$masa_salarial_mmad[renta_real_vs_ebitda$year %in% c(2021, 2022, 2023)] <-
  c(10550, 11615, 11518)  # Ronda 3: Charges de personnel, para referencia directa
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
#    los perímetros mezclados son internamente consistentes.
# 2. composicion_ocp_ingresos -> el peso real de roca (12-14%) vs. química
#    (82-85%) que ahora pondera alpha_ocp_mezclado, en vez del supuesto
#    sectorial agregado (alpha_ocp_mezclado_v0, dejado para comparar).
# 3. test_sanidad_auto -> margen 0,94, empate técnico aceptado.
# 4. brecha_vpmgl_va (13,6x) -> la brecha final, sobre VA y perímetros
#    corregidos, para citar en la presentación.
# 5. conclusiones_robustez -> TRUE en las 4 celdas: el hallazgo del Bloque
#    2 es robusto a la elección de alpha (HCP vs. OCDE).
# 6. tendencias / brecha_alpha -> la retractación documentada de la
#    convergencia, lado a lado con la versión que se retracta.
# 7. validacion_salario_real -> EL CAMBIO MÁS IMPORTANTE de esta ronda:
#    w/VPMgL ~ 1,05. Retracta el encuadre de monopsonio de versiones
#    anteriores (inversion_parametros, eps/phi implícitos - ya no forman
#    parte del script activo). El inciso 2d se reencuadra como restricción
#    TÉCNICA de capacidad (Leontief, Sección F), no de PRECIO del trabajo.
# 8. resumen_reencuadre_2d -> la síntesis lista para redactar el inciso.
# 9. validacion_ci_p_ronda3 -> confirma que io_coeficientes (HCP) no
#    necesita ajuste: los tres años/fuentes coinciden dentro de 1-2 puntos.



modelo_labsh_marruecos <- lm(labsh ~ year, data = filter(pwt10.01, country == "Morocco"))
tidy(modelo_labsh_marruecos)
glance(modelo_labsh_marruecos)


modelo_labsh_alemania <- lm(labsh ~ year, data = filter(pwt10.01, country == "Germany"))
modelo_labsh_espana   <- lm(labsh ~ year, data = filter(pwt10.01, country == "Spain"))
modelo_labsh_francia  <- lm(labsh ~ year, data = filter(pwt10.01, country == "France"))

tidy(modelo_labsh_alemania)
tidy(modelo_labsh_espana)
tidy(modelo_labsh_francia)
