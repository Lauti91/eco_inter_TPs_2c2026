#===============================================================================#
# TP2 - MÓDULO CORREGIDO: VPMgL SOBRE VALOR AGREGADO + PREDICCIÓN DEL SALARIO
#===============================================================================#
# Sucede a 02b_TP2_ESCENARIOS_ALPHA.R y corrige tres errores metodológicos
# detectados al verificar la construcción. Correr DESPUÉS de 02b (usa
# alpha_oecd, oecd_mar, io_coeficientes y las series del bloque 0.5).
#
# ERROR 1 (menor) - Se usaba (1 - alpha_capital) como participación del
#   trabajo. Pero el VA se reparte en TRES: compensación de empleados,
#   excedente de explotación E IMPUESTOS NETOS sobre la producción. En B08
#   2021: 0,5392 + 0,4472 = 0,9865, no 1. FIX: usar share_trabajo (D1/B1G)
#   directamente, que es dato observado, en vez de derivarlo como 1-alpha.
#
# ERROR 2 (serio) - Las participaciones son sobre VALOR AGREGADO, pero se
#   multiplicaban por ingresos de OCP y exportaciones automotrices, que son
#   PRODUCCIÓN BRUTA. El ratio VA/producción difiere mucho entre sectores
#   (minería ~0,60 vs. automotor ~0,27), así que el sesgo NO se cancela:
#   inflaba la VPMgL de automotor 3,7x y la de fosfatos solo 1,67x, es
#   decir, operaba EN CONTRA de la tesis del trabajo. FIX: convertir
#   producción bruta a VA antes de aplicar la participación.
#
# ERROR 3 (el más serio, detectado después) - OCP NO es una empresa minera
#   pura: está verticalmente integrada (extracción de roca = B08 +
#   producción de fertilizantes DAP/MAP = C20 química). La mayor parte de
#   sus ingresos viene del segmento químico, no del minero. Aplicarle el
#   alpha de B08 a sus ingresos totales da un VA de OCP ~3,3x el VA de TODO
#   el sector B08 de Marruecos - imposible, OCP no puede superar al sector
#   que la contiene. FIX: construir un alpha mezclado B08+C20 ponderado por
#   VA, y chequear explícitamente la consistencia.
#
# ADVERTENCIA SOBRE LA "VALIDACIÓN" CONTRA CNSS: bajo Cobb-Douglas con
#   competencia, VPMgL = participación_trabajo × VA / L, que es
#   IDÉNTICAMENTE la compensación por trabajador si VA y L vienen del mismo
#   universo. Es decir, comparar la VPMgL contra el salario medio del mismo
#   agregado es en parte una tautología. El contraste tiene contenido
#   económico SOLO porque se combina el VA de la EMPRESA (OCP) con la
#   participación factorial del SECTOR - la diferencia entre ambos es
#   justamente lo que se quiere medir. Ver el chequeo no-tautológico de la
#   sección D.4 (renta implícita vs. EBITDA y dividendos), que usa datos
#   que NO entran en el cálculo de la VPMgL.
#
# NO ESTÁ CORRIDO NI VALIDADO EN R.


#===============================================================================#
# C.1 - ALPHA MEZCLADO PARA OCP (integración vertical B08 + C20)
#===============================================================================#

alpha_oecd_extendido <- oecd_mar |>
  filter(ACTIVITY %in% c("B08", "C20", "C29"),
         TRANSACTION %in% c("D1", "B1G", "B2A3G", "P1", "P2")) |>
  select(ACTIVITY, `Economic activity`, TRANSACTION, TIME_PERIOD, OBS_VALUE) |>
  pivot_wider(names_from = TRANSACTION, values_from = OBS_VALUE) |>
  mutate(
    alpha_capital  = B2A3G / B1G,
    share_trabajo  = D1 / B1G,          # <- ERROR 1: usar esto, no 1-alpha
    va_sobre_output = B1G / P1,          # <- ERROR 2: ratio observado, no supuesto
    impuestos_netos = 1 - alpha_capital - share_trabajo
  ) |>
  arrange(ACTIVITY, TIME_PERIOD)

alpha_oecd_extendido
write_csv(alpha_oecd_extendido, paste0(ruta_tablas, "alpha_oecd_extendido.csv"))
# NOTA: va_sobre_output sale de P1 (output) y B1G (VA) del propio dataset de
# la OCDE, así que reemplaza la aproximación que veníamos usando (el punto
# medio del rango ci_produccion de io_coeficientes). Si P1/P2 no estuvieran
# disponibles para alguna rama, el fallback es io_coeficientes.

# Alpha mezclado de OCP: promedio de B08 y C20 ponderado por VA de cada rama.
# Es una aproximación: lo correcto sería ponderar por la participación de
# CADA SEGMENTO DENTRO DE OCP (roca vs. fertilizantes), dato que no tenemos.
# Ponderar por el VA sectorial asume que OCP se reparte entre ambas ramas en
# la misma proporción que la economía marroquí - discutible, pero mucho más
# defendible que atribuirle todo a B08.
alpha_ocp_mezclado <- alpha_oecd_extendido |>
  filter(ACTIVITY %in% c("B08", "C20")) |>
  group_by(TIME_PERIOD) |>
  summarise(
    B1G_total       = sum(B1G, na.rm = TRUE),
    D1_total        = sum(D1, na.rm = TRUE),
    B2A3G_total     = sum(B2A3G, na.rm = TRUE),
    P1_total        = sum(P1, na.rm = TRUE),
    share_trabajo   = D1_total / B1G_total,
    alpha_capital   = B2A3G_total / B1G_total,
    va_sobre_output = B1G_total / P1_total,
    .groups = "drop"
  )
alpha_ocp_mezclado
write_csv(alpha_ocp_mezclado, paste0(ruta_tablas, "alpha_ocp_mezclado.csv"))


#===============================================================================#
# C.2 - CHEQUEO DE CONSISTENCIA: ¿el VA de OCP cabe dentro de su sector?
#===============================================================================#
# Este chequeo es el que destapó el ERROR 3. Se deja en el script para que
# cualquier recalibración futura vuelva a pasarlo antes de usarse.

chequeo_consistencia <- tibble(TIME_PERIOD = alpha_ocp_mezclado$TIME_PERIOD) |>
  left_join(ocp_financieros |> select(TIME_PERIOD = year, ingresos_mmad), by = "TIME_PERIOD") |>
  left_join(alpha_ocp_mezclado |> select(TIME_PERIOD, B1G_B08_C20 = B1G_total, va_sobre_output), by = "TIME_PERIOD") |>
  left_join(
    alpha_oecd_extendido |> filter(ACTIVITY == "B08") |> select(TIME_PERIOD, B1G_solo_B08 = B1G),
    by = "TIME_PERIOD"
  ) |>
  filter(!is.na(ingresos_mmad)) |>
  mutate(
    va_ocp_estimado      = ingresos_mmad * va_sobre_output,
    ratio_vs_solo_B08    = va_ocp_estimado / B1G_solo_B08,   # debería ser < 1 y NO lo es
    ratio_vs_B08_mas_C20 = va_ocp_estimado / B1G_B08_C20,    # debería ser < 1
    consistente          = ratio_vs_B08_mas_C20 < 1
  )
chequeo_consistencia
write_csv(chequeo_consistencia, paste0(ruta_tablas, "chequeo_consistencia_ocp_sector.csv"))
# Interpretación: si ratio_vs_solo_B08 > 1, confirma que atribuir todo OCP a
# minería es insostenible. Si ratio_vs_B08_mas_C20 sigue siendo > 1, ni
# siquiera el agregado minería+química alcanza, y habría que revisar el
# perímetro de ingresos de OCP (consolidado global vs. operaciones en
# Marruecos) antes de seguir.


#===============================================================================#
# C.3 - VPMgL CORREGIDA (sobre VA, con participación del trabajo observada)
#===============================================================================#

calcular_vpmgl_corregida <- function(valor_bruto, empleo, share_trab, va_output, etiqueta) {
  va <- valor_bruto * va_output
  tibble(
    concepto            = etiqueta,
    valor_bruto_mmad    = valor_bruto,
    va_sobre_output     = va_output,
    va_mmad             = va,
    empleo              = empleo,
    share_trabajo       = share_trab,
    vpmgl_mmad_anio     = share_trab * va / empleo,
    vpmgl_mad_mes       = share_trab * va / empleo * 1e6 / 12
  )
}

anio_param <- anio_oecd_ref  # 2021: último año con datos OCDE observados

par_fosfatos <- alpha_ocp_mezclado |> filter(TIME_PERIOD == anio_param)
par_auto     <- alpha_oecd_extendido |> filter(ACTIVITY == "C29", TIME_PERIOD == anio_param)

vpmgl_corregida <- bind_rows(
  calcular_vpmgl_corregida(
    valor_bruto = ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp],
    empleo      = ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref_fpp],
    share_trab  = par_fosfatos$share_trabajo,
    va_output   = par_fosfatos$va_sobre_output,
    etiqueta    = "Fosfatos (OCP, alpha mezclado B08+C20)"
  ),
  calcular_vpmgl_corregida(
    valor_bruto = auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp],
    empleo      = auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref_fpp],
    share_trab  = par_auto$share_trabajo,
    va_output   = par_auto$va_sobre_output,
    etiqueta    = "Automotor (C29)"
  )
)
vpmgl_corregida
write_csv(vpmgl_corregida, paste0(ruta_tablas, "vpmgl_corregida.csv"))

brecha_corregida <- vpmgl_corregida$vpmgl_mmad_anio[1] / vpmgl_corregida$vpmgl_mmad_anio[2]
brecha_corregida


#===============================================================================#
# D - PREDICCIÓN DEL SALARIO DEL SECTOR FOSFATOS
#===============================================================================#
# El modelo competitivo predice w = VPMgL. Para fosfatos esa predicción da
# un número absurdo frente a cualquier salario real de Marruecos. En vez de
# descartarlo como "el modelo falla", se hace lo mismo que con Leontief: se
# confronta la predicción contra modelos ALTERNATIVOS de determinación del
# salario, y se pregunta qué parámetro haría falta para racionalizar lo
# observado. Si ese parámetro resulta implausible, eso ES la evidencia de
# que el mecanismo competitivo no está operando.

## --- D.1 Cota superior empírica del salario en fosfatos ---
# No hay salario de OCP publicado ni desagregación CNSS para minería. Se
# construye una cota SUPERIOR: la masa salarial de todo el sector B08
# dividida por el empleo de OCP. Es cota superior porque el numerador cubre
# más trabajadores (todo B08) que el denominador (solo OCP).
w_fosfatos_cota_sup <- alpha_oecd_extendido |>
  filter(ACTIVITY == "B08", TIME_PERIOD == anio_param) |>
  pull(D1) /
  ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_param]

# Salario de referencia del resto de la economía formal (opción externa del
# trabajador): CNSS "Industrie", que es además el proxy del automotor.
w_industrie_anual <- salarios_cnss_sector_2020$salario_mad_mes[
  grepl("Industrie", salarios_cnss_sector_2020$sector)] * 12 / 1e6

escenarios_salario <- tibble(
  supuesto = c("Cota superior (D1 de B08 / empleo OCP)",
               "CNSS Industrie (opción externa)",
               "CNSS Industrie x 2 (prima minera moderada)",
               "CNSS Industrie x 3 (prima minera alta)"),
  w_mmad_anio = c(w_fosfatos_cota_sup, w_industrie_anual,
                  w_industrie_anual * 2, w_industrie_anual * 3)
) |>
  mutate(w_mad_mes = w_mmad_anio * 1e6 / 12)
escenarios_salario


## --- D.2 Inversión del modelo: qué parámetro racionaliza lo observado ---
# Modelo 1 - Competitivo:      w = VPMgL                      (markdown = 1)
# Modelo 2 - Monopsonio:       w = VPMgL * eps/(1+eps)  ->  eps = m/(1-m)
#            con eps = elasticidad de la oferta de trabajo que enfrenta la
#            firma. eps -> infinito reproduce competencia perfecta; eps
#            chico = poder de monopsonio fuerte. OCP es el empleador
#            dominante en Khouribga, Benguerir y Youssoufia, así que un eps
#            bajo es teóricamente plausible; la pregunta es CUÁN bajo hay
#            que ponerlo.
# Modelo 3 - Reparto de renta: w = w_ext + phi*(VPMgL - w_ext)  ->
#            phi = (w - w_ext)/(VPMgL - w_ext). phi = 1 es competencia;
#            phi = 0 significa que el trabajo no captura NADA del
#            excedente por encima de su opción externa.

vpmgl_f <- vpmgl_corregida$vpmgl_mmad_anio[1]

inversion_parametros <- escenarios_salario |>
  mutate(
    vpmgl_fosfatos_mmad = vpmgl_f,
    markdown_m          = w_mmad_anio / vpmgl_fosfatos_mmad,
    eps_implicita       = markdown_m / (1 - markdown_m),
    phi_implicita       = (w_mmad_anio - w_industrie_anual) /
      (vpmgl_fosfatos_mmad - w_industrie_anual)
  ) |>
  select(supuesto, w_mad_mes, vpmgl_fosfatos_mmad, markdown_m, eps_implicita, phi_implicita)
inversion_parametros
write_csv(inversion_parametros, paste0(ruta_tablas, "inversion_parametros_salario.csv"))
# CÓMO LEERLO: la literatura empírica de monopsonio típicamente estima
# elasticidades de oferta de trabajo a la firma del orden de 1 a 5. Si la
# eps_implicita necesaria para racionalizar el salario observado es muy
# inferior a ese rango, el modelo de monopsonio tampoco alcanza por sí solo
# y hay que apelar a la determinación administrativa del empleo (plantilla
# fijada por decisión política, no por productividad marginal) - que es
# exactamente el argumento institucional del trabajo.


## --- D.3 Curva de salario predicho según el modelo ---
grilla_salario <- tibble(eps = seq(0.05, 20, length.out = 500)) |>
  mutate(
    w_monopsonio_mmad = vpmgl_f * eps / (1 + eps),
    w_monopsonio_mes  = w_monopsonio_mmad * 1e6 / 12
  )

g_salario_predicho <- ggplot(grilla_salario, aes(x = eps, y = w_monopsonio_mes)) +
  geom_line(linewidth = 1.1, color = paleta_sectores[["Fosfatos"]]) +
  geom_hline(yintercept = vpmgl_f * 1e6 / 12, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = w_fosfatos_cota_sup * 1e6 / 12, linetype = "dotted", color = "#c1121f", linewidth = 0.9) +
  geom_hline(yintercept = w_industrie_anual * 1e6 / 12, linetype = "dotted", color = "gray25", linewidth = 0.9) +
  scale_x_log10() +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "¿Qué poder de monopsonio haría falta para explicar el salario observado?",
       subtitle = "Salario predicho por el modelo de monopsonio según la elasticidad de oferta de trabajo a la firma",
       x = "Elasticidad de oferta de trabajo a la firma (escala log)",
       y = "Salario predicho (MAD/mes)",
       caption = paste0(
         "Rayada: VPMgL (predicción competitiva, ", format(round(vpmgl_f*1e6/12), big.mark = ".", decimal.mark = ","),
         " MAD/mes). Punteada roja: cota superior empírica del salario en fosfatos. ",
         "Punteada gris: CNSS Industrie. La literatura estima elasticidades de 1 a 5."
       )) +
  theme_tp1()
g_salario_predicho
ggsave(paste0(ruta_graficos, "grafico_salario_predicho_monopsonio.png"), g_salario_predicho,
       width = 10, height = 6.5, dpi = 300, bg = "white")


## --- D.4 Chequeo NO tautológico: ¿dónde va la renta que no se paga? ---
# Este es el equivalente al chequeo de Leontief: usa datos que NO entran en
# el cálculo de la VPMgL (EBITDA, dividendos, capex) para validar que la
# renta implícita por el modelo es de una magnitud que las cuentas de OCP
# efectivamente pueden sostener. Si la renta calculada excediera al EBITDA,
# el modelo estaría inventando excedente que no existe.

L_f_ref <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref_fpp]

renta_implicita <- escenarios_salario |>
  mutate(
    vpmgl_total_mmad     = vpmgl_f * L_f_ref,              # producto marginal total atribuible al trabajo
    masa_salarial_mmad   = w_mmad_anio * L_f_ref,
    renta_no_pagada_mmad = vpmgl_total_mmad - masa_salarial_mmad,
    ebitda_ref_mmad      = ocp_financieros$ebitda_mmad[ocp_financieros$year == anio_ref_fpp],
    dividendos_ref_mmad  = ocp_financieros$dividendos_mmad[ocp_financieros$year == anio_ref_fpp],
    renta_sobre_ebitda   = renta_no_pagada_mmad / ebitda_ref_mmad,
    cabe_en_ebitda       = renta_no_pagada_mmad <= ebitda_ref_mmad
  ) |>
  select(supuesto, w_mad_mes, renta_no_pagada_mmad, ebitda_ref_mmad,
         dividendos_ref_mmad, renta_sobre_ebitda, cabe_en_ebitda)
renta_implicita
write_csv(renta_implicita, paste0(ruta_tablas, "renta_implicita_vs_ebitda.csv"))
# CÓMO LEERLO:
#  - cabe_en_ebitda = TRUE en todos los escenarios -> el modelo es
#    internamente coherente con las cuentas de la empresa: la renta que
#    predice como "no pagada al trabajo" efectivamente existe en el
#    excedente de OCP. Esto NO es tautológico: EBITDA y dividendos nunca
#    entraron en el cálculo de la VPMgL.
#  - Si además renta_no_pagada es del orden de los dividendos girados al
#    Tesoro, cierra el circuito del Bloque 4: la renta del factor
#    específico se capta vía dividendos y se recicla como subsidio al
#    consumo (Caisse de Compensation).
#  - Si cabe_en_ebitda = FALSE, el modelo está sobreestimando y hay que
#    revisar el perímetro de ingresos o el alpha antes de citar nada.


#===============================================================================#
# E - QUÉ MIRAR CUANDO ESTO CORRA
#===============================================================================#
# 1. chequeo_consistencia   -> confirma el ERROR 3 (OCP no cabe en B08) y si
#                              el agregado B08+C20 sí lo contiene.
# 2. alpha_ocp_mezclado     -> el parámetro corregido para fosfatos.
# 3. vpmgl_corregida        -> VPMgL sobre VA con participación observada,
#                              en MAD/mes para poder compararla con salarios.
# 4. inversion_parametros   -> la elasticidad y el phi que harían falta para
#                              racionalizar el salario observado. Este es el
#                              resultado central del módulo de salarios.
# 5. renta_implicita        -> el chequeo no tautológico contra EBITDA y
#                              dividendos.
# 6. g_salario_predicho     -> el gráfico del módulo de salarios.
#
# PENDIENTE DE DATOS (pedido a Spark):
#  - Salario medio efectivo de OCP (masa salarial / plantilla, estados
#    financieros). Reemplazaría la cota superior de D.1 por un dato real y
#    volvería todo el módulo D mucho más firme.
#  - Composición de los ingresos de OCP por segmento (roca vs. fertilizantes
#    procesados). Permitiría ponderar el alpha mezclado por la estructura
#    real de la empresa en vez de por el VA sectorial.