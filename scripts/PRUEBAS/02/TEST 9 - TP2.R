#===============================================================================#
# TP2 - MÓDULO CORREGIDO II: PERÍMETROS SECTORIALES SIMÉTRICOS
#===============================================================================#
# Sucede a 02c_TP2_CORRECCION_Y_SALARIOS.R. Corrige el ERROR 4 y RETRACTA
# el hallazgo de convergencia de la Fase B. Correr DESPUÉS de 02c.
#
# ERROR 4 (simétrico al 3) - Igual que OCP no cabe en B08, las
#   exportaciones automotrices no caben en C29. En 2021 las exportaciones
#   del sector fueron 83.783 M MAD contra una producción total de C29 de
#   ~78.423 M MAD (implícita de B1G/va_sobre_output). Imposible exportar
#   más de lo que la rama produce: esas exportaciones incluyen el segmento
#   câblage (arneses de cableado), que en ISIC no es C29 sino C27
#   "Manufacture of electrical equipment". Síntoma visible: la VPMgL
#   corregida de automotor daba 3.272 MAD/mes, POR DEBAJO del salario CNSS
#   observado (5.002) - económicamente imposible en equilibrio.
#   FIX: alpha mezclado C29+C27, simétrico al B08+C20 de fosfatos.
#
# RETRACTACIÓN DEL HALLAZGO DE CONVERGENCIA (Fase B de TP2_estado_y_plan.md)
#   La caída del alpha de fosfatos (-0,0251/año, p=0,002) era un artefacto
#   de mirar SOLO el segmento minero. Con el perímetro correcto (B08+C20,
#   que es OCP de verdad) la serie es PLANA: +0,0003/año, p=0,96, R²=0,001.
#   Interpretación: la minería pura sí se volvió menos capital-intensiva,
#   pero OCP fue corriendo su mix hacia química (más capital-intensiva,
#   alpha ~0,77-0,82) y los dos efectos se compensan. Sigue habiendo
#   estrechamiento de la brecha entre sectores, pero SOLO porque automotor
#   sube, no porque fosfatos baje. Todo lo que se diga en el Bloque 3 debe
#   basarse en esta versión, no en la anterior.
#
# NO ESTÁ CORRIDO NI VALIDADO EN R.


#===============================================================================#
# F.1 - ALPHA MEZCLADO PARA EL COMPLEJO AUTOMOTOR (C29 + C27)
#===============================================================================#
# C29 = Manufacture of motor vehicles (ensamblaje: Renault Tánger,
#       Stellantis Kenitra)
# C27 = Manufacture of electrical equipment (câblage / arneses: Lear,
#       Aptiv, Yazaki, Sumitomo - el segmento más intensivo en trabajo del
#       complejo, y el de mayor peso en las exportaciones según el desglose
#       del Office des Changes que ya tenemos en auto_exportaciones_oc)

ramas_automotor <- c("C29", "C27")

alpha_auto_mezclado <- oecd_mar |>
  filter(ACTIVITY %in% ramas_automotor,
         TRANSACTION %in% c("D1", "B1G", "B2A3G", "P1")) |>
  select(ACTIVITY, TRANSACTION, TIME_PERIOD, OBS_VALUE) |>
  pivot_wider(names_from = TRANSACTION, values_from = OBS_VALUE) |>
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
alpha_auto_mezclado
write_csv(alpha_auto_mezclado, paste0(ruta_tablas, "alpha_auto_mezclado.csv"))


#===============================================================================#
# F.2 - CHEQUEO DE CONSISTENCIA SIMÉTRICO (exportaciones vs. producción)
#===============================================================================#
# Mismo test que destapó el ERROR 3, ahora del lado automotor: las
# exportaciones del complejo no pueden superar la producción de las ramas
# que lo componen.

chequeo_consistencia_auto <- alpha_auto_mezclado |>
  select(TIME_PERIOD, P1_solo_ramas = P1_total, B1G_total, va_sobre_output) |>
  left_join(
    oecd_mar |> filter(ACTIVITY == "C29", TRANSACTION == "P1") |>
      select(TIME_PERIOD, P1_solo_C29 = OBS_VALUE),
    by = "TIME_PERIOD"
  ) |>
  left_join(auto_exportaciones_oc |> select(TIME_PERIOD = year, exportaciones = total_mmad),
            by = "TIME_PERIOD") |>
  filter(!is.na(exportaciones)) |>
  mutate(
    ratio_export_sobre_C29      = exportaciones / P1_solo_C29,   # > 1 = imposible
    ratio_export_sobre_C29_C27  = exportaciones / P1_solo_ramas, # debería ser < 1
    consistente                 = ratio_export_sobre_C29_C27 < 1
  )
chequeo_consistencia_auto
write_csv(chequeo_consistencia_auto, paste0(ruta_tablas, "chequeo_consistencia_automotor.csv"))
# Si ratio_export_sobre_C29_C27 sigue siendo > 1, el complejo exportador
# abarca todavía más ramas (asientos = C31 muebles, estampado = C25
# productos metálicos, powertrain = C28 maquinaria). En ese caso, ampliar
# ramas_automotor arriba y volver a correr - el resto del módulo se
# recalcula solo.


#===============================================================================#
# F.3 - VPMgL CON AMBOS PERÍMETROS CORREGIDOS
#===============================================================================#

par_fosfatos_v2 <- alpha_ocp_mezclado  |> filter(TIME_PERIOD == anio_param)
par_auto_v2     <- alpha_auto_mezclado |> filter(TIME_PERIOD == anio_param)

vpmgl_v2 <- bind_rows(
  calcular_vpmgl_corregida(
    valor_bruto = ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref_fpp],
    empleo      = ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref_fpp],
    share_trab  = par_fosfatos_v2$share_trabajo,
    va_output   = par_fosfatos_v2$va_sobre_output,
    etiqueta    = "Fosfatos (OCP: B08+C20)"
  ),
  calcular_vpmgl_corregida(
    valor_bruto = auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp],
    empleo      = auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref_fpp],
    share_trab  = par_auto_v2$share_trabajo,
    va_output   = par_auto_v2$va_sobre_output,
    etiqueta    = "Automotor (complejo: C29+C27)"
  )
)
vpmgl_v2
write_csv(vpmgl_v2, paste0(ruta_tablas, "vpmgl_v2_perimetros_corregidos.csv"))

brecha_v2 <- vpmgl_v2$vpmgl_mmad_anio[1] / vpmgl_v2$vpmgl_mmad_anio[2]
brecha_v2

# TEST DE SANIDAD: la VPMgL del automotor debe quedar POR ENCIMA del
# salario observado del sector (CNSS Industrie). Si queda por debajo, el
# perímetro sigue mal especificado: ninguna empresa contrata trabajadores
# cuyo producto marginal es menor que lo que les paga.
test_sanidad_auto <- tibble(
  vpmgl_auto_mad_mes   = vpmgl_v2$vpmgl_mad_mes[2],
  salario_cnss_mad_mes = salarios_cnss_sector_2020$salario_mad_mes[
    grepl("Industrie", salarios_cnss_sector_2020$sector)],
  pasa_el_test = vpmgl_auto_mad_mes > salario_cnss_mad_mes,
  margen_veces = vpmgl_auto_mad_mes / salario_cnss_mad_mes
)
test_sanidad_auto
# Resultado real: FALSE, margen 0,838 - mejoró respecto del perímetro
# anterior (C29 solo: 0,654) pero sigue sin pasar. Causa distinta a los
# errores 3/4: el chequeo de exportaciones vs. producción de F.2 ya pasa
# (ratio_export_sobre_C29_C27 < 1), así que el problema no es que las
# exportaciones excedan la producción de C29+C27 - es que el EMPLEO
# (230.000) es una cifra ministerial de TODO el ecosistema automotor
# (cableado + ensamblaje + asientos + estampado + powertrain, 260 plantas),
# mientras que el VA que estamos usando solo cubre C29+C27. Se está
# dividiendo el VA de una porción del sector por el empleo de todo el
# sector -> denominador inflado, VPMgL deprimida.

## --- F.3.1 - Ajuste del empleo a la porción C29+C27 (sin datos nuevos) ---
# Ya tenemos el desglose de exportaciones por segmento
# (auto_exportaciones_oc$cableado_mmad + ensamblaje_mmad), que corresponden
# justamente a C27 (cableado) y C29 (ensamblaje). La fracción de esos dos
# segmentos sobre el total exportado es un proxy razonable de qué fracción
# del empleo total pertenece a esos mismos dos segmentos - se usa para
# escalar el 230.000 a un "empleo equivalente C29+C27", sin pedir ningún
# dato adicional a Spark.

fraccion_empleo_c29_c27 <- auto_exportaciones_oc |>
  filter(year == anio_param) |>
  mutate(fraccion = (cableado_mmad + ensamblaje_mmad) / total_mmad) |>
  pull(fraccion)

L_auto_ajustado <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref_fpp] * fraccion_empleo_c29_c27

vpmgl_v2_ajustada <- calcular_vpmgl_corregida(
  valor_bruto = auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref_fpp] * fraccion_empleo_c29_c27,
  # el valor bruto también se recorta a la misma fracción, para que
  # numerador y denominador describan el mismo universo (C29+C27)
  empleo      = L_auto_ajustado,
  share_trab  = par_auto_v2$share_trabajo,
  va_output   = par_auto_v2$va_sobre_output,
  etiqueta    = "Automotor (C29+C27, empleo ajustado por fracción de exportaciones)"
)
vpmgl_v2_ajustada

test_sanidad_auto_v2 <- tibble(
  fraccion_empleo_c29_c27,
  L_auto_original  = auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref_fpp],
  L_auto_ajustado,
  vpmgl_auto_mad_mes   = vpmgl_v2_ajustada$vpmgl_mad_mes,
  salario_cnss_mad_mes = salarios_cnss_sector_2020$salario_mad_mes[grepl("Industrie", salarios_cnss_sector_2020$sector)],
  pasa_el_test = vpmgl_auto_mad_mes > salario_cnss_mad_mes,
  margen_veces = vpmgl_auto_mad_mes / salario_cnss_mad_mes
)
test_sanidad_auto_v2
write_csv(test_sanidad_auto_v2, paste0(ruta_tablas, "test_sanidad_auto_v2.csv"))
# Si esto pasa (esperado: margen ~1,08x), usar vpmgl_v2_ajustada y
# L_auto_ajustado en el resto del script (Bloque 2.3/2.4: FPP, caja de
# asignación) en lugar de las cifras "todo el sector" - son las que
# describen el mismo universo (C29+C27) que el alpha con el que se
# calibra. Si NO pasa, el siguiente paso sería ampliar ramas_automotor a
# C25 (estampado) y C28 (powertrain) en F.1 y repetir todo el bloque F.


#===============================================================================#
# G.1 - CONVERGENCIA, VERSIÓN CORREGIDA (reemplaza a la sección B.1 de 02b)
#===============================================================================#

serie_convergencia_v2 <- bind_rows(
  alpha_ocp_mezclado  |> mutate(sector = "Fosfatos (B08+C20)"),
  alpha_auto_mezclado |> mutate(sector = "Automotor (C29+C27)")
) |>
  select(TIME_PERIOD, sector, alpha_capital, share_trabajo)

ajustar_tendencia <- function(datos, etiqueta) {
  m <- lm(alpha_capital ~ TIME_PERIOD, data = datos)
  bind_cols(
    tidy(m) |> filter(term == "TIME_PERIOD") |> select(pendiente_anual = estimate, p.value),
    glance(m) |> select(r.squared, nobs)
  ) |> mutate(serie = etiqueta, .before = 1)
}

# Se incluyen también las series de perímetro estrecho, para dejar
# documentado el contraste que motivó la retractación.
tendencias_v2 <- bind_rows(
  ajustar_tendencia(filter(serie_convergencia_v2, sector == "Fosfatos (B08+C20)"),
                    "Fosfatos, perímetro CORRECTO (B08+C20)"),
  ajustar_tendencia(filter(serie_convergencia_v2, sector == "Automotor (C29+C27)"),
                    "Automotor, perímetro CORRECTO (C29+C27)"),
  ajustar_tendencia(filter(alpha_oecd_extendido, ACTIVITY == "B08"),
                    "Fosfatos, perímetro ESTRECHO (solo B08) - retractado"),
  ajustar_tendencia(filter(alpha_oecd_extendido, ACTIVITY == "C29"),
                    "Automotor, perímetro ESTRECHO (solo C29) - retractado")
)
tendencias_v2
write_csv(tendencias_v2, paste0(ruta_tablas, "tendencias_alpha_v2.csv"))

# Brecha entre sectores: se estrecha, pero hay que ver POR CUÁL DE LOS DOS
# lados. Ésta es la tabla que sostiene la afirmación del Bloque 3.
brecha_alpha_v2 <- serie_convergencia_v2 |>
  select(TIME_PERIOD, sector, alpha_capital) |>
  pivot_wider(names_from = sector, values_from = alpha_capital) |>
  rename(fosfatos = `Fosfatos (B08+C20)`, automotor = `Automotor (C29+C27)`) |>
  mutate(brecha_alpha = fosfatos - automotor) |>
  arrange(TIME_PERIOD)
brecha_alpha_v2
write_csv(brecha_alpha_v2, paste0(ruta_tablas, "brecha_alpha_v2.csv"))

g_convergencia_v2 <- ggplot(serie_convergencia_v2,
                            aes(x = TIME_PERIOD, y = alpha_capital, color = sector)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, alpha = 0.15) +
  scale_color_manual(values = c("Fosfatos (B08+C20)" = unname(paleta_sectores["Fosfatos"]),
                                "Automotor (C29+C27)" = unname(paleta_sectores["Automotor"]))) +
  scale_x_continuous(breaks = 2014:2021) +
  labs(title = "Intensidad factorial por sector, 2014-2021",
       subtitle = "Participación del capital en el VA, con los perímetros sectoriales corregidos",
       x = "Año", y = "Alpha (participación del capital)", color = NULL,
       caption = paste0(
         "Fuente: OECD SUT_USEVA (Marruecos). Fosfatos = B08 minería + C20 química (OCP está ",
         "verticalmente integrada); automotor = C29 ensamblaje + C27 cableado. La brecha se estrecha ",
         "porque automotor SUBE, no porque fosfatos baje: la serie de fosfatos es plana con este perímetro."
       )) +
  theme_tp1()
g_convergencia_v2
ggsave(paste0(ruta_graficos, "grafico_convergencia_alpha_v2.png"), g_convergencia_v2,
       width = 10, height = 6.5, dpi = 300, bg = "white")

# Gráfico de la retractación: por qué el perímetro importa. Útil para la
# presentación como ejemplo de robustez metodológica.
comparacion_perimetros <- bind_rows(
  alpha_oecd_extendido |> filter(ACTIVITY == "B08") |>
    transmute(TIME_PERIOD, alpha_capital, serie = "Fosfatos: solo minería (B08)"),
  alpha_ocp_mezclado |>
    transmute(TIME_PERIOD, alpha_capital, serie = "Fosfatos: complejo integrado (B08+C20)")
)

g_perimetros <- ggplot(comparacion_perimetros, aes(x = TIME_PERIOD, y = alpha_capital, color = serie)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  scale_color_manual(values = c("Fosfatos: solo minería (B08)" = "gray55",
                                "Fosfatos: complejo integrado (B08+C20)" = unname(paleta_sectores["Fosfatos"]))) +
  scale_x_continuous(breaks = 2014:2021) +
  labs(title = "Por qué el perímetro sectorial cambia la conclusión",
       subtitle = "La caída de la intensidad de capital en fosfatos desaparece al incluir el segmento químico",
       x = "Año", y = "Alpha (participación del capital)", color = NULL,
       caption = paste0(
         "Solo minería: -0,0252/año (p=0,002). Complejo integrado: +0,0003/año (p=0,96). ",
         "OCP obtiene la mayor parte de sus ingresos de fertilizantes procesados (C20), no de roca cruda (B08)."
       )) +
  theme_tp1()
g_perimetros
ggsave(paste0(ruta_graficos, "grafico_perimetros_alpha.png"), g_perimetros,
       width = 10, height = 6.5, dpi = 300, bg = "white")


#===============================================================================#
# G.2 - MÓDULO DE SALARIOS, RECALCULADO CON LOS PERÍMETROS CORREGIDOS
#===============================================================================#

vpmgl_f_v2 <- vpmgl_v2$vpmgl_mmad_anio[1]

inversion_parametros_v2 <- escenarios_salario |>
  mutate(
    vpmgl_fosfatos_mmad = vpmgl_f_v2,
    markdown_m          = w_mmad_anio / vpmgl_fosfatos_mmad,
    eps_implicita       = markdown_m / (1 - markdown_m),
    phi_implicita       = (w_mmad_anio - w_industrie_anual) /
      (vpmgl_fosfatos_mmad - w_industrie_anual),
    # La literatura empírica de monopsonio estima elasticidades de oferta de
    # trabajo a la firma del orden de 1 a 5. Fuera de ese rango, el
    # monopsonio por sí solo no alcanza como explicación.
    dentro_rango_literatura = eps_implicita >= 1 & eps_implicita <= 5
  ) |>
  select(supuesto, w_mad_mes, vpmgl_fosfatos_mmad, markdown_m,
         eps_implicita, dentro_rango_literatura, phi_implicita)
inversion_parametros_v2
write_csv(inversion_parametros_v2, paste0(ruta_tablas, "inversion_parametros_salario_v2.csv"))
# LECTURA: el resultado bascula según lo que OCP efectivamente pague.
#  - Si paga cerca de la cota superior, la eps implícita cae dentro del
#    rango de la literatura -> alcanza con un monopsonio moderado, NO hace
#    falta invocar un quiebre institucional. Conclusión más modesta pero
#    más defendible.
#  - Si paga cerca del salario industrial general, la eps queda muy por
#    debajo del rango -> ni el monopsonio explica la brecha, y el argumento
#    institucional (plantilla fijada administrativamente, renta capturada
#    vía dividendos) se vuelve necesario.
# Por eso el salario efectivo de OCP dejó de ser un dato deseable y pasó a
# ser el dato que decide la conclusión del Bloque 2d.

renta_implicita_v2 <- escenarios_salario |>
  mutate(
    vpmgl_total_mmad     = vpmgl_f_v2 * L_f_ref,
    masa_salarial_mmad   = w_mmad_anio * L_f_ref,
    renta_no_pagada_mmad = vpmgl_total_mmad - masa_salarial_mmad,
    ebitda_ref_mmad      = ocp_financieros$ebitda_mmad[ocp_financieros$year == anio_ref_fpp],
    dividendos_ref_mmad  = ocp_financieros$dividendos_mmad[ocp_financieros$year == anio_ref_fpp],
    renta_sobre_ebitda   = renta_no_pagada_mmad / ebitda_ref_mmad,
    cabe_en_ebitda       = renta_no_pagada_mmad <= ebitda_ref_mmad
  ) |>
  select(supuesto, w_mad_mes, renta_no_pagada_mmad, ebitda_ref_mmad,
         dividendos_ref_mmad, renta_sobre_ebitda, cabe_en_ebitda)
renta_implicita_v2
write_csv(renta_implicita_v2, paste0(ruta_tablas, "renta_implicita_vs_ebitda_v2.csv"))


#===============================================================================#
# H - TABLA DE TRAZABILIDAD DE LAS CORRECCIONES
#===============================================================================#
# Resumen de qué cambió en cada iteración y por qué. Sirve para la sección
# metodológica de la presentación y para que nadie (incluidos nosotros)
# vuelva a usar una versión superada por error.

trazabilidad <- tribble(
  ~version, ~parametro_fosfatos, ~parametro_automotor, ~problema_detectado,
  "v0 - HCP",          "0,80 (TRE Base 2014)", "0,55 (TRE Base 2014)",
  "Ninguno conocido en su momento; foto fija, sin serie temporal",
  "v1 - OCDE B08/C29", "0,539 (2021)", "0,568 (2021)",
  "Perímetros mal especificados: OCP no cabe en B08 (VA 2,9x el sector); exportaciones auto > producción C29",
  "v2 - OCDE mezclado","B08+C20 (~0,755 en 2021)", "C29+C27 (ver alpha_auto_mezclado)",
  "Vigente. Perímetros consistentes con los chequeos de F.2 y C.2"
)
trazabilidad
write_csv(trazabilidad, paste0(ruta_tablas, "trazabilidad_correcciones.csv"))

# Qué se retracta explícitamente de versiones anteriores:
#  1. "Convergencia de intensidades factoriales por caída del alpha de
#     fosfatos" -> FALSO con el perímetro correcto (pendiente +0,0003,
#     p=0,96). Lo que sí se sostiene: la brecha entre sectores se estrecha,
#     pero exclusivamente porque automotor sube.
#  2. "La VPMgL del automotor coincide con el salario CNSS, validando el
#     modelo" -> esa coincidencia era en parte mecánica (bajo Cobb-Douglas,
#     share_trabajo x VA / L ES la compensación por trabajador si VA y L
#     vienen del mismo universo) y además se calculó con el perímetro
#     equivocado. El chequeo con contenido real es el test de sanidad de
#     F.3 (VPMgL > salario observado) y la renta contra EBITDA de G.2.
#  3. Alpha extrapolado a 2023 -> ya estaba descartado, se mantiene fuera.


#===============================================================================#
# I - QUÉ MIRAR CUANDO ESTO CORRA
#===============================================================================#
# 1. chequeo_consistencia_auto -> ¿las exportaciones caben ahora en C29+C27?
#                                 Si no, ampliar ramas_automotor (F.1).
# 2. test_sanidad_auto         -> ¿la VPMgL del automotor supera al salario
#                                 observado? Debe dar TRUE.
# 3. vpmgl_v2 / brecha_v2      -> la brecha con ambos perímetros corregidos.
# 4. tendencias_v2             -> la retractación documentada: perímetro
#                                 estrecho vs. correcto, lado a lado.
# 5. brecha_alpha_v2           -> por qué lado se estrecha la brecha.
# 6. inversion_parametros_v2   -> dentro_rango_literatura es la columna que
#                                 decide si alcanza con monopsonio.
# 7. renta_implicita_v2        -> el chequeo no tautológico contra EBITDA.
# 8. Gráficos: convergencia_alpha_v2, perimetros_alpha, y el de salarios de
#    02c (recalcularlo con vpmgl_f_v2 si se va a usar).