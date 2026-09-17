#===============================================================================#
# TP2 - BLOQUE EMPÍRICO: ESCENARIOS DE CALIBRACIÓN Y CONVERGENCIA DE ALPHA
#===============================================================================#
# Corresponde a las Fases A y B de TP2_estado_y_plan.md.
#
# QUÉ RESUELVE ESTE BLOQUE:
#   Fase A - La calibración del MFE (FPP, caja de asignación, Leontief)
#            depende del parámetro alpha (participación del factor específico
#            en el VA). Hay dos fuentes que NO coinciden para fosfatos:
#              Escenario A: HCP (TRE Base 2014)  -> 0,80 fosfatos / 0,55 auto
#              Escenario B: OCDE SUT_USEVA 2021  -> 0,539 fosfatos / 0,568 auto
#            En vez de elegir a dedo, se recalcula TODO bajo ambos y se
#            comparan los resultados lado a lado en una sola tabla.
#   Fase B - La serie OCDE 2014-2021 muestra convergencia de intensidades
#            entre sectores (fosfatos cae, automotor sube, ambas
#            significativas). Eso es material del Bloque 3 (HO), no un
#            problema del Bloque 2.
#
# ORDEN DE EJECUCIÓN: correr DESPUÉS del bloque 0.5 (carga del Excel de
# investigación) y del sub-bloque 2.1 del script principal, porque usa
# ocp_empleo_serie, auto_empleo_serie, fosfatos_produccion, auto_produccion,
# ocp_financieros, auto_exportaciones_oc e io_coeficientes.
#
# NO ESTÁ CORRIDO NI VALIDADO EN R (se escribió sin acceso a R para
# probarlo). Revisar en particular la descarga de la OCDE (A.0), que puede
# fallar por cambios en el endpoint o por el 403 que ya apareció con rsdmx.

paquetes_fase_ab <- c("broom", "readr", "httr")
for (pkg in paquetes_fase_ab) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg, repos = "https://cloud.r-project.org")
}
library(broom)


#===============================================================================#
# A.0 - DESCARGA Y PARSEO DE LA SERIE OCDE (SUT_USEVA, Marruecos 2014-2021)
#===============================================================================#
# El endpoint SDMX con filtro de país devolvió 403 vía rsdmx; el CSV completo
# sin filtro sí funciona (se filtra después en R). Se cachea en disco para no
# volver a bajarlo en cada corrida.

ruta_cache_oecd <- paste0(ruta_tablas, "oecd_useva_marruecos.csv")

if (file.exists(ruta_cache_oecd)) {
  oecd_mar <- read_csv(ruta_cache_oecd, show_col_types = FALSE)
} else {
  url_useva <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NASU@DF_USEVA_T1600,1.0/all?format=csvfilewithlabels"
  datos_useva <- read_csv(url_useva, show_col_types = FALSE)
  oecd_mar <- datos_useva |> filter(REF_AREA == "MAR")
  write_csv(oecd_mar, ruta_cache_oecd)
  rm(datos_useva)  # el archivo completo es grande y no se vuelve a usar
}

# Códigos relevantes (confirmados contra el dataset real):
#   B    = Mining and quarrying (agregado)
#   B08  = Other mining and quarrying <- fosfatos (mineral NO metálico;
#          B07 "mining of metal ores" es plata/cobalto, no OCP)
#   C29  = Manufacture of motor vehicles, trailers and semi-trailers
#   B1G  = Value added, gross | D1 = Compensation of employees
#   B2A3G = Operating surplus and mixed income, gross
alpha_oecd <- oecd_mar |>
  filter(ACTIVITY %in% c("B", "B08", "C29"),
         TRANSACTION %in% c("D1", "B1G", "B2A3G")) |>
  select(ACTIVITY, `Economic activity`, TRANSACTION, TIME_PERIOD, OBS_VALUE) |>
  pivot_wider(names_from = TRANSACTION, values_from = OBS_VALUE) |>
  mutate(
    alpha_capital = B2A3G / B1G,
    share_trabajo = D1 / B1G,
    sector = case_when(ACTIVITY %in% c("B", "B08") ~ "Fosfatos", ACTIVITY == "C29" ~ "Automotor")
  ) |>
  arrange(ACTIVITY, TIME_PERIOD)

write_csv(alpha_oecd, paste0(ruta_tablas, "alpha_oecd_serie.csv"))
alpha_oecd


#===============================================================================#
# A.1 - DEFINICIÓN DE LOS DOS ESCENARIOS DE CALIBRACIÓN
#===============================================================================#
# Se toman los alpha de cada fuente. Del lado OCDE se usa el ÚLTIMO AÑO
# OBSERVADO (2021), NO una extrapolación a 2023: proyectar la tendencia dos
# años más allá del último dato atravesaría el ciclo de precios 2022-2023
# (margen EBITDA de OCP: 43,7% -> 32,2%) que la regresión nunca vio. Ver
# PARTE 4 de TP2_estado_y_plan.md.

anio_oecd_ref <- 2021

alpha_fosfatos_A <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
alpha_auto_A     <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]

alpha_fosfatos_B <- alpha_oecd$alpha_capital[alpha_oecd$ACTIVITY == "B08" & alpha_oecd$TIME_PERIOD == anio_oecd_ref]
alpha_auto_B     <- alpha_oecd$alpha_capital[alpha_oecd$ACTIVITY == "C29" & alpha_oecd$TIME_PERIOD == anio_oecd_ref]

escenarios <- tibble(
  escenario      = c("A - HCP (TRE Base 2014)", "B - OCDE SUT_USEVA (2021 observado)"),
  alpha_fosfatos = c(alpha_fosfatos_A, alpha_fosfatos_B),
  alpha_auto     = c(alpha_auto_A, alpha_auto_B),
  nota = c("Fuente estadística nacional; foto fija, más cercana al año de referencia (2023)",
           "Fuente internacional armonizada; serie 2014-2021, último año observado")
)
escenarios


#===============================================================================#
# A.2 - FUNCIÓN QUE RECALCULA TODO EL MFE PARA UN PAR DE ALPHAS
#===============================================================================#
# Encapsula lo que antes estaba escrito una sola vez a mano: calibración
# Cobb-Douglas, FPP, caja de asignación, equilibrio, VPMgL observada y techo
# Leontief. Así ambos escenarios se calculan con EXACTAMENTE el mismo código
# y cualquier diferencia en los resultados viene solo del alpha.

calibrar_mfe <- function(alpha_f, alpha_a, etiqueta, anio_ref = anio_ref_fpp) {
  
  L_f0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref]
  L_a0 <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref]
  L_tot <- L_f0 + L_a0
  
  Q_f0 <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio_ref]
  Q_a0 <- auto_produccion$unidades[auto_produccion$year == anio_ref]
  
  ingresos_f <- ocp_financieros$ingresos_mmad[ocp_financieros$year == anio_ref]
  export_a   <- auto_exportaciones_oc$total_mmad[auto_exportaciones_oc$year == anio_ref]
  export_f   <- auto_exportaciones_oc$fosfatos_ref_mmad[auto_exportaciones_oc$year == anio_ref]
  
  A_f <- Q_f0 / (L_f0 ^ (1 - alpha_f))
  A_a <- Q_a0 / (L_a0 ^ (1 - alpha_a))
  
  precio_f <- ingresos_f / Q_f0
  precio_a <- export_a / Q_a0
  
  grilla <- tibble(L_fosfatos = seq(1000, L_tot - 1000, length.out = 2000)) |>
    mutate(
      L_auto         = L_tot - L_fosfatos,
      Q_fosfatos     = A_f * L_fosfatos ^ (1 - alpha_f),
      Q_auto         = A_a * L_auto     ^ (1 - alpha_a),
      pmgl_fosfatos  = (1 - alpha_f) * A_f * L_fosfatos ^ (-alpha_f),
      pmgl_auto      = (1 - alpha_a) * A_a * L_auto     ^ (-alpha_a),
      vpmgl_fosfatos = precio_f * pmgl_fosfatos,
      vpmgl_auto     = precio_a * pmgl_auto,
      escenario      = etiqueta
    )
  
  eq <- grilla |> mutate(brecha = abs(vpmgl_fosfatos - vpmgl_auto)) |> slice_min(brecha, n = 1)
  
  # VPMgL en el punto OBSERVADO (no en el equilibrio teórico)
  vpmgl_f_obs <- (1 - alpha_f) * (ingresos_f / L_f0)
  vpmgl_a_obs <- (1 - alpha_a) * (export_a / L_a0)
  # Robustez: empleo consolidado, y exportación aduanera en vez de ingresos OCP
  vpmgl_f_obs_consol <- (1 - alpha_f) * (ingresos_f / ocp_empleo_consolidado_alt)
  vpmgl_f_obs_oc     <- (1 - alpha_f) * (export_f / L_f0)
  
  # Techo técnico bajo Leontief (coeficientes fijos calibrados en el mismo punto)
  aL_leontief <- L_f0 / Q_f0
  Q_max <- max(fosfatos_produccion$produccion_mt, na.rm = TRUE)
  L_cap_leontief <- aL_leontief * Q_max
  
  list(
    etiqueta = etiqueta,
    alpha_f = alpha_f, alpha_a = alpha_a,
    L_f0 = L_f0, L_a0 = L_a0, L_tot = L_tot,
    A_f = A_f, A_a = A_a, precio_f = precio_f, precio_a = precio_a,
    grilla = grilla,
    w_eq = mean(c(eq$vpmgl_fosfatos, eq$vpmgl_auto)),
    L_f_eq = eq$L_fosfatos,
    vpmgl_f_obs = vpmgl_f_obs, vpmgl_a_obs = vpmgl_a_obs,
    brecha_obs = vpmgl_f_obs / vpmgl_a_obs,
    brecha_obs_consol = vpmgl_f_obs_consol / vpmgl_a_obs,
    brecha_obs_oc = vpmgl_f_obs_oc / vpmgl_a_obs,
    aL_leontief = aL_leontief, Q_max = Q_max, L_cap_leontief = L_cap_leontief,
    ratio_eq_sobre_observado = eq$L_fosfatos / L_f0,
    ratio_eq_sobre_capacidad = eq$L_fosfatos / L_cap_leontief
  )
}

res_A <- calibrar_mfe(alpha_fosfatos_A, alpha_auto_A, "A - HCP")
res_B <- calibrar_mfe(alpha_fosfatos_B, alpha_auto_B, "B - OCDE 2021")


#===============================================================================#
# A.3 - TABLA COMPARATIVA: TODOS LOS RESULTADOS, AMBOS ESCENARIOS
#===============================================================================#
# Esta es la tabla que permite contrastar de una vez sin más vueltas.

resumir_escenario <- function(r) {
  tibble(
    escenario                     = r$etiqueta,
    alpha_fosfatos                = r$alpha_f,
    alpha_automotor               = r$alpha_a,
    vpmgl_fosfatos_obs            = r$vpmgl_f_obs,
    vpmgl_automotor_obs           = r$vpmgl_a_obs,
    brecha_vpmgl_obs              = r$brecha_obs,
    brecha_robustez_empleo_consol = r$brecha_obs_consol,
    brecha_robustez_export_oc     = r$brecha_obs_oc,
    w_equilibrio                  = r$w_eq,
    L_fosfatos_equilibrio         = r$L_f_eq,
    L_fosfatos_observado          = r$L_f0,
    L_capacidad_leontief          = r$L_cap_leontief,
    eq_sobre_observado_veces      = r$ratio_eq_sobre_observado,
    eq_sobre_capacidad_veces      = r$ratio_eq_sobre_capacidad
  )
}

comparacion_escenarios <- bind_rows(resumir_escenario(res_A), resumir_escenario(res_B))
write_csv(comparacion_escenarios, paste0(ruta_tablas, "comparacion_escenarios_alpha.csv"))
comparacion_escenarios

# Lectura rápida de la comparación: las tres preguntas que deciden si el
# análisis del Bloque 2 sobrevive al cambio de parámetro.
conclusiones_robustez <- tibble(
  pregunta = c(
    "1. ¿La brecha VPMgL observada sigue siendo > 1 (fosfatos genera más valor por trabajador)?",
    "2. ¿El equilibrio teórico sigue estando MUY por encima del empleo observado?",
    "3. ¿El equilibrio teórico sigue estando por encima del techo técnico Leontief?"
  ),
  escenario_A = c(
    res_A$brecha_obs > 1,
    res_A$ratio_eq_sobre_observado > 2,
    res_A$L_f_eq > res_A$L_cap_leontief
  ),
  escenario_B = c(
    res_B$brecha_obs > 1,
    res_B$ratio_eq_sobre_observado > 2,
    res_B$L_f_eq > res_B$L_cap_leontief
  )
)
conclusiones_robustez
write_csv(conclusiones_robustez, paste0(ruta_tablas, "conclusiones_robustez_escenarios.csv"))
# Si las tres dan TRUE en AMBOS escenarios -> el hallazgo es robusto a la
# elección del parámetro, y eso FORTALECE la presentación (se puede decir
# "vale con cualquiera de las dos fuentes oficiales disponibles").
# Si alguna difiere -> decirlo explícitamente en la presentación; el
# resultado pasa a ser condicional al parámetro, lo cual también es un
# resultado legítimo y honesto.


#===============================================================================#
# A.4 - GRÁFICOS COMPARATIVOS (ambos escenarios superpuestos)
#===============================================================================#

fpp_ambos <- bind_rows(res_A$grilla, res_B$grilla)

fpp_observado <- inner_join(
  fosfatos_produccion |> rename(Q_fosfatos = produccion_mt),
  auto_produccion |> rename(Q_auto = unidades),
  by = "year"
)

g_fpp_escenarios <- ggplot() +
  geom_path(data = fpp_ambos, aes(x = Q_fosfatos, y = Q_auto, color = escenario), linewidth = 1.1) +
  geom_point(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto), size = 2.5, color = "gray25") +
  geom_text(data = fpp_observado, aes(x = Q_fosfatos, y = Q_auto, label = year),
            vjust = -1, size = 3, color = "gray35") +
  scale_color_manual(values = c("A - HCP" = "#1b4965", "B - OCDE 2021" = "#c1121f")) +
  labs(title = "FPP de Marruecos bajo dos calibraciones del parámetro alpha",
       subtitle = paste0("Misma producción y empleo observados (", anio_ref_fpp,
                         "); lo único que cambia es la fuente del alpha"),
       x = "Producción de fosfatos (miles de toneladas)", y = "Producción automotriz (unidades)",
       color = "Escenario",
       caption = "A: HCP (TRE Base 2014). B: OCDE SUT_USEVA, último año observado (2021, sin extrapolar).") +
  theme_tp1()
g_fpp_escenarios
ggsave(paste0(ruta_graficos, "grafico_fpp_escenarios.png"), g_fpp_escenarios,
       width = 10, height = 7, dpi = 300, bg = "white")

g_caja_escenarios <- fpp_ambos |>
  select(escenario, L_fosfatos, vpmgl_fosfatos, vpmgl_auto) |>
  pivot_longer(c(vpmgl_fosfatos, vpmgl_auto), names_to = "sector", values_to = "vpmgl") |>
  mutate(sector = recode(sector, vpmgl_fosfatos = "Fosfatos", vpmgl_auto = "Automotor")) |>
  ggplot(aes(x = L_fosfatos, y = vpmgl, color = sector, linetype = escenario)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = res_A$L_f0, linetype = "dotted", color = "gray20", linewidth = 0.8) +
  coord_cartesian(ylim = c(0, 2)) +
  scale_color_manual(values = paleta_sectores) +
  labs(title = "Caja de asignación del trabajo bajo ambos escenarios",
       subtitle = paste0("Línea punteada vertical: empleo observado en fosfatos (",
                         format(res_A$L_f0, big.mark = "."), ", ", anio_ref_fpp, ")"),
       x = "Trabajo asignado a fosfatos", y = "VPMgL (M MAD/trabajador/año)",
       color = NULL, linetype = "Escenario",
       caption = "Curvas recortadas en VPMgL=2 (las colas asintóticas son un artefacto del Cobb-Douglas).") +
  theme_tp1()
g_caja_escenarios
ggsave(paste0(ruta_graficos, "grafico_caja_escenarios.png"), g_caja_escenarios,
       width = 10, height = 6.5, dpi = 300, bg = "white")

# Leontief vs. Cobb-Douglas, con el equilibrio de AMBOS escenarios marcado
leontief_ambos <- tibble(L_fosfatos = seq(1000, res_A$L_tot - 1000, length.out = 2000)) |>
  mutate(
    `Cobb-Douglas (escenario A)` = res_A$A_f * L_fosfatos ^ (1 - res_A$alpha_f),
    `Cobb-Douglas (escenario B)` = res_B$A_f * L_fosfatos ^ (1 - res_B$alpha_f),
    `Leontief (coef. fijos)`     = pmin(L_fosfatos / res_A$aL_leontief, res_A$Q_max)
  ) |>
  pivot_longer(-L_fosfatos, names_to = "tecnologia", values_to = "Q_fosfatos")

g_leontief_escenarios <- ggplot(leontief_ambos, aes(x = L_fosfatos, y = Q_fosfatos, color = tecnologia)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = res_A$L_f0, linetype = "dotted", color = "gray20") +
  geom_vline(xintercept = res_A$L_f_eq, linetype = "dashed", color = "#1b4965") +
  geom_vline(xintercept = res_B$L_f_eq, linetype = "dashed", color = "#c1121f") +
  scale_color_manual(values = c("Cobb-Douglas (escenario A)" = "#1b4965",
                                "Cobb-Douglas (escenario B)" = "#c1121f",
                                "Leontief (coef. fijos)" = "gray40")) +
  labs(title = "¿Hay margen técnico para el equilibrio que predice Cobb-Douglas?",
       subtitle = "Rayadas: equilibrio de cada escenario. Punteada: empleo observado. El techo Leontief no depende de alpha.",
       x = "Trabajo asignado a fosfatos", y = "Producción de fosfatos (miles de toneladas)",
       color = NULL,
       caption = paste0("Techo Leontief calibrado en ", anio_ref_fpp,
                        " al pico histórico de producción: ", format(round(res_A$L_cap_leontief), big.mark = "."),
                        " trabajadores.")) +
  theme_tp1()
g_leontief_escenarios
ggsave(paste0(ruta_graficos, "grafico_leontief_escenarios.png"), g_leontief_escenarios,
       width = 10, height = 6.5, dpi = 300, bg = "white")


#===============================================================================#
# B.1 - CONVERGENCIA DE INTENSIDADES FACTORIALES (material del Bloque 3, HO)
#===============================================================================#
# El "problema" del alpha discordante es, leído desde el largo plazo, un
# hallazgo: las intensidades factoriales de ambos sectores convergen entre
# 2014 y 2021. El MFE toma las intensidades como DADAS (corto plazo); el HO
# las ve MOVERSE (largo plazo). Esto conecta los dos bloques del TP.

serie_convergencia <- alpha_oecd |> filter(ACTIVITY %in% c("B08", "C29"))

modelo_fosfatos <- lm(alpha_capital ~ TIME_PERIOD, data = filter(serie_convergencia, ACTIVITY == "B08"))
modelo_auto     <- lm(alpha_capital ~ TIME_PERIOD, data = filter(serie_convergencia, ACTIVITY == "C29"))
# Robustez: la tendencia de fosfatos sin 2020 (descarta que sea efecto COVID)
modelo_fosfatos_sin2020 <- lm(alpha_capital ~ TIME_PERIOD,
                              data = filter(serie_convergencia, ACTIVITY == "B08", TIME_PERIOD != 2020))

tabla_tendencias <- bind_rows(
  tidy(modelo_fosfatos)         |> mutate(modelo = "Fosfatos (B08), 2014-2021"),
  tidy(modelo_fosfatos_sin2020) |> mutate(modelo = "Fosfatos (B08), sin 2020"),
  tidy(modelo_auto)             |> mutate(modelo = "Automotor (C29), 2014-2021")
) |>
  filter(term == "TIME_PERIOD") |>
  select(modelo, pendiente_anual = estimate, std.error, statistic, p.value) |>
  left_join(
    bind_rows(
      glance(modelo_fosfatos)         |> mutate(modelo = "Fosfatos (B08), 2014-2021"),
      glance(modelo_fosfatos_sin2020) |> mutate(modelo = "Fosfatos (B08), sin 2020"),
      glance(modelo_auto)             |> mutate(modelo = "Automotor (C29), 2014-2021")
    ) |> select(modelo, r.squared, nobs),
    by = "modelo"
  )
tabla_tendencias
write_csv(tabla_tendencias, paste0(ruta_tablas, "tendencias_alpha_convergencia.csv"))
# Resultado esperado (ya verificado en consola): fosfatos -0,0251/año
# (p=0,007, idéntico sacando 2020 -> NO es efecto COVID); automotor
# +0,0114/año (p=0,008). Direcciones opuestas y ambas significativas: no es
# una deriva macro general, es específico de cada sector.

g_convergencia <- ggplot(serie_convergencia, aes(x = TIME_PERIOD, y = alpha_capital, color = sector)) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1, alpha = 0.15) +
  scale_color_manual(values = paleta_sectores) +
  scale_x_continuous(breaks = 2014:2021) +
  labs(title = "Convergencia de intensidades factoriales, 2014-2021",
       subtitle = "Participación del capital en el valor agregado (excedente de explotación / VA)",
       x = "Año", y = "Alpha (participación del capital)", color = NULL,
       caption = paste0(
         "Fuente: OECD SUT_USEVA (Marruecos). Fosfatos = B08 'Other mining and quarrying'; ",
         "automotor = C29. Pendientes: fosfatos ", round(coef(modelo_fosfatos)[2], 4),
         "/año (p=", signif(tidy(modelo_fosfatos)$p.value[2], 2), "), automotor +",
         round(coef(modelo_auto)[2], 4), "/año (p=", signif(tidy(modelo_auto)$p.value[2], 2), ")."
       )) +
  theme_tp1()
g_convergencia
ggsave(paste0(ruta_graficos, "grafico_convergencia_alpha.png"), g_convergencia,
       width = 10, height = 6.5, dpi = 300, bg = "white")

# Contraste explícito entre las dos fuentes de alpha, para que quede claro
# en la presentación que la discrepancia es de nivel en fosfatos, no en
# automotor (donde ambas fuentes coinciden casi exactamente).
contraste_fuentes <- tibble(
  sector = c("Fosfatos", "Automotor"),
  hcp_alpha = c(alpha_fosfatos_A, alpha_auto_A),
  oecd_alpha_2014 = c(
    alpha_oecd$alpha_capital[alpha_oecd$ACTIVITY == "B08" & alpha_oecd$TIME_PERIOD == 2014],
    alpha_oecd$alpha_capital[alpha_oecd$ACTIVITY == "C29" & alpha_oecd$TIME_PERIOD == 2014]
  ),
  oecd_alpha_2021 = c(alpha_fosfatos_B, alpha_auto_B)
) |>
  mutate(diferencia_hcp_vs_oecd2021 = hcp_alpha - oecd_alpha_2021)
contraste_fuentes
write_csv(contraste_fuentes, paste0(ruta_tablas, "contraste_fuentes_alpha.csv"))
# Nota para la presentación: HCP es una foto de base 2014. Comparado contra
# el alpha OCDE DE 2014 (no de 2021) la discrepancia en fosfatos se achica
# bastante - parte de la diferencia es que se están comparando años
# distintos, no solo metodologías distintas.


#===============================================================================#
# B.2 - QUÉ MIRAR CUANDO ESTO CORRA
#===============================================================================#
# 1. comparacion_escenarios  -> todos los resultados, ambos escenarios.
# 2. conclusiones_robustez   -> las tres preguntas que deciden si el Bloque 2
#                               sobrevive al cambio de parámetro. Objetivo:
#                               TRUE en las tres, en ambos escenarios.
# 3. tabla_tendencias        -> confirma que la convergencia es significativa
#                               y que no es efecto COVID.
# 4. contraste_fuentes       -> muestra que la discrepancia entre fuentes es
#                               de nivel y solo en fosfatos; en automotor
#                               ambas fuentes coinciden.
# 5. Los cuatro gráficos nuevos: fpp_escenarios, caja_escenarios,
#    leontief_escenarios, convergencia_alpha.
#
# DECISIÓN QUE QUEDA ABIERTA (depende de lo que muestre el punto 2):
#   - Si las tres preguntas dan TRUE en ambos escenarios: presentar el
#     escenario A como principal y el B como robustez, con una línea que
#     diga que el hallazgo vale con cualquiera de las dos fuentes.
#   - Si alguna difiere: presentar ambos en paralelo y decir explícitamente
#     qué conclusión depende del parámetro y cuál no.