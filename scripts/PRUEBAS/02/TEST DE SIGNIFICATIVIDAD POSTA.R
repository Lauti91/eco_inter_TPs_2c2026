#===============================================================================#
# CHEQUEO DE ROBUSTEZ: pendiente de labsh en Francia con ventana pareja
#===============================================================================#
# Motivo: la regresión original (audit_tendencia_labsh, Script 1) usa para
# cada país SU PROPIA ventana de dato REAL de labsh (anio_real_labsh, según
# detectar_primer_anio_real()): Marruecos desde 1999, España desde 1996,
# Alemania desde 1992, Francia desde 1950 (Francia no tiene el problema de
# imputación de PWT, así que conserva la serie completa). Eso deja a Francia
# en desventaja de comparabilidad: su pendiente significativa está dominada
# por la transición de posguerra 1950->1990 (caída del share laboral
# agrícola/industrial de esa época), que no tiene nada que ver con el
# argumento de integración comercial reciente que el TP2 quiere testear.
#
# Este script prueba si la pendiente de Francia se sostiene restringida a
# una ventana comparable con los otros europeos (desde 1992, como Alemania,
# o desde 1996, como España) en vez de la serie completa.
#
# DEPENDENCIA: correr TP2_01 antes (necesita kl_benchmark con la columna
# labsh_real ya calculada, y de paso deja armado audit_tendencia_labsh, que
# se reutiliza acá para la tabla comparativa final).
#===============================================================================#

if (!exists("kl_benchmark")) {
  stop("Correr primero TP2_01 - este chequeo depende de kl_benchmark (con labsh_real).")
}

library(tidyverse); library(broom)

ruta_tablas_05 <- if (exists("ruta_tablas")) ruta_tablas else "output/tablas/"
if (!dir.exists(ruta_tablas_05)) dir.create(ruta_tablas_05, recursive = TRUE)

## --- 1. Pendiente de labsh en Francia, para distintas ventanas -------------
pendiente_francia_ventana <- function(anio_desde, etiqueta) {
  datos <- kl_benchmark |>
    filter(country == "France", year >= anio_desde, !is.na(labsh_real))
  bind_cols(
    tibble(ventana = etiqueta, ventana_desde = anio_desde, n_anios = nrow(datos)),
    tidy(lm(labsh_real ~ year, data = datos)) |> filter(term == "year") |>
      select(pendiente_anual = estimate, p.value),
    glance(lm(labsh_real ~ year, data = datos)) |> select(r.squared)
  )
}

robustez_francia <- bind_rows(
  pendiente_francia_ventana(1950, "completa (original, 1950-2019)"),
  pendiente_francia_ventana(1992, "pareja con Alemania (desde 1992)"),
  pendiente_francia_ventana(1996, "pareja con España (desde 1996)")
)
robustez_francia  # AUDITORÍA: comparar pendiente_anual y p.value entre ventanas
write_csv(robustez_francia, paste0(ruta_tablas_05, "robustez_labsh_francia.csv"))

## --- 2. Tabla comparativa con los otros 3 países (ventana original) --------
# Requiere que TP2_01 haya dejado armado audit_tendencia_labsh en el entorno.
if (exists("audit_tendencia_labsh")) {
  comparacion_completa <- bind_rows(
    audit_tendencia_labsh |>
      filter(country != "France") |>
      transmute(pais_o_ventana = country, pendiente_anual, p.value, r.squared,
                significativo = p.value < 0.05),
    robustez_francia |>
      transmute(pais_o_ventana = paste("Francia -", ventana),
                pendiente_anual, p.value, r.squared, significativo = p.value < 0.05)
  )
  comparacion_completa  # AUDITORÍA: tabla final para decidir 3/4 vs. 2/3
  write_csv(comparacion_completa, paste0(ruta_tablas_05, "comparacion_labsh_ventana_pareja.csv"))
}

## --- 3. Resumen en consola --------------------------------------------------
cat("\n=== Pendiente de labsh, Francia, por ventana de comparación ===\n")
print(robustez_francia)
cat("\nSi la pendiente deja de ser significativa (p > 0,05) en 'pareja con Alemania'",
    "o 'pareja con España', el argumento de Stolper-Samuelson del Bloque 3d pasa de",
    "'3 de 4 países con caída significativa' a '2 de 3 socios (sin Francia) con",
    "caída significativa' - una afirmación más débil pero más honesta.\n",
    "Si en cambio la pendiente se sostiene significativa incluso en la ventana",
    "corta, el hallazgo original queda mejor respaldado (no depende de la",
    "asimetría de ventanas).\n")



















#===============================================================================#
# CHEQUEO: ¿la tendencia de labsh en Marruecos cambia con la llegada de la
# industria automotriz (Renault Tánger, 2012)?
#===============================================================================#
# Motivo: la pendiente de labsh de Marruecos que usamos en el TP2 (Bloque 3d)
# es una única regresión lineal sobre toda la ventana de dato real disponible
# (1999-2019, ~-0,00017/año, no significativa). Eso asume implícitamente que
# la relación labsh~año es igual de plana en todo el período. Pero automotor
# -que ya mostramos en el Bloque 3c que es relativamente trabajo-intensivo
# dentro de lo que Marruecos exporta- recién arranca en 2012 (Renault
# Tánger). Si el efecto sobre la participación del trabajo se concentra
# después de esa fecha, una pendiente única sobre todo el período podría
# estar promediando dos regímenes distintos y diluyendo la señal.
#
# ADVERTENCIA DE POTENCIA: partiendo 1999-2019 en dos tramos quedan ~13 y ~8
# observaciones anuales por tramo. Con tan pocos años, no hay que esperar
# significatividad estadística fuerte en ninguno de los dos - esto es un
# chequeo direccional/exploratorio, no un test con mucha potencia. Por eso
# se prioriza el test de cambio estructural (interacción año x post-2012)
# sobre comparar dos regresiones sueltas: da un p-valor formal para "¿la
# pendiente cambió?" en lugar de solo mirar dos números a ojo.
#
# DEPENDENCIA: correr TP2_01 antes (necesita kl_benchmark con labsh_real).
#===============================================================================#

if (!exists("kl_benchmark")) {
  stop("Correr primero TP2_01 - este chequeo depende de kl_benchmark (con labsh_real).")
}

library(tidyverse); library(broom)

ruta_tablas_06 <- if (exists("ruta_tablas")) ruta_tablas else "output/tablas/"
if (!dir.exists(ruta_tablas_06)) dir.create(ruta_tablas_06, recursive = TRUE)

anio_corte_auto <- 2012  # arranque de producción de Renault en Tánger

mar_labsh <- kl_benchmark |>
  filter(country == "Morocco", !is.na(labsh_real)) |>
  mutate(post_auto = as.integer(year >= anio_corte_auto))

mar_labsh |> count(post_auto)  # AUDITORÍA: tamaño de cada tramo (ojo con n chico)

## --- 1. Pendiente única, todo el período (la que ya está en el TP2) --------
m_completo <- lm(labsh_real ~ year, data = mar_labsh)
tidy(m_completo) |> filter(term == "year")  # AUDITORÍA: referencia (~-0,00017, p=0,60)

## --- 2. Pendientes por tramo, pre y post automotor -------------------------
m_pre  <- lm(labsh_real ~ year, data = filter(mar_labsh, year <  anio_corte_auto))
m_post <- lm(labsh_real ~ year, data = filter(mar_labsh, year >= anio_corte_auto))

pendientes_tramo <- bind_rows(
  tidy(m_completo) |> filter(term == "year") |>
    mutate(tramo = paste0("completo (1999-2019, n=", nobs(m_completo), ")")),
  tidy(m_pre) |> filter(term == "year") |>
    mutate(tramo = paste0("pre-automotor (1999-", anio_corte_auto - 1, ", n=", nobs(m_pre), ")")),
  tidy(m_post) |> filter(term == "year") |>
    mutate(tramo = paste0("post-automotor (", anio_corte_auto, "-2019, n=", nobs(m_post), ")"))
) |>
  transmute(tramo, pendiente_anual = estimate, p.value)
pendientes_tramo  # AUDITORÍA
write_csv(pendientes_tramo, paste0(ruta_tablas_06, "labsh_marruecos_por_tramo.csv"))

## --- 3. Test formal de cambio estructural (interacción año x post-2012) ----
# El término year:post_auto es el test que importa: si es significativo,
# la pendiente post-2012 es distinta de la pre-2012 (más que una curiosidad
# de dos regresiones sueltas, es una diferencia estadísticamente detectable).
m_quiebre <- lm(labsh_real ~ year * post_auto, data = mar_labsh)
audit_quiebre <- tidy(m_quiebre, conf.int = TRUE)
audit_quiebre  # AUDITORÍA: mirar la fila year:post_auto

write_csv(audit_quiebre, paste0(ruta_tablas_06, "labsh_marruecos_quiebre_estructural.csv"))

## --- 4. Resumen en consola --------------------------------------------------
cat("\n=== Pendiente de labsh en Marruecos, por tramo (corte:", anio_corte_auto, ") ===\n")
print(pendientes_tramo)
cat("\n=== Test de cambio estructural (interacción year:post_auto) ===\n")
print(audit_quiebre |> filter(term == "year:post_auto"))
cat("\nCon n=", nobs(m_pre), "y n=", nobs(m_post), "por tramo, no hay que esperar mucha",
    "potencia estadística - esto es más útil como chequeo direccional (¿el signo/magnitud",
    "de la pendiente post-2012 va en la dirección de más participación laboral que antes?)",
    "que como test concluyente.\n")