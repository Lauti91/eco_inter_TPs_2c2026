#===============================================================================#
# TP2 - Economía Internacional
# Marruecos: Modelo de Factores Específicos (MFE) y Heckscher-Ohlin (HO)
#===============================================================================#
# Arranca corriendo el script final del TP1 completo, para heredar sus
# objetos (bases limpias, VCR/VCRN por producto y año, IIC por socio, tema
# visual, paletas) en vez de recalcularlos.
#
# Reestructurado para que los bloques giren alrededor de los dos modelos que
# pide la consigna (MFE y HO), con sub-bloques adentro de cada uno, en vez de
# alrededor de las fuentes de datos. Sigue sin estar corrido ni validado en R
# (se escribió sin acceso a R para probarlo) - revisar nombres de columnas de
# pwt10.01 al cargarlo la primera vez.
#
# Incorpora la devolución de Gemini Spark sobre este mismo borrador: la FPP
# real en el plano (Q_fosfatos, Q_automotor) y la caja de asignación con
# VPMgL (2.3/2.4), y la justificación empírica de las intensidades
# factoriales calculada en código en vez de asumida (3.1). Ver BLOQUE 5 para
# lo que sigue abierto incluso después de esto (en particular, la variante
# con OECD TiVA/ICIO que propuso Gemini para calcular alpha directamente
# desde Valor Agregado/Remuneración de asalariados, en vez de vía
# io_coeficientes del HCP).

objetos_tp1 <- "output/tablas/01/objetos_heredados_tp1.RData"
if (file.exists(objetos_tp1)) {
  load(objetos_tp1)
} else {
  # Fallback si todavía no se corrió el TP1 con el guardado de objetos
  # livianos del BLOQUE 8 de 01_INDICADORES_FINAL.R (más lento: reprocesa
  # las bases .dta de WITS y regenera los 14 gráficos del TP1).
  source("scripts/FINAL/01/01_INDICADORES_FINAL.R", print.eval = FALSE)
}
# print.eval = FALSE evita reintentar mostrar cada gráfico del TP1 en pantalla
# durante el source() (causa típica de "Viewport has zero dimension(s)" si el
# panel de Plots está colapsado). No afecta los ggsave() del TP1.

if (!requireNamespace("WDI", quietly = TRUE)) install.packages("WDI", repos = "https://cloud.r-project.org")
if (!requireNamespace("pwt10", quietly = TRUE)) install.packages("pwt10", repos = "https://cloud.r-project.org")
library(WDI)
library(pwt10)

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
# Comparar contra auto_exportaciones_oc (sub-bloque 2.1, Office des Changes)
# una vez corrido - son dos fuentes distintas (mirror stats WITS vs. balanza
# oficial) y pueden no coincidir exactamente. Para el número "de tapa" del
# cruce en la diapositiva, usar Office des Changes (más autorizado); para
# los gráficos internos, WITS es más consistente con el resto del TP1/TP2.
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
iic_automotor  # confirmar que Francia muestra IIC alto, como esperaría la
# narrativa de Renault/Stellantis


#===============================================================================#
# BLOQUE 2: MODELO DE FACTORES ESPECÍFICOS (corto plazo)
#===============================================================================#
# Corresponde al Bloque 2 de la consigna (incisos a-d): factor específico vs.
# factor móvil, FPP cóncava y PMgL decreciente, shock de TOT, y lógica de
# ganadores/perdedores de corto plazo.

## --- 2.1 Datos duros por sector (rondas 1 y 2 de Spark) ---
# Insumo tanto para justificar qué factor es específico a cada sector como
# para calibrar la FPP/VPMgL empírica del sub-bloque 2.2. Ver
# TP2_seguimiento_investigacion_ronda2.md para el detalle de fuentes.
# Los valores muy redondos en empleo automotor (148.000, 160.000, 220.000...)
# son declaraciones ministeriales/parlamentarias, no series estadísticas de
# precisión censal - tratar como "según declaraciones oficiales", no como
# estadística dura sin matices.

# OCP (fosfatos): financieros y empleo
ocp_financieros <- tibble(
  year            = 2021:2024,
  ingresos_mmad   = c(84300, 114574, 91277, 97000),
  ebitda_mmad     = c(36269, 50076, 29396, 38800),
  capex_mmad      = c(13135, 20011, 26825, NA_real_),
  dividendos_mmad = c(5126, 8163, 7251, NA_real_)
)

ocp_empleo_serie <- tibble(
  year   = 2019:2024,
  empleo = c(20000, 18357, 17961, 17688, 17000, 20000)
  # 2019 y 2024: plantilla consolidada (~20.000). 2020-2023: plantilla
  # directa en minas y química, más acotada (17.000-18.357) - la caída y
  # posterior repunte 2024 coincide con el Green Investment Programme.
)

# USGS: reservas y producción física de roca fosfórica
fosfatos_reservas <- tibble(
  year = c(2024, 2025), reservas_mt = c(50000, 50000), pct_mundial = c(0.676, 0.685)
)
fosfatos_produccion <- tibble(
  year = 2019:2025,
  produccion_mt = c(35200, 37400, 38100, 38000, 33000, 35300, 36000)
)

# Automotor: producción física y empleo
auto_produccion <- tibble(
  year     = c(2019, 2021, 2022, 2023, 2024, 2025),  # 2020 no reportado por OICA
  unidades = c(403218, 403007, 464864, 535825, 559645, 501965)
)

auto_empleo_serie <- tibble(
  year   = 2019:2025,
  empleo = c(148000, 160000, 180000, 220000, 230000, 238000, 250000),
  nota   = c(rep("declarado/observado", 6), "meta proyectada, no observada")
)

# Exportaciones automotrices, Office des Changes (fuente oficial - más
# autorizada que el WITS de 1.2 para el número "de tapa" del cruce).
auto_exportaciones_oc <- tibble(
  year                  = 2019:2025,
  total_mmad            = c(77128, 72283, 83783, 111281, 141763, 157594, 154494),
  cableado_mmad         = c(33500, 25695, 25206, 34810, 46136, 53643, 57777),
  ensamblaje_mmad       = c(34000, 29216, 39491, 55149, 67629, 70955, 61299),
  fosfatos_ref_mmad     = c(48923, 50869, 79893, 115484, 76141, 87083, 99804)
)
# 2023: declaración ministerial cita 148.000 (vs. 141.763 de balanza
# provisional) - Spark dejó ambas versiones anotadas; decidir cuál citar.

# Coeficientes de reparto del valor agregado por rama (HCP, TRE Base 2014).
# Sirven acá como alpha del Cobb-Douglas (2.2) y se reutilizan en el
# sub-bloque 3.2 como aproximación de los requerimientos técnicos aL,j/aK,j
# de HO. No hay matriz insumo-producto desagregada disponible (Ley 371-71 de
# secreto estadístico: al haber 1-2 empresas dominantes por rama - OCP en
# minería, Renault/Stellantis en automotor - el HCP solo publica el TRE a
# 20 ramas agregadas).
io_coeficientes <- tibble(
  rama              = c("Minería/fosfatos (B00)", "Automotor/IMME (C00)"),
  capital_va_min    = c(0.75, 0.50),
  capital_va_max    = c(0.85, 0.60),
  trabajo_va_min    = c(0.12, 0.38),
  trabajo_va_max    = c(0.20, 0.48),
  ci_produccion_min = c(0.35, 0.68),
  ci_produccion_max = c(0.45, 0.78)
) |>
  mutate(capital_va_medio = (capital_va_min + capital_va_max) / 2,
         trabajo_va_medio = (trabajo_va_min + trabajo_va_max) / 2)

## --- 2.2 Evolución temporal de la productividad media del trabajo ---
# Con io_coeficientes (participación del capital en el VA de cada rama) más
# las series de producción física y empleo, se puede aproximar la VPMgL de
# cada sector sin necesitar la matriz insumo-producto completa: el share de
# capital de las cuentas nacionales ES el alpha de un Cobb-Douglas simple
# Y = A * F^alpha * L^(1-alpha).
# Esto da la evolución en el tiempo de cada sector por separado (útil como
# contexto histórico), pero NO es una FPP - una FPP se grafica en el plano
# (Q_fosfatos, Q_automotor), no año a año. Para eso ver el sub-bloque 2.3.

calcular_vpmgl_cobb_douglas <- function(produccion, empleo, alpha) {
  # produccion: tibble con year, <col_produccion>
  # empleo: tibble con year, empleo
  # alpha: participación del factor específico (capital o tierra) en el VA
  base <- inner_join(produccion, empleo, by = "year")
  col_prod <- names(produccion)[2]
  base |>
    mutate(
      producto_medio_trabajo = .data[[col_prod]] / empleo,
      # VPMgL bajo Cobb-Douglas: PMgL = (1-alpha) * Y / L (proporcional al
      # producto medio, no un cálculo con precios - falta el precio del
      # producto para pasar de PMgL a VPMgL; usar precios_internacionales
      # (2.3) o ocp_financieros/auto_exportaciones_oc como proxy de precio
      # implícito si se quiere ir más allá del producto marginal físico).
      pmgl_relativo = (1 - alpha) * producto_medio_trabajo
    )
}

vpmgl_fosfatos <- calcular_vpmgl_cobb_douglas(
  fosfatos_produccion, ocp_empleo_serie,
  alpha = io_coeficientes$capital_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
)

vpmgl_automotor <- calcular_vpmgl_cobb_douglas(
  auto_produccion |> rename(unidades_producidas = unidades), auto_empleo_serie,
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
       subtitle = "Producción física / empleo, escala log — no son unidades comparables entre sectores,\nsolo la FORMA de cada curva es lo que importa para la concavidad de la FPP",
       x = "Año", y = "Unidades físicas / empleado (log)", color = NULL,
       caption = "Calibrado con coeficientes de reparto del VA (HCP) como alpha de un Cobb-Douglas simple.") +
  theme_tp1()
g_productividad
ggsave(paste0(ruta_graficos, "grafico_productividad_mfe.png"), g_productividad,
       width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 2.3 La FPP real de Marruecos (plano Q_fosfatos x Q_automotor) ---
# Calibración Cobb-Douglas simple por sector (Q = A * L^(1-alpha), con
# alpha = participación del factor específico según io_coeficientes),
# ajustando A para que cada sector reproduzca exactamente su producción
# observada en el año de referencia con el empleo observado ese año. Con
# eso se arma una grilla de asignación del trabajo móvil (L_fosfatos +
# L_automotor = L_total fijo) que traza la curva cóncava de la FPP.
#
# IMPORTANTE: los puntos observados de otros años (2019, 2021, 2022...) NO
# van a caer exactamente sobre esta curva, porque la curva fija L_total en
# el nivel de anio_ref_fpp mientras que el empleo total efectivamente varió
# año a año. Eso no es un error: mostrar esos puntos "por fuera" de la curva
# calibrada en un año puntual es precisamente lo que ilustra que la FPP se
# desplazó con el tiempo (acumulación de capital vía IED en Tánger/Kenitra),
# no solo que la economía se movió a lo largo de una FPP fija.

anio_ref_fpp <- 2023  # último año con las cuatro series (producción y
# empleo de ambos sectores) simultáneamente completas

L_fosfatos_0 <- ocp_empleo_serie$empleo[ocp_empleo_serie$year == anio_ref_fpp]
L_auto_0     <- auto_empleo_serie$empleo[auto_empleo_serie$year == anio_ref_fpp]
L_total      <- L_fosfatos_0 + L_auto_0

Q_fosfatos_0 <- fosfatos_produccion$produccion_mt[fosfatos_produccion$year == anio_ref_fpp]
Q_auto_0     <- auto_produccion$unidades[auto_produccion$year == anio_ref_fpp]

alpha_fosfatos <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Minería/fosfatos (B00)"]
alpha_auto     <- io_coeficientes$capital_va_medio[io_coeficientes$rama == "Automotor/IMME (C00)"]

A_fosfatos <- Q_fosfatos_0 / (L_fosfatos_0 ^ (1 - alpha_fosfatos))
A_auto     <- Q_auto_0     / (L_auto_0     ^ (1 - alpha_auto))

fpp_grilla <- tibble(
  L_fosfatos = seq(1000, L_total - 1000, length.out = 300)
) |>
  mutate(
    L_auto     = L_total - L_fosfatos,
    Q_fosfatos = A_fosfatos * L_fosfatos ^ (1 - alpha_fosfatos),
    Q_auto     = A_auto     * L_auto     ^ (1 - alpha_auto)
  )

# Puntos observados en los años con datos completos de ambas producciones,
# para mostrar el desplazamiento de la economía a través del tiempo
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
       caption = "Curva calibrada con alpha de io_coeficientes (HCP) sobre fosfatos_produccion$produccion_mt / auto_produccion$unidades; el punto 2023 coincide con la curva por ser el año de calibración.") +
  theme_tp1()
g_fpp
ggsave(paste0(ruta_graficos, "grafico_fpp_real.png"), g_fpp, width = 10, height = 7, dpi = 300, bg = "white")

# La curva de arriba está calibrada en un solo año (anio_ref_fpp) y por eso
# los puntos de otros años quedan "adentro" - muestra el desplazamiento de
# forma indirecta. Para mostrarlo explícitamente se recalibra una curva
# distinta para cada año con datos completos: cada curva usa el L_total y
# la producción observada DE ESE AÑO para fijar A_fosfatos/A_auto, así que
# el punto de cada año cae exactamente sobre su propia curva - lo que se ve
# es la FPP entera corriéndose hacia afuera año a año, no un punto que se
# aleja de una curva fija.
anios_fpp_familia <- intersect(
  intersect(ocp_empleo_serie$year, auto_empleo_serie$year[auto_empleo_serie$nota == "declarado/observado"]),
  intersect(fosfatos_produccion$year, auto_produccion$year)
)  # excluye 2025 (empleo automotor proyectado, no observado) y 2020 (sin
# producción automotriz reportada por OICA)

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
       caption = "Cada curva recalibra A_fosfatos/A_auto con el empleo y la producción observados ESE año, manteniendo fijo el alpha de io_coeficientes. El corrimiento hacia afuera es la contracara de la acumulación de capital vía IED (3.4).") +
  theme_tp1()
g_fpp_desplazamiento
ggsave(paste0(ruta_graficos, "grafico_fpp_desplazamiento.png"), g_fpp_desplazamiento,
       width = 10, height = 7, dpi = 300, bg = "white")

## --- 2.4 Caja de asignación del trabajo y salario de equilibrio (VPMgL) ---
# Convierte el PMgL físico de 2.3 a valor monetario multiplicando por el
# precio implícito de cada sector (ingreso/exportación total del año de
# referencia ÷ producción física de ese año), para que ambas curvas queden
# en la misma unidad y puedan graficarse juntas en la caja clásica de
# asignación del trabajo del MFE.
# VERIFICAR ANTES DE CITAR: confirmar que produccion_mt (fosfatos) y las
# unidades de ingresos_mmad/total_mmad están en bases compatibles entre sí
# antes de confiar en el nivel absoluto de precio_fosfatos/precio_auto o en
# w_equilibrio - lo que sí es robusto sin esa verificación es la FORMA de
# las dos curvas VPMgL (monótonas decrecientes) y que se cruzan una vez.

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

g_caja_asignacion <- ggplot(caja_asignacion, aes(x = L_fosfatos)) +
  geom_line(aes(y = vpmgl_fosfatos, color = "Fosfatos"), linewidth = 1.1) +
  geom_line(aes(y = vpmgl_auto, color = "Automotor"), linewidth = 1.1) +
  geom_vline(xintercept = L_fosfatos_eq, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = w_equilibrio, linetype = "dashed", color = "gray40") +
  coord_cartesian(ylim = c(0, 2)) +  # recorta las colas asintóticas (PMgL -> Inf
  # cuando el trabajo de un sector -> 0) para
  # que el cruce se vea con detalle; ver nota
  # de la caption sobre lo que queda afuera
  scale_color_manual(values = paleta_sectores) +
  labs(title = "Caja de asignación del trabajo — MFE",
       subtitle = paste0("Salario de equilibrio w* \u2248 ", round(w_equilibrio, 3),
                         " en L_fosfatos \u2248 ", round(L_fosfatos_eq)),
       x = "Trabajo asignado a fosfatos (L_fosfatos)", y = "VPMgL (precio implícito × PMgL)",
       color = NULL,
       caption = "Precio implícito = ingreso/exportación total ÷ producción física del año de referencia. Curvas recortadas en VPMgL=2 (la divergencia hacia los extremos es un artefacto esperado del Cobb-Douglas, no un error). Verificar unidades (ver comentario arriba) antes de citar w* en la presentación.") +
  theme_tp1()
g_caja_asignacion
ggsave(paste0(ruta_graficos, "grafico_caja_asignacion.png"), g_caja_asignacion,
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

precios_internacionales <- tibble(
  year                  = c(2022, 2023, 2024),
  dap_usd_mt            = c(772.2, 550.0, 563.7),
  roca_fosforica_usd_mt = c(266.2, 323.8, 321.7),
  gas_natural_usd_mmbtu = c(40.34, 13.11, 12.00)
)

g_precios <- precios_internacionales |>
  pivot_longer(cols = c(dap_usd_mt, roca_fosforica_usd_mt), names_to = "producto", values_to = "precio") |>
  mutate(producto = recode(producto, dap_usd_mt = "DAP", roca_fosforica_usd_mt = "Roca fosfórica")) |>
  ggplot(aes(x = year, y = precio, color = producto)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.2) +
  labs(title = "Precio internacional de fertilizantes",
       subtitle = "USD por tonelada métrica, f.o.b.",
       x = "Año", y = "USD/mt", color = NULL,
       caption = "Fuente: World Bank Commodity Markets (Pink Sheet).") +
  theme_tp1()
g_precios
ggsave(paste0(ruta_graficos, "grafico_precios_internacionales.png"), g_precios,
       width = 10, height = 6, dpi = 300, bg = "white")

## --- 2.6 Efectos distributivos observados (salarios) ---
# Insumo para contrastar la predicción teórica de ganadores/perdedores del
# MFE (inciso d) contra lo efectivamente ocurrido.
salario_promedio_general <- tibble(
  year = c(2019, 2023, 2024), salario_mad_mes = c(5255, 5500, 5871)
)
# Corte 2020 por rama NMA-1 (no hay desagregación específica de automotor
# ni de minería/fosfatos - "Industrie" es el mejor proxy disponible para
# automotor; fosfatos opera bajo régimen estatutario propio de OCP, fuera
# de este esquema).
salarios_cnss_sector_2020 <- tibble(
  sector = c("Industrie (proxy automotor)", "Agriculture/pesca", "Transporte",
             "Comercio", "Servicios de mercado", "Construcción", "Financiero/seguros"),
  salario_mad_mes = c(5002, 2975, 6520, 5712, 5163, 4046, 14937)
)


#===============================================================================#
# BLOQUE 3: MODELO DE HECKSCHER-OHLIN (largo plazo)
#===============================================================================#
# Corresponde al Bloque 3 de la consigna (incisos a-d): dotación factorial
# relativa, patrón exportador predicho, y Stolper-Samuelson como contraparte
# distributiva de largo plazo.

## --- 3.1 Justificación empírica de las intensidades factoriales ---
# En vez de asumir directamente que fosfatos es intensivo en el factor
# específico (tierra/capital) y automotor relativamente absorbente de
# trabajo, se lo muestra con la brecha de valor generado por trabajador
# entre ambos sectores, calculada acá de las series ya cargadas en 2.1 (no
# copiada a mano). Es la variante "Opción A" de la devolución de Gemini
# (facturación/trabajador); la "Opción B" -Valor Agregado y Remuneración de
# Asalariados desde OECD TiVA/ICIO para Marruecos, sectores B y C29- daría
# un alpha más riguroso que io_coeficientes pero requiere descargar esa base
# aparte y no está implementada todavía (ver BLOQUE 5).

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

brecha_valor_trabajador  # confirmar el orden de magnitud (esperado ~7x en
# 2023, según el hallazgo de la ronda 2 de Spark):
# respalda tomar fosfatos como intensivo en el
# factor específico y automotor como el sector
# relativamente absorbente de trabajo abundante.

## --- 3.2 Dotación factorial relativa (Penn World Tables) ---
data("pwt10.01", package = "pwt10")
paises_benchmark <- c("Morocco", "Germany", "Spain", "France")

# Verificar nombres exactos de columnas con glimpse() antes de confiar en el
# pipe: rnna = capital stock, emp = empleo, hc = capital humano, labsh =
# participación del trabajo en el ingreso (nombres estándar de pwt10.01).
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

## --- 3.3 Requerimientos técnicos por rama (reutiliza io_coeficientes de 2.1) ---
# Los mismos coeficientes de reparto del VA que sirvieron de alpha en el
# Cobb-Douglas de 2.2/2.3 son, leídos al revés, una aproximación de los
# requerimientos técnicos aL,j/aK,j que pide el estilo del Ejercicio 11 de
# la guía teórica (ver io_coeficientes$trabajo_va_medio / capital_va_medio).
# No reemplazan una matriz insumo-producto real por rama individual (no
# existe en fuentes abiertas por secreto estadístico, Ley 371-71), pero son
# el máximo nivel de detalle técnico disponible para HO, junto con la
# brecha de valor por trabajador de 3.1.

## --- 3.4 Origen del capital extranjero (movilidad de largo plazo) ---
ied_stock_total <- tibble(
  year        = 2019:2024,
  stock_musd  = c(66500, 67500, 72994, 63278, 69297, 71500)
)

# Corte 2020 (único año con desglose completo por país de origen; Francia
# tiene además el dato de 2022 actualizado en euros).
ied_origen_pais_2020 <- tibble(
  pais       = c("Francia", "Emiratos Árabes Unidos", "España", "Países Bajos", "Estados Unidos"),
  stock_mmad = c(194400, 132400, 53100, 27500, 22500),  # NL y EEUU: punto medio del rango informado
  pct_stock  = c(0.303, 0.207, 0.083, 0.045, 0.035),
  nota       = c("Renault, Valeo, Saint-Gobain",
                 "Telecom, inmobiliario, energía",
                 "Autopartes, agroindustria, logística",
                 "Sede fiscal de Stellantis N.V.",
                 "Cableado automotor: Lear, Aptiv")
)

g_ied_origen <- ggplot(ied_origen_pais_2020, aes(x = reorder(pais, stock_mmad), y = stock_mmad)) +
  geom_col(fill = paleta_sectores["Automotor"]) +
  coord_flip() +
  labs(title = "Stock de IED en Marruecos por país de origen",
       subtitle = "2020, millones de MAD",
       x = NULL, y = "Millones de MAD",
       caption = "Fuente: Office des Changes (PEG 2020) / DG Trésor / UNCTADstat.") +
  theme_tp1()
g_ied_origen
ggsave(paste0(ruta_graficos, "grafico_ied_origen.png"), g_ied_origen,
       width = 9, height = 6, dpi = 300, bg = "white")

# Inversión / IED puntual en el sector automotor (no es serie anual limpia,
# son hitos de inversión por proyecto/empresa)
auto_inversion <- tibble(
  entidad  = c("Renault Group (Tánger + SOMACA)", "Stellantis (Kenitra)",
               "Gigafactorías de baterías (Gotion/BTR)"),
  monto_mmad = c(14000, 9000, 20000),
  año_corte  = c(2022, 2022, 2024),
  nota = c("Acumulada desde 2012, capacidad 500.000 veh/año",
           "6.000 (2015) + ampliación 3.000 (nov-2022), capacidad 400.000 veh/año",
           "Convenios comprometidos, no necesariamente desembolsados")
)
ied_manufactura <- tibble(
  year = c(2020, 2024), stock_mmad = c(152000, 170000)
)
ied_manufactura_flujo_anual_mmad <- c(8000, 10000)  # rango, no serie por año


#===============================================================================#
# BLOQUE 4: SÍNTESIS Y POLÍTICA
#===============================================================================#
# Corresponde al Bloque 4 de la consigna: comparación de ambos modelos y una
# política pública concreta como respuesta a las tensiones distributivas.

## --- 4.1 Valor agregado sectorial (contexto para la comparación MFE vs. HO) ---
hcp_va_crecimiento <- tibble(
  year = c(2023, 2024, 2025), mineria_pct = c(-4.2, 11.5, 7.5), manufactura_pct = c(3.2, 2.1, 1.9)
)
hcp_va_total_mmad <- tibble(year = c(2023, 2024, 2025), total = c(1340158, 1440407, 1517747))

## --- 4.2 Caisse de Compensation: política pública concreta (inciso 4b) ---
caisse_compensacion <- tibble(
  year = c(2022, 2023, 2024), carga_mmad = c(42060, 30000, 16357),
  nota = c("pico histórico, 21.812 solo gas butano", NA, "presupuestado, Loi de Finances")
)
# Régimen AMDIE: exención IS 5 años + 15% posterior, exención arancelaria/
# IVA, primas hasta 30% (Charte de l'Investissement, Zonas de Aceleración
# Industrial). Texto para la diapositiva, no serie - candidato natural para
# el inciso 4b (quién financia/quién recibe) junto con caisse_compensacion.


#===============================================================================#
# BLOQUE 5: LO QUE QUEDA GENUINAMENTE SIN RESOLVER (no insistir con Spark)
#===============================================================================#
# 1. Matriz insumo-producto desagregada a nivel de rama individual
#    (minería vs. automotor puros, no IMME agregado) - no existe en fuentes
#    abiertas por secreto estadístico (Ley 371-71). Los coeficientes
#    agregados de io_coeficientes son el máximo nivel de detalle disponible
#    vía HCP y alcanzan para la calibración de 2.2/2.3/2.4 y la
#    aproximación de 3.3.
# 2. Salario CNSS específico de la sub-rama automotriz (vs. "Industrie"
#    agregada) - mismo motivo estructural, no está desagregado en fuentes
#    abiertas. salarios_cnss_sector_2020 con "Industrie" es el mejor proxy
#    disponible.
# 3. Variante "Opción B" de la justificación empírica de intensidades
#    factoriales (3.1): calcular alpha directamente desde Valor Agregado y
#    Remuneración de Asalariados de OECD TiVA/ICIO para Marruecos (sectores
#    B y C29), en vez de vía io_coeficientes del HCP. Más riguroso que la
#    brecha de valor/trabajador ya calculada, pero requiere descargar esa
#    base aparte - no implementado.
# 4. Verificación de unidades de precio_fosfatos/precio_auto en 2.4 (ver
#    comentario ahí): antes de citar el valor de w* en la presentación,
#    confirmar que produccion_mt e ingresos_mmad/total_mmad están en bases
#    compatibles.
# 5. anio_ref_fpp está fijo en 2023 (2.3/2.4) - si al correr el script
#    conviene otro año de referencia (por disponibilidad de datos o para
#    la narrativa), es la única variable que hay que cambiar para
#    recalcular la FPP y la caja de asignación completas.