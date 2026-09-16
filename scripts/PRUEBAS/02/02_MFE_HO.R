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
# pwt10.01 al cargarlo la primera vez. Pendiente además: dar justificación
# empírica (con datos tratados en código) de por qué cada sector se toma como
# intensivo en el factor que se le asigna, y armar la FPP real de Marruecos
# con los dos sectores seleccionados - ninguna de las dos cosas está resuelta
# todavía en esta versión.

source("scripts/FINAL/01/01_INDICADORES_FINAL.R", print.eval = FALSE)
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

## --- 2.2 FPP cóncava y PMgL decreciente: calibración empírica (Cobb-Douglas) ---
# Con io_coeficientes (participación del capital en el VA de cada rama) más
# las series de producción física y empleo, se puede aproximar la VPMgL de
# cada sector sin necesitar la matriz insumo-producto completa: el share de
# capital de las cuentas nacionales ES el alpha de un Cobb-Douglas simple
# Y = A * F^alpha * L^(1-alpha).
# PENDIENTE (ver encabezado del script): esto calibra el PMgL relativo de
# cada sector por separado, pero todavía no arma la FPP real de Marruecos
# (el par de curvas de producción posible entre los dos sectores) - falta
# ese paso para cerrar el inciso b del Bloque 2 de la consigna.

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

## --- 2.3 Shock de términos de intercambio ---
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

## --- 2.4 Efectos distributivos observados (salarios) ---
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

## --- 3.1 Dotación factorial relativa (Penn World Tables) ---
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
# PENDIENTE (ver encabezado del script): esto compara K/L pero todavía no
# da justificación empírica explícita de por qué fosfatos se toma como
# intensivo en tierra/capital y automotor en capital - falta ese argumento
# para cerrar del todo el inciso b del Bloque 3.

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

## --- 3.2 Requerimientos técnicos por rama (reutiliza io_coeficientes de 2.1) ---
# Los mismos coeficientes de reparto del VA que sirvieron de alpha en el
# Cobb-Douglas de 2.2 son, leídos al revés, una aproximación de los
# requerimientos técnicos aL,j/aK,j que pide el estilo del Ejercicio 11 de
# la guía teórica (ver io_coeficientes$trabajo_va_medio / capital_va_medio).
# No reemplazan una matriz insumo-producto real por rama individual (no
# existe en fuentes abiertas por secreto estadístico, Ley 371-71), pero son
# el máximo nivel de detalle técnico disponible para HO.

## --- 3.3 Origen del capital extranjero (movilidad de largo plazo) ---
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
#    y alcanzan para la calibración del Bloque 2.2 y la aproximación de 3.2.
# 2. Salario CNSS específico de la sub-rama automotriz (vs. "Industrie"
#    agregada) - mismo motivo estructural, no está desagregado en fuentes
#    abiertas. salarios_cnss_sector_2020 con "Industrie" es el mejor proxy
#    disponible.
# 3. Justificación empírica explícita de la intensidad factorial de cada
#    sector (por qué fosfatos = tierra/capital-intensivo, automotor =
#    capital-intensivo con mano de obra abundante) - kl_benchmark (3.1) e
#    io_coeficientes (2.1/3.2) dan insumos, pero falta un tratamiento en
#    código que lo deje explícito en vez de asumido.
# 4. FPP real de Marruecos con los dos sectores seleccionados - vpmgl_fosfatos
#    y vpmgl_automotor (2.2) calibran el PMgL de cada sector por separado,
#    pero no arman todavía el par de curvas de producción posible entre
#    ambos.
