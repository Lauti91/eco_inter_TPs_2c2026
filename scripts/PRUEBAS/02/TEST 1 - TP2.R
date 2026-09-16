#===============================================================================#
# TP2 - Economía Internacional
# Marruecos: MFE (Fosfatos vs. Automotor) y Heckscher-Ohlin
#===============================================================================#
# Este script arranca corriendo el script final del TP1 completo, para
# heredar sus objetos (bases limpias, VCR/VCRN por producto y año, IIC por
# socio, tema visual, paletas) en vez de recalcularlos. Es más lento que
# redefinir todo de cero, pero garantiza que ambos TP usan exactamente los
# mismos números - y evita pedirle a Spark datos que el TP1 ya tiene.
#
# No está corrido ni validado (se escribió sin acceso a R para probarlo) -
# revisar nombres de columnas de pwt10.01 al cargarlo la primera vez.

source("scripts/FINAL/01/01_INDICADORES_FINAL.R", print.eval = FALSE)

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


#===============================================================================#
# BLOQUE 1: LO QUE YA ESTÁ RESUELTO EN LAS BASES DEL TP1
#===============================================================================#
# vcr_mar, mar_exp e iic_heatmap_completo tienen TODOS los productos y años
# (no solo el top 12 de las tablas finales) - fosfatos y automotor están ahí
# adentro, solo hay que filtrarlos.

sectores_mfe <- c("272", "562", "781", "784")
nombres_sectores_mfe <- c(
  "272" = "Fertilizantes crudos", "562" = "Fertilizantes manufacturados",
  "781" = "Autos de pasajeros",   "784" = "Autopartes"
)

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

## --- 1.2 Serie de exportaciones (valor) de fosfatos y automotor, WITS 2021-2025 ---
# Resuelve el TODO 4.3 que le habíamos dejado a la ronda 2 de Spark (serie
# histórica de exportaciones automotrices) - no hacía falta pedirlo, ya está
# en la base que bajaron para el TP1.
#
# OJO - son cifras espejo de WITS (mirror statistics, SITC Rev. 3), no
# necesariamente idénticas a los boletines del Office des Changes (141.760 M
# MAD para automotor 2023 que trajo la investigación de Spark). Para los
# gráficos internos de este TP2 conviene usar WITS, por consistencia
# metodológica con el resto del análisis (TP1 incluido); para el número "de
# tapa" del cruce automotor/fosfatos en la diapositiva, el dato del Office
# des Changes es más autorizado por ser la fuente oficial del país. Definir
# cuál se cita en cada lugar antes de armar la diapositiva final.

exportaciones_mfe <- mar_exp |>
  filter(p == "WLD", cuci %in% sectores_mfe) |>
  mutate(
    sector_desc = nombres_sectores_mfe[cuci],
    grupo = if_else(cuci %in% c("272", "562"), "Fosfatos", "Automotor")
  ) |>
  select(year, cuci, sector_desc, grupo, value, share)

exportaciones_mfe_agrupado <- exportaciones_mfe |>
  group_by(year, grupo) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")

g_cruce <- ggplot(exportaciones_mfe_agrupado, aes(x = year, y = total * 1000, color = grupo)) +
  geom_line(linewidth = 1.3) + geom_point(size = 2.5) +
  scale_color_manual(values = paleta_sectores) +
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = " mil M USD")) +
  labs(title = "El cruce: automotor supera a fosfatos en valor exportado",
       subtitle = "Marruecos, 2021-2025 (WITS, mirror statistics SITC Rev. 3)",
       x = "Año", y = NULL, color = NULL,
       caption = paste(
         "Para el dato oficial 2023 (Office des Changes: automotor 141.760 M MAD",
         "vs. fosfatos 76.140 M MAD) ver Bloque 4."
       )) +
  theme_tp1()
g_cruce
ggsave(paste0(ruta_graficos, "grafico_cruce_automotor_fosfatos.png"), g_cruce,
       width = 10, height = 6.5, dpi = 300, bg = "white")

## --- 1.3 IIC de automotor por socio (ya calculado en el heatmap del TP1) ---
# 781/784 son parte de sectores_volumen (top_volumen del TP1), así que su IIC
# contra ESP/FRA/DEU/USA/BRA ya está en iic_heatmap_completo - clave para
# conectar el origen francés del capital (Renault/Stellantis) con el patrón
# de comercio observado, sin pedirle nada nuevo a UNCTAD para este ángulo
# puntual (el stock de IED por país de origen sigue siendo un dato aparte,
# porque mide propiedad del capital, no destino del comercio).

iic_automotor <- iic_heatmap_completo |>
  filter(cuci %in% c("781", "784")) |>
  left_join(tibble(socio = names(nombres_socio), socio_nombre = nombres_socio), by = "socio")

iic_automotor  # revisar antes de graficar: confirmar si Francia muestra IIC
# alto en autopartes/autos de pasajeros, como esperaría la
# narrativa de Renault/Stellantis


#===============================================================================#
# BLOQUE 2: TÉRMINOS DE INTERCAMBIO (2c)
#===============================================================================#

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

# Complemento: precio internacional de DAP y roca fosfórica (World Bank Pink
# Sheet, dato traído por Spark) - más específico que el índice agregado,
# conecta el shock con el bien puntual que exporta Marruecos.
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


#===============================================================================#
# BLOQUE 3: DOTACIÓN FACTORIAL (3b) — Penn World Tables
#===============================================================================#

data("pwt10.01", package = "pwt10")

paises_benchmark <- c("Morocco", "Germany", "Spain", "France")

# OJO: verificar nombres exactos de columnas al cargar con glimpse() -
# rnna = capital stock, emp = empleo, hc = índice de capital humano,
# labsh = participación del trabajo en el ingreso, nombres estándar de
# pwt10.0/10.01, pero conviene confirmar antes de confiar en el pipe.
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
       x = "Año", y = "K/L (capital stock / empleo, PWT rnna/emp)", color = NULL,
       caption = "Fuente: Penn World Table 10.01.") +
  theme_tp1()
g_kl
ggsave(paste0(ruta_graficos, "grafico_kl_benchmark.png"), g_kl,
       width = 10, height = 6.5, dpi = 300, bg = "white")

# TODO: definir si el promedio UE es simple o ponderado (PBI o población)
# antes de agregarlo a este gráfico. Sin definir todavía.

# labsh de Marruecos en el tiempo - insumo directo para testear
# Stolper-Samuelson de largo plazo (3d): si sube a medida que crece el
# sector intensivo en trabajo (automotor), es evidencia a favor de la
# predicción teórica.
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


#===============================================================================#
# BLOQUE 4: DATOS DUROS DE LA RONDA 1 DE SPARK (OCP, USGS, OICA, HCP, fiscal)
#===============================================================================#
# Ver TP2_fuentes_de_datos.md y TP2_seguimiento_investigacion_ronda2.md para
# fuente/URL/fecha de consulta de cada valor.

ocp_financieros <- tibble(
  year            = 2021:2024,
  ingresos_mmad   = c(84300, 114574, 91277, 97000),
  ebitda_mmad     = c(36269, 50076, 29396, 38800),
  capex_mmad      = c(13135, 20011, 26825, NA_real_),
  dividendos_mmad = c(5126, 8163, 7251, NA_real_)
)
ocp_empleo_2023 <- 20000  # headcount directo Marruecos, Document de Référence

fosfatos_reservas <- tibble(
  year = c(2024, 2025), reservas_mt = c(50000, 50000), pct_mundial = c(0.676, 0.685)
)
fosfatos_produccion <- tibble(
  year = 2019:2025,
  produccion_mt = c(35200, 37400, 38100, 38000, 33000, 35300, 36000)
)

auto_produccion <- tibble(
  year     = c(2019, 2021, 2022, 2023, 2024, 2025),
  unidades = c(403218, 403007, 464864, 535825, 559645, 501965)
)

auto_2023 <- list(empleo = 230000, plantas = 260, exportaciones_mmad = 141760)
fosfatos_exportaciones_2023_mmad <- 76140

hcp_va_crecimiento <- tibble(
  year = c(2023, 2024, 2025), mineria_pct = c(-4.2, 11.5, 7.5), manufactura_pct = c(3.2, 2.1, 1.9)
)
hcp_va_total_mmad <- tibble(year = c(2023, 2024, 2025), total = c(1340158, 1440407, 1517747))

caisse_compensacion <- tibble(
  year = c(2022, 2023, 2024), carga_mmad = c(42060, 30000, 16357),
  nota = c("pico histórico, 21.812 solo gas butano", NA, "presupuestado, Loi de Finances")
)
# Régimen AMDIE (Charte de l'Investissement + Zonas de Aceleración Industrial):
# exención de IS 5 años + 15% posterior, exención arancelaria/IVA, primas
# hasta 30%. Queda como texto para la diapositiva, no como serie.


#===============================================================================#
# BLOQUE 5: PENDIENTE DE LA RONDA 2 DE SPARK — placeholders, no correr todavía
#===============================================================================#
# 4.3 (serie de exportaciones automotrices) SALIÓ de la lista: ya resuelto en
# el Bloque 1.2 con la base del TP1.

# TODO 4.1 — Series de empleo año a año (fosfatos y automotor), para
# calibrar la VPMgL empírica del Bloque 2a/2b. Hoy solo hay un punto por
# sector (2023: ocp_empleo_2023 y auto_2023$empleo).
empleo_fosfatos_serie  <- tibble(year = integer(), empleo = integer())  # PENDIENTE
empleo_automotor_serie <- tibble(year = integer(), empleo = integer())  # PENDIENTE

# TODO 4.2 — Matriz insumo-producto del HCP (Tableau Entrées-Sorties):
# coeficientes a_L,j y a_K,j reales para minería y automotor/material de
# transporte. Habilita reconstruir el patrón de comercio de HO (Bloque 3c)
# con datos propios en vez de compararlo solo cualitativamente con el TP1.
# io_matriz_hcp <- ...  # PENDIENTE

# TODO 4.4 — Serie de inversión/capital acumulado en el sector automotor
# (AMDIE), año a año, comparable a capex_mmad de OCP.
auto_inversion_serie <- tibble(year = integer(), inversion_mmad = numeric())  # PENDIENTE

# TODO 4.5 — CNSS: salario medio declarado por sector (minería, automotor,
# agricultura). Insumo para el Bloque 2d (efectos distributivos reales).
salarios_cnss <- tibble(year = integer(), sector = character(), salario_mad = numeric())  # PENDIENTE

# TODO 4.6 — UNCTADstat: stock de IED por sector y por país de origen
# (interesa Francia y Países Bajos, por Renault y Stellantis). Distinto de
# iic_automotor del Bloque 1.3: esto mide propiedad del capital, no destino
# del comercio.
ied_unctad <- tibble(year = integer(), sector = character(), pais_origen = character(),
                     stock_usd = numeric())  # PENDIENTE


#===============================================================================#
# BLOQUE 6: FUNCIONES A COMPLETAR CUANDO LLEGUEN LOS DATOS PENDIENTES
#===============================================================================#

# Aproxima productividad media del trabajo (proxy de VPMgL) año a año, a
# partir de producción física y empleo pareados. Requiere TODO 4.1.
calcular_productividad_media <- function(produccion, empleo) {
  inner_join(produccion, empleo, by = "year") |>
    mutate(productividad = .[[2]] / .[[ncol(.)]])
  # placeholder - ajustar nombres de columna reales una vez que estén las series
}

# Cuando estén los coeficientes técnicos reales (TODO 4.2), arma la FPP de
# Marruecos bajo HO al estilo del Ejercicio 11 de la guía teórica, pero con
# a_L,j y a_K,j propios en vez de supuestos.
construir_fpp_ho <- function(aL_fosfatos, aK_fosfatos, aL_auto, aK_auto, L_total, K_total) {
  # placeholder - implementar una vez que llegue la matriz insumo-producto
}