#===============================================================================#
# TP1 - Economía Internacional
# Marruecos, 2021-2025
#===============================================================================#

if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer", repos = "https://cloud.r-project.org")
if (!requireNamespace("scales", quietly = TRUE)) install.packages("scales", repos = "https://cloud.r-project.org")

library(tidyverse)
library(haven)
library(ggrepel)
library(RColorBrewer)
library(scales)

setwd("/cloud/project")
getwd()


ruta_datos <- "bases de datos/"
ruta_graficos <- "output/graficos/01/"
ruta_tablas <- "output/tablas/01/"

#===============================================================================#
# BLOQUE 0: ESTILO GLOBAL
#===============================================================================#

paleta_socios <- c(
  "ESP" = "#AA151B", "FRA" = "#0055A4", "DEU" = "#FFCE00",
  "USA" = "#3C3B6E", "BRA" = "#009739"
)
nombres_socio <- c(ESP = "España", FRA = "Francia", DEU = "Alemania",
                   USA = "EE.UU.", BRA = "Brasil")

theme_tp1 <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, color = "gray10"),
      plot.subtitle = element_text(color = "gray35", size = base_size - 1.5, margin = margin(b = 10)),
      plot.caption = element_text(color = "gray55", size = base_size - 5, hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(color = "gray25", size = base_size - 1),
      axis.text = element_text(color = "gray40"),
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = base_size - 1)
    )
}

#===============================================================================#
# BLOQUE 0.5: FORMATO DE VALORES MONETARIOS
#===============================================================================#


formatear_valor <- function(valor_miles_usd, digits_grandes = 2, digits_chicos = 1) {
  valor_usd <- valor_miles_usd * 1000
  dplyr::if_else(
    valor_usd >= 1e9,
    paste0(scales::number(valor_usd / 1e9, accuracy = 10^-digits_grandes), " mil millones USD"),
    paste0(scales::number(valor_usd / 1e6, accuracy = 10^-digits_chicos), " millones USD")
  )
}


#===============================================================================#
# BLOQUE 1: CARGA Y LIMPIEZA (3 bases definitivas)
#===============================================================================#

limpiar_wits <- function(path, incluir_desc = TRUE) {
  base <- read_dta(path)
  names(base) <- tolower(names(base))
  base <- base %>%
    mutate(
      cuci = as.character(as_factor(productcode)),
      r    = as.character(as_factor(reporteriso3)),
      p    = as.character(as_factor(partneriso3)),
      flow = as.character(as_factor(tradeflowname))
    ) %>%
    rename(value = tradevaluein1000usd)
  if (incluir_desc) {
    base <- base %>%
      mutate(cuci_desc = as.character(as_factor(productdescription))) %>%
      select(r, p, flow, cuci, cuci_desc, value, year)
  } else {
    base <- base %>% select(r, p, flow, cuci, value, year)
  }
  base
}

# Reporter = MAR, Partners = 77 países + WLD
comtrade_mar <- limpiar_wits(paste0(ruta_datos, "MAR_ALLPARTNERS_WLD.dta"))
distinct(comtrade_mar, r); nrow(comtrade_mar)  # no debería dar un redondo tipo 100000

# Reporters = ESP/FRA/DEU/BRA/IND/CHN/ARE/GBR/USA, Partner = WLD
comtrade_socios <- limpiar_wits(paste0(ruta_datos, "SOCIOS_WLD.dta"))
distinct(comtrade_socios, r); nrow(comtrade_socios)

# Reporter = World, Partner = World (ojo: "World" = "All" en esta base)
comtrade_wld <- limpiar_wits(paste0(ruta_datos, "WLD_WLD.dta"), incluir_desc = FALSE)
distinct(comtrade_wld, r, p)

# Reporter = MAR, Partner = WLD, nomenclatura a 2 dígitos (división)
comtrade_2dig <- limpiar_wits(paste0(ruta_datos, "MAR_2DIG_WLD.dta"))

# Bases de trabajo derivadas
mar_exp <- comtrade_mar |>
  filter(flow == "Export") |>
  group_by(year, p) |>
  mutate(total_expo = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = value / total_expo)

mar_imp <- comtrade_mar |>
  filter(flow == "Import") |>
  group_by(year, p) |>
  mutate(total_impo = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = value / total_impo)

wld_exp <- comtrade_wld |>
  filter(flow == "Export", r == "All", p == "All") |>
  group_by(year) |>
  mutate(total_wld = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share_wld = value / total_wld) |>
  select(year, cuci, share_wld)


#===============================================================================#
# BLOQUE 2: PERFIL DE COMERCIO EXTERIOR (punto 2 de la consigna)
#===============================================================================#

n_bienes <- 5
n_paises <- 4

# Ranking de socios comerciales de Marruecos por EXPORTACIONES 
# (se reutiliza más abajo, en 2.4, para el ranking país-a-país)
top_socios <- mar_exp |>
  filter(p != "WLD", year == 2024) |>
  group_by(p) |>
  summarise(total = first(total_expo)) |>
  arrange(desc(total)) |>
  slice_max(total, n = 15) |>
  mutate(total_fmt = formatear_valor(total))

top_socios

# Destino de los fertilizantes (272 y 562) 
destino_fertilizantes <- comtrade_mar |>
  filter(flow == "Export", year == 2024, cuci %in% c("272", "562"), p != "WLD") |>
  group_by(p, cuci_desc) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total)) |>
  slice_max(total, n = 15) |>
  mutate(total_fmt = formatear_valor(total))

destino_fertilizantes

# Lookup de descripciones a nivel 2 dígitos 
lookup_2dig <- comtrade_2dig |>
  filter(r == "MAR", p == "WLD") |>
  distinct(cuci, cuci_desc) |>
  rename(cuci_2d = cuci, desc_2d = cuci_desc)

# Principales divisiones exportadoras e importadoras a 2 dígitos
sectores_2dig_exp <- mar_exp |>
  filter(p == "WLD", year == 2024) |>
  mutate(cuci_2d = substr(cuci, 1, 2)) |>
  group_by(cuci_2d) |>
  summarise(total = sum(value, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  slice_max(total, n = 5) |>
  left_join(lookup_2dig, by = "cuci_2d") |>
  relocate(desc_2d, .after = cuci_2d) |>
  mutate(total_fmt = formatear_valor(total))

sectores_2dig_exp

sectores_2dig_imp <- mar_imp |>
  filter(p == "WLD", year == 2024) |>
  mutate(cuci_2d = substr(cuci, 1, 2)) |>
  group_by(cuci_2d) |>
  summarise(total = sum(value, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  slice_max(total, n = 5) |>
  left_join(lookup_2dig, by = "cuci_2d") |>
  relocate(desc_2d, .after = cuci_2d) |>
  mutate(total_fmt = formatear_valor(total))

sectores_2dig_imp

# Ranking de cada producto (3 dígitos) dentro de su propia división (2 dígitos)
# 1 = el más grande de esa división
detalle_composicion <- mar_exp |>
  filter(p == "WLD", year == 2024, substr(cuci, 1, 2) %in% sectores_2dig_exp$cuci_2d) |>
  mutate(cuci_2d = substr(cuci, 1, 2)) |>
  left_join(lookup_2dig, by = "cuci_2d") |>
  group_by(cuci_2d) |>
  mutate(
    rank_en_division = rank(-value, ties.method = "first"),
    pct_division = value / sum(value) * 100
  ) |>
  ungroup() |>
  mutate(
    categoria_rank = case_when(
      rank_en_division == 1 ~ "Producto principal",
      rank_en_division == 2 ~ "2° producto",
      rank_en_division == 3 ~ "3° producto",
      TRUE ~ "Resto"
    )
  )

orden_apilado <- c("Resto", "3° producto", "2° producto", "Producto principal")

detalle_composicion <- detalle_composicion |>
  mutate(categoria_rank = factor(categoria_rank, levels = orden_apilado))

# Acumulado por división 
detalle_apilado <- detalle_composicion |>
  arrange(desc_2d, categoria_rank) |>
  group_by(desc_2d) |>
  mutate(
    ymax = cumsum(value),
    ymin = ymax - value,
    y_centro = (ymin + ymax) / 2
  ) |>
  ungroup()

# Etiqueta del producto principal
etiquetas_top <- detalle_apilado |>
  filter(rank_en_division == 1) |>
  mutate(label_completo = paste0(round(pct_division), "%"))

totales_division <- detalle_apilado |>
  group_by(desc_2d) |>
  summarise(total = max(ymax), .groups = "drop")

paleta_composicion <- c(
  "Producto principal" = "#1b4965",
  "2° producto"         = "#5fa8d3",
  "3° producto"         = "#bee9e8",
  "Resto"               = "#e8e8e8"
)

g_composicion <- ggplot(detalle_apilado, aes(x = reorder(desc_2d, ymax, max))) +
  geom_rect(aes(xmin = as.numeric(reorder(desc_2d, ymax, max)) - 0.38,
                xmax = as.numeric(reorder(desc_2d, ymax, max)) + 0.38,
                ymin = ymin, ymax = ymax, fill = categoria_rank),
            color = "white", linewidth = 0.6) +
  geom_text(
    data = etiquetas_top, aes(y = y_centro, label = label_completo),
    color = "white", fontface = "bold", size = 4
  ) +
  geom_text(
    data = totales_division, aes(x = desc_2d, y = total, label = scales::label_number(scale = 1/1000, suffix = "M", accuracy = 0.1)(total)),
    hjust = -0.15, size = 3.8, fontface = "bold", color = "gray25", inherit.aes = FALSE
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = paleta_composicion) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14)), labels = NULL) +
  labs(
    title = "¿Qué tan concentrada está cada división exportadora?",
    subtitle = "Marruecos, 2024 · el % indica el peso del producto principal dentro de su división",
    x = NULL, y = NULL, fill = "Posición dentro\nde la división",
    caption = "Total exportado por división indicado al final de cada barra (millones de US$)."
  ) +
  theme_tp1(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold", color = "gray20"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    plot.title.position = "plot",
    plot.margin = margin(t = 10, r = 45, b = 10, l = 10)
  )

g_composicion
ggsave(paste0(ruta_graficos, "grafico_composicion_2dig.png"), g_composicion,
       width = 10, height = 6.5, dpi = 300, bg = "white")


## --- 2.1: Principales bienes exportados e importados (3 dígitos) ---

top_bienes_exp <- mar_exp |>
  filter(p == "WLD", year == 2024) |>
  select(cuci, cuci_desc, value) |>
  slice_max(value, n = n_bienes) |>
  mutate(flow = "Exportado", value_fmt = formatear_valor(value))

top_bienes_imp <- mar_imp |>
  filter(p == "WLD", year == 2024) |>
  select(cuci, cuci_desc, value) |>
  slice_max(value, n = n_bienes) |>
  mutate(flow = "Importado", value_fmt = formatear_valor(value))

top_bienes_exp
top_bienes_imp

## --- 2.2: Montos totales de expos/impos (WLD) + evolución + saldo ---

comercio_total <- bind_rows(
  mar_exp |> filter(p == "WLD") |> distinct(year, total_expo) |> rename(total = total_expo) |> mutate(flujo = "Exportaciones"),
  mar_imp |> filter(p == "WLD") |> distinct(year, total_impo) |> rename(total = total_impo) |> mutate(flujo = "Importaciones")
) |>
  mutate(total_fmt = formatear_valor(total))

comercio_total

saldo_comercial <- comercio_total |>
  select(year, flujo, total) |>
  pivot_wider(names_from = flujo, values_from = total) |>
  mutate(saldo = Exportaciones - Importaciones, saldo_fmt = formatear_valor(saldo))

saldo_comercial

g_comercio_total <- ggplot(comercio_total, aes(x = year, y = total * 1000, color = flujo)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.3) +
  scale_color_manual(values = c("Exportaciones" = "#1b4965", "Importaciones" = "#c1121f")) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = " mil M USD")) +
  labs(title = "Evolución del comercio exterior de Marruecos",
       subtitle = "Exportaciones e importaciones totales, 2021-2025",
       x = "Año", y = NULL, color = NULL) +
  theme_tp1()
g_comercio_total
ggsave(paste0(ruta_graficos, "grafico_comercio_total.png"), g_comercio_total,
       width = 10, height = 6, dpi = 300, bg = "white")

g_saldo <- ggplot(saldo_comercial, aes(x = year, y = saldo * 1000)) +
  geom_col(fill = "#780000") +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.4) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-9, suffix = " mil M USD")) +
  labs(title = "Saldo comercial de Marruecos",
       subtitle = "Exportaciones − Importaciones, 2021-2025",
       x = "Año", y = NULL) +
  theme_tp1()
g_saldo
ggsave(paste0(ruta_graficos, "grafico_saldo_comercial.png"), g_saldo,
       width = 10, height = 6, dpi = 300, bg = "white")

## --- 2.3: Principales países a los que Marruecos les compra/vende cada bien ---

desglose_pais <- function(cuci_code, flow_code, total_mundial) {
  base <- comtrade_mar |>
    filter(flow == flow_code, year == 2024, cuci == cuci_code, p != "WLD")
  top_p <- base |> slice_max(value, n = n_paises) |> select(p, value)
  resto <- total_mundial - sum(top_p$value, na.rm = TRUE)
  bind_rows(top_p, tibble(p = "Otros", value = pmax(resto, 0))) |>
    mutate(pct = round(value / total_mundial * 100, 1), value_fmt = formatear_valor(value))
}

tabla_paises_exp <- pmap_dfr(
  top_bienes_exp |> select(cuci, cuci_desc, value),
  function(cuci, cuci_desc, value) {
    desglose_pais(cuci, "Export", value) |> mutate(cuci_desc = cuci_desc, .before = 1)
  }
)
tabla_paises_exp

tabla_paises_imp <- pmap_dfr(
  top_bienes_imp |> select(cuci, cuci_desc, value),
  function(cuci, cuci_desc, value) {
    desglose_pais(cuci, "Import", value) |> mutate(cuci_desc = cuci_desc, .before = 1)
  }
)
tabla_paises_imp

## --- 2.4: Ranking de países por importaciones totales ---

ranking_paises_imp <- mar_imp |>
  filter(p != "WLD", year == 2024) |>
  group_by(p) |>
  summarise(total = first(total_impo)) |>
  arrange(desc(total)) |>
  slice_max(total, n = 15) |>
  mutate(total_fmt = formatear_valor(total))
ranking_paises_imp


#===============================================================================#
# BLOQUE 3: VCR - Ventajas Comparativas Reveladas (Balassa, 1965)
#===============================================================================#

vcr_aux_mar <- mar_exp |>
  filter(p == "WLD") |>
  select(year, cuci, cuci_desc, share_mar = share)

vcr_mar <- vcr_aux_mar |>
  left_join(wld_exp, by = c("year", "cuci")) |>
  mutate(vcr = share_mar / share_wld, vcrn = (vcr - 1) / (vcr + 1)) |>
  arrange(desc(vcr))

sum(is.na(vcr_mar$share_wld))  # chequeo de NA

top_ventaja <- vcr_mar |> filter(year == 2024) |> slice_max(vcrn, n = 12)
top_desventaja <- vcr_mar |> filter(year == 2024) |> slice_min(vcrn, n = 12)
sectores_top <- top_ventaja$cuci

# Top volumen 
top_volumen <- mar_exp |>
  filter(p == "WLD", year == 2024) |>
  select(cuci, cuci_desc, value) |>
  arrange(desc(value)) |>
  slice_max(value, n = 12) |>
  mutate(value_fmt = formatear_valor(value))

top_volumen

interseccion_vol_vcr <- intersect(top_volumen$cuci, top_ventaja$cuci)
length(interseccion_vol_vcr); interseccion_vol_vcr

# --- Gráfico final: evolución del VCRN, sectores clave + mayor caída ---
variacion_vcrn <- vcr_mar |>
  filter(cuci %in% sectores_top, year %in% c(2021, 2025)) |>
  select(cuci, cuci_desc, year, vcrn) |>
  pivot_wider(names_from = year, values_from = vcrn, names_prefix = "y") |>
  mutate(caida = y2025 - y2021) |>
  arrange(caida)

top_vcrn_2024 <- vcr_mar |> filter(year == 2024) |> slice_max(vcrn, n = 3) |> pull(cuci)
mayor_caida <- variacion_vcrn |> slice_min(caida, n = 2) |> pull(cuci)
sectores_reducidos <- union(top_vcrn_2024, mayor_caida)

g_vcrn <- vcr_mar |>
  filter(cuci %in% sectores_reducidos) |>
  ggplot(aes(x = year, y = vcrn, color = cuci_desc)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.2) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Evolución del VCRN — sectores clave y de mayor caída",
       subtitle = "Marruecos, 2021-2025", x = "Año", y = "VCRN", color = "Producto") +
  theme_tp1()
g_vcrn
ggsave(paste0(ruta_graficos, "grafico_evolucion_vcrn.png"), g_vcrn,
       width = 10, height = 6.5, dpi = 300, bg = "white")


#===============================================================================#
# BLOQUE 4: IIC - Índice de Intensidad Comercial (Yeats, 1997)
#===============================================================================#

calcular_iic_multi <- function(socio, sectores, anio) {
  mar_soc <- mar_exp |>
    filter(p == socio, year == anio, cuci %in% sectores) |>
    select(cuci, cuci_desc, xijk = value, xij = total_expo)
  mar_wld <- mar_exp |>
    filter(p == "WLD", year == anio, cuci %in% sectores) |>
    select(cuci, xik = value, xi = total_expo)
  mar_soc |>
    left_join(mar_wld, by = "cuci") |>
    mutate(iic = as.numeric((xijk / xik) / (xij / xi)), year = anio) |>
    select(year, cuci, cuci_desc, iic)
}

# IIC de los bienes de mayor volumen, para cada socio final -> heatmap
socios_finales <- c("ESP", "FRA", "DEU", "USA", "BRA")
sectores_volumen <- top_volumen$cuci

iic_heatmap <- map_dfr(socios_finales, function(s) {
  calcular_iic_multi(s, sectores_volumen, 2024) |> mutate(socio = s)
})

iic_heatmap_completo <- iic_heatmap |>
  tidyr::complete(socio = socios_finales, cuci, fill = list(iic = 0)) |>
  select(-cuci_desc) |>
  left_join(iic_heatmap |> distinct(cuci, cuci_desc), by = "cuci")

iic_heatmap_cat <- iic_heatmap_completo |>
  mutate(
    socio_label = factor(socio, levels = names(paleta_socios), labels = nombres_socio[names(paleta_socios)]),
    categoria = case_when(
      iic == 0 ~ "Sin comercio",
      iic < 0.5 ~ "Muy subrepresentado (<0.5)",
      iic < 1 ~ "Subrepresentado (0.5-1)",
      iic < 2 ~ "Intenso (1-2)",
      TRUE ~ "Muy intenso (>2)"
    ) |> factor(levels = c("Sin comercio", "Muy subrepresentado (<0.5)",
                           "Subrepresentado (0.5-1)", "Intenso (1-2)", "Muy intenso (>2)"))
  )

g_heatmap <- ggplot(iic_heatmap_cat, aes(x = socio_label, y = reorder(cuci_desc, iic), fill = categoria)) +
  geom_tile(color = "gray95", linewidth = 0.8) +
  geom_text(aes(label = ifelse(iic == 0, "—", round(iic, 2))), size = 3, color = "gray15") +
  scale_fill_manual(values = c(
    "Sin comercio" = "gray88", "Muy subrepresentado (<0.5)" = "#fee0d2",
    "Subrepresentado (0.5-1)" = "#fc9272", "Intenso (1-2)" = "#a1d99b", "Muy intenso (>2)" = "#31a354"
  )) +
  labs(title = "IIC de los bienes más exportados por Marruecos, por socio comercial",
       subtitle = "2024 · bienes ordenados por volumen exportado al mundo",
       x = "Socio comercial", y = NULL, fill = "Intensidad",
       caption = "IIC = 1 indica comercio bilateral proporcional al patrón exportador global de Marruecos.") +
  theme_tp1(base_size = 12) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.ticks = element_blank())
g_heatmap
ggsave(paste0(ruta_graficos, "grafico_heatmap_iic.png"), g_heatmap,
       width = 11, height = 7, dpi = 300, bg = "white")


#===============================================================================#
# BLOQUE 5: ICC - Índice de Complementariedad Comercial (Michaely, 1996)
#===============================================================================#

calcular_icc <- function(socio, anio) {
  m_mar <- mar_imp |> filter(p == "WLD", year == anio) |> select(cuci, cuci_desc, m = share)
  x_socio <- comtrade_socios |>
    filter(r == socio, p == "WLD", flow == "Export", year == anio) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, x)
  tabla <- full_join(m_mar, x_socio, by = "cuci") |>
    mutate(m = replace_na(m, 0), x = replace_na(x, 0), dif_abs = abs(m - x))
  icc <- 100 * (1 - sum(tabla$dif_abs, na.rm = TRUE) / 2)
  list(icc = icc, detalle = tabla)
}

# Ranking de ICC entre 9 candidatos, para elegir los socios finales con criterio empírico
candidatos <- c("ESP", "FRA", "DEU", "BRA", "IND", "CHN", "ARE", "GBR", "USA")
ranking_icc <- map_dfr(candidatos, ~tibble(socio = .x, icc = calcular_icc(.x, 2024)$icc)) |>
  arrange(desc(icc))
ranking_icc

# Evolución del ICC (excluyendo Brasil)
icc_evolucion <- map_dfr(2021:2025, function(a) {
  map_dfr(socios_finales, ~tibble(year = a, socio = .x, icc = calcular_icc(.x, a)$icc))
})

g_icc <- icc_evolucion |>
  filter(socio != "BRA") |>
  ggplot(aes(x = year, y = icc, color = socio)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  scale_color_manual(values = paleta_socios, labels = nombres_socio) +
  labs(title = "Evolución del ICC de Marruecos por socio comercial",
       subtitle = "España, Francia, Alemania y EE.UU. — 2021-2025",
       x = "Año", y = "ICC", color = "Socio",
       caption = "Se excluye Brasil (ICC estructuralmente bajo) para no distorsionar la escala.") +
  theme_tp1()
g_icc
ggsave(paste0(ruta_graficos, "grafico_evolucion_icc.png"), g_icc,
       width = 10, height = 6.5, dpi = 300, bg = "white")


#===============================================================================#
# BLOQUE 6: GRÁFICO INTEGRADOR — VCR x IIC x Volumen, por socio
#===============================================================================#

armar_cruce <- function(socio, anio = 2024) {
  vcr_mar |>
    filter(year == anio) |>
    select(cuci, cuci_desc, vcrn) |>
    inner_join(calcular_iic_multi(socio, unique(vcr_mar$cuci), anio) |> select(cuci, iic), by = "cuci") |>
    filter(!is.na(iic), is.finite(iic), iic > 0) |>
    left_join(mar_exp |> filter(p == socio, year == anio) |> select(cuci, value), by = "cuci") |>
    filter(!is.na(value), value > 0) |>
    mutate(socio = socio)
}

destacados_ids <- c("562", "272", "842", "773", "522")

graficar_bubble <- function(socio_code, iic_min = 0.02, n_extra = 3) {
  datos <- armar_cruce(socio_code) |> filter(iic >= iic_min)
  
  destacados_fijos <- datos |> filter(cuci %in% destacados_ids)
  destacados_extra <- datos |>
    filter(iic > 1, !cuci %in% destacados_ids) |>
    slice_max(value, n = n_extra)
  destacados_s <- bind_rows(destacados_fijos, destacados_extra) |>
    distinct(cuci, .keep_all = TRUE)
  
  n_dest <- nrow(destacados_s)
  colores_solidos <- brewer.pal(max(n_dest, 3), "Set1")[1:n_dest]
  names(colores_solidos) <- destacados_s$cuci_desc
  colores_relleno <- alpha(colores_solidos, 0.35)
  
  rango_valor <- range(sqrt(datos$value))
  destacados_s <- destacados_s |>
    mutate(size_real = rescale(sqrt(value), from = rango_valor, to = c(1, 14)))
  
  ggplot(datos, aes(x = vcrn, y = iic)) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = 1, ymax = Inf, fill = "steelblue", alpha = 0.06) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 1, fill = "firebrick", alpha = 0.06) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_point(aes(size = value), shape = 21, color = "gray50", fill = alpha("gray50", 0.15), stroke = 0.4) +
    geom_point(data = destacados_s, aes(size = value, color = cuci_desc, fill = cuci_desc), shape = 21, stroke = 1.1) +
    geom_text_repel(
      data = destacados_s, aes(label = cuci_desc, color = cuci_desc),
      size = 3.3, fontface = "bold", show.legend = FALSE,
      point.size = destacados_s$size_real, box.padding = 1, min.segment.length = 0,
      segment.color = "gray50", force = 10, max.iter = 15000
    ) +
    scale_y_log10(labels = label_number(accuracy = 0.01)) +
    scale_x_continuous(limits = c(-1.05, 1.05), breaks = seq(-1, 1, 0.5)) +
    scale_size_continuous(range = c(1, 14)) +
    scale_color_manual(values = colores_solidos) +
    scale_fill_manual(values = colores_relleno) +
    guides(color = "none", fill = "none", size = "none") +
    labs(
      title = paste("VCR vs. IIC — Marruecos hacia", nombres_socio[socio_code]),
      subtitle = "2024 · el tamaño del punto es el valor exportado",
      x = "VCRN — ventaja comparativa revelada normalizada", y = "IIC (escala log)",
      caption = paste0(
        "Línea punteada horizontal: IIC = 1 · vertical: VCRN = 0\n",
        "Se excluyen productos con IIC < ", iic_min, ". Etiquetas: 5 productos de referencia",
        if (n_extra > 0) paste0(" + hasta ", n_extra, " de mayor volumen con IIC > 1 propios de este socio.") else "."
      )
    ) +
    theme_tp1()
}

# Umbral de IIC ajustado por socio (Alemania necesita uno más permisivo porque
# su comercio bilateral con Marruecos está más disperso entre muchos productos
# de bajo valor unitario; con 0.02 casi no quedaban puntos para graficar)
umbrales_iic <- c(ESP = 0.02, FRA = 0.02, DEU = 0.001, USA = 0.02, BRA = 0.02)

for (s in names(umbrales_iic)) {
  p <- graficar_bubble(s, iic_min = umbrales_iic[[s]])
  print(p)
  ggsave(paste0(ruta_graficos, "bubble_", s, ".png"), plot = p,
         width = 10, height = 6.5, dpi = 300, bg = "white")
}


#===============================================================================#
# BLOQUE 7: EXPORTACIÓN DE TABLAS FINALES
#===============================================================================#
# Se guardan solo las tablas que constituyen resultados finales del TP1
# (las que responden directamente a la consigna). Quedan afuera las tablas
# puramente auxiliares/intermedias que sólo existen para armar un gráfico
# (lookup_2dig, detalle_composicion, detalle_apilado, etiquetas_top,
# totales_division, vcr_aux_mar, variacion_vcrn, iic_heatmap*, etc.).

if (!dir.exists(ruta_tablas)) dir.create(ruta_tablas, recursive = TRUE)

tablas_finales <- list(
  # --- Bloque 2: perfil de comercio exterior ---
  "top_socios"          = top_socios,          # ranking de socios comerciales por exportación
  "destino_fertilizantes" = destino_fertilizantes, # destino de los fertilizantes (272 y 562)
  "sectores_2dig_exp"    = sectores_2dig_exp,   # principales divisiones exportadoras (2 dígitos)
  "sectores_2dig_imp"    = sectores_2dig_imp,   # principales divisiones importadoras (2 dígitos)
  "top_bienes_exp"       = top_bienes_exp,      # principales bienes exportados (3 dígitos)
  "top_bienes_imp"       = top_bienes_imp,      # principales bienes importados (3 dígitos)
  "comercio_total"       = comercio_total,      # evolución de expo/impo totales
  "saldo_comercial"      = saldo_comercial,     # saldo comercial por año
  "tabla_paises_exp"     = tabla_paises_exp,    # principales países destino, por bien exportado
  "tabla_paises_imp"     = tabla_paises_imp,    # principales países origen, por bien importado
  "ranking_paises_imp"   = ranking_paises_imp,  # ranking de países por importaciones totales

  # --- Bloque 3: VCR (Balassa) ---
  "top_ventaja"          = top_ventaja,         # mayores ventajas comparativas reveladas (VCRN), 2024
  "top_desventaja"       = top_desventaja,      # mayores desventajas comparativas reveladas (VCRN), 2024
  "top_volumen"          = top_volumen,         # bienes de mayor volumen exportado, 2024

  # --- Bloque 5: ICC (Michaely) ---
  "ranking_icc"          = ranking_icc,         # ranking de socios candidatos por ICC (criterio de selección)
  "icc_evolucion"        = icc_evolucion        # evolución del ICC por socio final, 2021-2025
)

for (nombre in names(tablas_finales)) {
  write_csv(tablas_finales[[nombre]], paste0(ruta_tablas, nombre, ".csv"))
}

#===============================================================================#
# BLOQUE 8: OBJETOS LIVIANOS PARA HEREDAR EN OTROS TPs
#===============================================================================#
# El TP2 (y los que sigan) heredan objetos de este script vía source(), lo
# que obliga a reprocesar las bases .dta de WITS y regenerar los 14 gráficos
# cada vez. Guardar acá los objetos puntuales que necesitan los TPs
# siguientes permite que carguen esto en menos de un segundo con load() en
# vez de correr todo el script - ver el header de scripts/PRUEBAS/02/02_MFE_HO.R.

save(vcr_mar, iic_heatmap_completo, mar_exp, theme_tp1,
     file = paste0(ruta_tablas, "objetos_heredados_tp1.RData"))
