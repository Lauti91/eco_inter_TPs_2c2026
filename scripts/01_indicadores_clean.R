#===============================================================================#
# TP1 - Economía Internacional: Indicadores de Comercio - Marruecos
# VCR (Balassa), IIC (Yeats), ICC (Michaely)
#===============================================================================#

library(tidyverse)
library(haven)
library(ggrepel)

setwd("/cloud/project/bases de datos")
getwd()
#===============================================================================#
# 1) CARGA Y LIMPIEZA DE DATOS
#===============================================================================#

# Base principal: MAR como reporter, con ESP/FRA/WLD como partners
comtrade <- read_dta("MAR_ESP_FRA_WLD_full.dta")
names(comtrade) <- tolower(names(comtrade))

comtrade <- comtrade %>%
  mutate(
    cuci      = as.character(as_factor(productcode)),
    cuci_desc = as.character(as_factor(productdescription)),
    r         = as.character(as_factor(reporteriso3)),
    p         = as.character(as_factor(partneriso3)),
    flow      = as.character(as_factor(tradeflowname))
  ) %>%
  rename(value = tradevaluein1000usd) %>%
  select(r, p, flow, cuci, cuci_desc, value, year)

# Base auxiliar: WLD como reporter y partner (necesaria para el denominador del VCR)
comtrade_wld <- read_dta("WLD_WLD.dta")
names(comtrade_wld) <- tolower(names(comtrade_wld))

comtrade_wld <- comtrade_wld %>%
  mutate(
    cuci = as.character(as_factor(productcode)),
    r    = as.character(as_factor(reporteriso3)),
    p    = as.character(as_factor(partneriso3)),
    flow = as.character(as_factor(tradeflowname))
  ) %>%
  rename(value = tradevaluein1000usd) %>%
  select(r, p, flow, cuci, value, year)

# Exportaciones de MAR/ESP/FRA con share y total por reporter-partner-año
# (base de trabajo para VCR e IIC)
comtrade_exp <- comtrade |>
  filter(flow == "Export") |>
  group_by(year, r, p) |>
  mutate(total_expo = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = value / total_expo)


#===============================================================================#
# 2) VCR - Ventajas Comparativas Reveladas (Balassa, 1965)
#===============================================================================#

# Share de Marruecos en cada producto (numerador)
vcr_aux_mar <- comtrade_exp |>
  filter(r == "MAR", p == "WLD") |>
  select(year, cuci, cuci_desc, share_mar = share)

# Share mundial de cada producto (denominador)
wld_exp <- comtrade_wld |>
  filter(flow == "Export", r == "All", p == "All") |>
  group_by(year) |>
  mutate(total_wld = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share_wld = value / total_wld) |>
  select(year, cuci, share_wld)

vcr_mar <- vcr_aux_mar |>
  left_join(wld_exp, by = c("year", "cuci")) |>
  mutate(
    vcr  = share_mar / share_wld,
    vcrn = (vcr - 1) / (vcr + 1)
  ) |>
  arrange(desc(vcr))

sum(is.na(vcr_mar$share_wld))  # chequeo de NA 

# Top 5 sectores con mayor / menor ventaja comparativa (2024)
top5_ventaja <- vcr_mar |> filter(year == 2024) |> slice_max(vcrn, n = 5)
top5_desventaja <- vcr_mar |> filter(year == 2024) |> slice_min(vcrn, n = 5)
sectores_top5 <- top5_ventaja$cuci

# --- Gráfico: top 5 ventaja vs. top 5 desventaja (2024) ---
ggplot(top5_ventaja) +
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "steelblue") +
  coord_flip() +
  labs(title = "Marruecos (2024) — Mayor ventaja comparativa",
       x = "Producto", y = "VCRN") +
  theme_minimal()

ggplot(top5_desventaja) +
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "firebrick") +
  coord_flip() +
  labs(title = "Marruecos (2024) — Mayor desventaja comparativa",
       x = "Producto", y = "VCRN") +
  theme_minimal()

# --- Gráfico: evolución del VCRN de los sectores top (2021-2025) ---
vcr_mar |>
  filter(cuci %in% sectores_top5) |>
  ggplot(aes(x = year, y = vcrn, color = cuci_desc)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Evolución del VCRN — principales sectores de Marruecos",
       subtitle = "2021-2025",
       x = "Año", y = "VCRN", color = "Producto") +
  theme_minimal()


#===============================================================================#
# 3) IIC - Índice de Intensidad Comercial (Yeats, 1997)
#===============================================================================#

# Función: IIC de Marruecos hacia un socio, para un set de sectores, en un año dado
calcular_iic_multi <- function(socio, sectores, anio) {
  mar_soc <- comtrade_exp |>
    filter(r == "MAR", p == socio, year == anio, cuci %in% sectores) |>
    select(cuci, cuci_desc, xijk = value, xij = total_expo)
  
  mar_wld <- comtrade_exp |>
    filter(r == "MAR", p == "WLD", year == anio, cuci %in% sectores) |>
    select(cuci, xik = value, xi = total_expo)
  
  mar_soc |>
    left_join(mar_wld, by = "cuci") |>
    mutate(iic = as.numeric((xijk / xik) / (xij / xi)), year = anio) |>
    select(year, cuci, cuci_desc, iic)
}

# IIC 2024 de los 5 sectores top hacia España
iic_top5_esp <- calcular_iic_multi("ESP", sectores_top5, 2024) |> arrange(desc(iic))
iic_top5_esp

# --- Gráfico: evolución del IIC (2021-2025), top 5 sectores hacia España ---
iic_evolucion <- map_dfr(2021:2025, ~calcular_iic_multi("ESP", sectores_top5, .x))

ggplot(iic_evolucion, aes(x = year, y = iic, color = cuci_desc)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(title = "Evolución del IIC — Marruecos hacia España",
       subtitle = "Top 5 sectores con mayor VCR de Marruecos, 2021-2025",
       x = "Año", y = "IIC", color = "Producto") +
  theme_minimal()


#===============================================================================#
# 4) ICC - Índice de Complementariedad Comercial (Michaely, 1996)
#===============================================================================#

# --- ICC agregado (un número por socio) ---
calcular_icc <- function(socio, anio) {
  m_mar <- comtrade |>
    filter(r == "MAR", p == "WLD", flow == "Import", year == anio) |>
    mutate(m = value / sum(value, na.rm = TRUE)) |>
    select(cuci, cuci_desc, m)
  
  x_socio <- comtrade |>
    filter(r == socio, p == "WLD", flow == "Export", year == anio) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, x)
  
  tabla <- full_join(m_mar, x_socio, by = "cuci") |>
    mutate(m = replace_na(m, 0), x = replace_na(x, 0), dif_abs = abs(m - x))
  
  icc <- 100 * (1 - sum(tabla$dif_abs, na.rm = TRUE) / 2)
  list(icc = icc, detalle = tabla)
}

icc_esp_mar <- calcular_icc("ESP", 2024)
icc_esp_mar$icc

icc_fra_mar <- calcular_icc("FRA", 2024)
icc_fra_mar$icc

# Chequeos: los shares de cada lado deberían sumar ~1
sum(icc_esp_mar$detalle$m, na.rm = TRUE)
sum(icc_esp_mar$detalle$x, na.rm = TRUE)

# --- Evolución del ICC agregado, España vs. Francia (2021-2025) ---
icc_evolucion <- map_dfr(2021:2025, function(a) {
  tibble(year = a,
         icc_esp = calcular_icc("ESP", a)$icc,
         icc_fra = calcular_icc("FRA", a)$icc)
})

icc_evolucion |>
  pivot_longer(cols = c(icc_esp, icc_fra), names_to = "socio", values_to = "icc") |>
  mutate(socio = recode(socio, icc_esp = "España", icc_fra = "Francia")) |>
  ggplot(aes(x = year, y = icc, color = socio)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Índice de Complementariedad Comercial de Marruecos",
       subtitle = "España vs. Francia, 2021-2025",
       x = "Año", y = "ICC", color = "Socio") +
  theme_minimal()

# --- ICC por producto (ranking de matching, sin la sumatoria) ---
calcular_icc_producto <- function(socio, anio) {
  m_mar <- comtrade |>
    filter(r == "MAR", p == "WLD", flow == "Import", year == anio) |>
    mutate(m = value / sum(value, na.rm = TRUE)) |>
    select(cuci, cuci_desc, m)
  
  x_socio <- comtrade |>
    filter(r == socio, p == "WLD", flow == "Export", year == anio) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, cuci_desc_x = cuci_desc, x)
  
  full_join(m_mar, x_socio, by = "cuci") |>
    mutate(
      cuci_desc = coalesce(cuci_desc, cuci_desc_x),
      m = replace_na(m, 0),
      x = replace_na(x, 0),
      icc_k = 100 * (1 - abs(m - x) / 2)
    ) |>
    select(cuci, cuci_desc, m, x, icc_k) |>
    arrange(desc(icc_k))
}

icc_prod_esp <- calcular_icc_producto("ESP", 2024)
icc_prod_fra <- calcular_icc_producto("FRA", 2024)

# Top 10 de mejor matching real (se excluyen productos marginales de ambos lados)
icc_prod_esp |> filter(m > 0.001, x > 0.001) |> arrange(desc(icc_k)) |> head(10)
icc_prod_fra |> filter(m > 0.001, x > 0.001) |> arrange(desc(icc_k)) |> head(10)


#===============================================================================#
# 5) GRÁFICO INTEGRADOR: VCR x IIC x Volumen comerciado
#===============================================================================#

# Cruce VCRN (2024) con IIC hacia España (2024), para todos los productos en común
cruce_vcr_iic <- vcr_mar |>
  filter(year == 2024) |>
  select(cuci, cuci_desc, vcrn) |>
  inner_join(
    calcular_iic_multi("ESP", unique(vcr_mar$cuci), 2024) |> select(cuci, iic),
    by = "cuci"
  ) |>
  filter(!is.na(iic), is.finite(iic), iic > 0)

# Sumamos el volumen exportado a España (tamaño de la burbuja)
cruce_vcr_iic_vol <- cruce_vcr_iic |>
  left_join(
    comtrade_exp |> filter(r == "MAR", p == "ESP", year == 2024) |> select(cuci, value),
    by = "cuci"
  ) |>
  filter(!is.na(value), value > 0)

# Se excluyen productos con IIC muy bajo (< 0.02) para mejorar la legibilidad
n_excluidos <- sum(cruce_vcr_iic_vol$iic < 0.02, na.rm = TRUE)
cruce_vcr_iic_vol_recortado <- cruce_vcr_iic_vol |> filter(iic >= 0.02)

destacados_vol <- cruce_vcr_iic_vol_recortado |>
  filter(cuci %in% c("562", "272", "842", "773", "522"))

ggplot(cruce_vcr_iic_vol_recortado, aes(x = vcrn, y = iic)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 1, ymax = Inf,
           fill = "steelblue", alpha = 0.06) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 1,
           fill = "firebrick", alpha = 0.06) +
  geom_point(aes(size = value), alpha = 0.35, color = "gray40") +
  geom_point(data = destacados_vol, aes(size = value, color = cuci_desc)) +
  geom_text_repel(
    data = destacados_vol, aes(label = cuci_desc, color = cuci_desc),
    size = 3.5, fontface = "bold", show.legend = FALSE,
    box.padding = 0.8, point.padding = 0.4,
    segment.color = "gray50", min.segment.length = 0
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
  scale_size_continuous(range = c(1, 14),
                        labels = scales::label_number(suffix = "k US$")) +
  scale_color_brewer(palette = "Set1") +
  guides(color = "none", size = "none") +
  labs(
    title = "Ventaja comparativa global, intensidad bilateral y volumen — Marruecos hacia España",
    subtitle = "2024 · el tamaño del punto es el valor exportado a España",
    x = "VCRN — ventaja comparativa revelada normalizada",
    y = "IIC (escala log)",
    caption = paste0(
      "Línea punteada horizontal: IIC = 1 (comercio ni más ni menos intenso de lo esperado)\n",
      "Línea punteada vertical: VCRN = 0 (frontera entre ventaja y desventaja comparativa)\n",
      "Se excluyen ", n_excluidos, " productos con IIC < 0.02 para mejorar la legibilidad del gráfico"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 11),
    plot.caption = element_text(color = "gray50", size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92")
  )

ggsave("bubble_vcr_iic_espana.png", width = 11, height = 7, dpi = 300, bg = "white")
