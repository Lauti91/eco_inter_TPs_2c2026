#===============================================================================#
# TP1 - Economía Internacional: Indicadores de Comercio - Marruecos
# VCR (Balassa), IIC (Yeats), ICC (Michaely)
# Bases definitivas: MAR_ALLPARTNERS_WLD, SOCIOS_WLD, WLD_WLD
#===============================================================================#

library(tidyverse)
library(haven)
library(ggrepel)

setwd("/cloud/project/bases de datos")

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


#===============================================================================#
# 1) CARGA Y LIMPIEZA DE LAS 3 BASES DEFINITIVAS
#===============================================================================#

# Reporter = MAR, Partners = 77 países + WLD (socios, sectores 2 díg., destino de fertilizantes, VCR numerador, IIC)
comtrade_mar <- limpiar_wits("MAR_ALLPARTNERS_WLD.dta")
distinct(comtrade_mar, r); nrow(comtrade_mar)  # chequeo: nrow no debería ser un redondo tipo 100000

# Reporters = ESP/FRA/DEU/BRA/IND/CHN/ARE/GBR/USA, Partner = WLD (para el ICC)
comtrade_socios <- limpiar_wits("SOCIOS_WLD.dta")
distinct(comtrade_socios, r); nrow(comtrade_socios)

# Reporter = World, Partner = World (denominador del VCR) - ojo, acá "World" = "All"
comtrade_wld <- limpiar_wits("WLD_WLD.dta", incluir_desc = FALSE)
distinct(comtrade_wld, r, p)


#===============================================================================#
# 2) BASES DE TRABAJO DERIVADAS
#===============================================================================#

# Exportaciones de Marruecos (share y total, por partner-año) - sirve para VCR e IIC
mar_exp <- comtrade_mar |>
  filter(flow == "Export") |>
  group_by(year, p) |>
  mutate(total_expo = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = value / total_expo)

# Importaciones de Marruecos (share y total) - sirve para el ICC
mar_imp <- comtrade_mar |>
  filter(flow == "Import") |>
  group_by(year, p) |>
  mutate(total_impo = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = value / total_impo)

# Share mundial de cada producto (denominador del VCR)
wld_exp <- comtrade_wld |>
  filter(flow == "Export", r == "All", p == "All") |>
  group_by(year) |>
  mutate(total_wld = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share_wld = value / total_wld) |>
  select(year, cuci, share_wld)


#===============================================================================#
# 3) VCR - Ventajas Comparativas Reveladas (Balassa, 1965)
#===============================================================================#

vcr_aux_mar <- mar_exp |>
  filter(p == "WLD") |>
  select(year, cuci, cuci_desc, share_mar = share)

vcr_mar <- vcr_aux_mar |>
  left_join(wld_exp, by = c("year", "cuci")) |>
  mutate(vcr = share_mar / share_wld, vcrn = (vcr - 1) / (vcr + 1)) |>
  arrange(desc(vcr))

sum(is.na(vcr_mar$share_wld))  # chequeo de NA

# Top 10-15 sectores con mayor / menor ventaja comparativa (2024)
top_ventaja <- vcr_mar |> filter(year == 2024) |> slice_max(vcrn, n = 12)
top_desventaja <- vcr_mar |> filter(year == 2024) |> slice_min(vcrn, n = 12)
sectores_top <- top_ventaja$cuci

ggplot(top_ventaja) +
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "steelblue") +
  coord_flip() +
  labs(title = "Marruecos (2024) — Mayor ventaja comparativa", x = "Producto", y = "VCRN") +
  theme_minimal()

ggplot(top_desventaja) +
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "firebrick") +
  coord_flip() +
  labs(title = "Marruecos (2024) — Mayor desventaja comparativa", x = "Producto", y = "VCRN") +
  theme_minimal()

vcr_mar |>
  filter(cuci %in% sectores_top) |>
  ggplot(aes(x = year, y = vcrn, color = cuci_desc)) +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Evolución del VCRN — principales sectores de Marruecos",
       subtitle = "2021-2025", x = "Año", y = "VCRN", color = "Producto") +
  theme_minimal()


#===============================================================================#
# 4) IIC - Índice de Intensidad Comercial (Yeats, 1997)
#===============================================================================#

# Generalizada: funciona con cualquier partner presente en mar_exp (ya no solo ESP/FRA)
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

iic_top_esp <- calcular_iic_multi("ESP", sectores_top, 2024) |> arrange(desc(iic))
iic_top_esp

iic_evolucion <- map_dfr(2021:2025, ~calcular_iic_multi("ESP", sectores_top, .x))

ggplot(iic_evolucion, aes(x = year, y = iic, color = cuci_desc)) +
  geom_line(linewidth = 1) + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(title = "Evolución del IIC — Marruecos hacia España",
       subtitle = "Top sectores con mayor VCR de Marruecos, 2021-2025",
       x = "Año", y = "IIC", color = "Producto") +
  theme_minimal()


#===============================================================================#
# 5) ICC - Índice de Complementariedad Comercial (Michaely, 1996)
#===============================================================================#

calcular_icc <- function(socio, anio) {
  m_mar <- mar_imp |>
    filter(p == "WLD", year == anio) |>
    select(cuci, cuci_desc, m = share)
  
  x_socio <- comtrade_socios |>
    filter(r == socio, p == "WLD", flow == "Export", year == anio) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, x)
  
  tabla <- full_join(m_mar, x_socio, by = "cuci") |>
    mutate(m = replace_na(m, 0), x = replace_na(x, 0), dif_abs = abs(m - x))
  
  icc <- 100 * (1 - sum(tabla$dif_abs, na.rm = TRUE) / 2)
  list(icc = icc, detalle = tabla)
}

# --- Ranking de ICC entre los 9 candidatos, para elegir los socios finales con criterio empírico ---
candidatos <- c("ESP", "FRA", "DEU", "BRA", "IND", "CHN", "ARE", "GBR", "USA")

ranking_icc <- map_dfr(candidatos, ~tibble(socio = .x, icc = calcular_icc(.x, 2024)$icc)) |>
  arrange(desc(icc))

ranking_icc

ggplot(ranking_icc, aes(x = reorder(socio, icc), y = icc)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Ranking de ICC de Marruecos por socio comercial (2024)",
       x = "Socio", y = "ICC") +
  theme_minimal()

# Socios finales elegidos con base en el ranking + relevancia por volumen/VCR
# (ajustar según lo que decida el grupo tras ver ranking_icc)
socios_finales <- c("ESP", "FRA", "DEU", "USA", "BRA")

# --- Evolución del ICC agregado para los socios finales (2021-2025) ---
icc_evolucion <- map_dfr(2021:2025, function(a) {
  map_dfr(socios_finales, ~tibble(year = a, socio = .x, icc = calcular_icc(.x, a)$icc))
})

ggplot(icc_evolucion, aes(x = year, y = icc, color = socio)) +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Evolución del ICC de Marruecos por socio", x = "Año", y = "ICC", color = "Socio") +
  theme_minimal()

# --- ICC por producto (ranking de matching, sin la sumatoria) ---
calcular_icc_producto <- function(socio, anio) {
  m_mar <- mar_imp |>
    filter(p == "WLD", year == anio) |>
    select(cuci, cuci_desc, m = share)
  
  x_socio <- comtrade_socios |>
    filter(r == socio, p == "WLD", flow == "Export", year == anio) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, cuci_desc_x = cuci_desc, x)
  
  full_join(m_mar, x_socio, by = "cuci") |>
    mutate(
      cuci_desc = coalesce(cuci_desc, cuci_desc_x),
      m = replace_na(m, 0), x = replace_na(x, 0),
      icc_k = 100 * (1 - abs(m - x) / 2)
    ) |>
    select(cuci, cuci_desc, m, x, icc_k) |>
    arrange(desc(icc_k))
}

icc_prod_por_socio <- map(socios_finales, ~calcular_icc_producto(.x, 2024)) |>
  set_names(socios_finales)

# Ejemplo de uso: top 10 de mejor matching real con España
icc_prod_por_socio$ESP |> filter(m > 0.001, x > 0.001) |> head(10)


#===============================================================================#
# 6) SEGUIMIENTO: Top bienes por VOLUMEN exportado vs. top bienes por VCRN,
#    y cómo se comportan (IIC) esos bienes de alto volumen en cada socio.
#===============================================================================#

# a) Top bienes por volumen exportado (MAR -> WLD, 2024)
top_volumen <- mar_exp |>
  filter(p == "WLD", year == 2024) |>
  select(cuci, cuci_desc, value) |>
  arrange(desc(value)) |>
  slice_max(value, n = 12)

top_volumen

# b) Top bienes por VCRN (ya calculado arriba: top_ventaja)
top_ventaja

# c) Intersección: ¿los bienes de mayor volumen son también los de mayor ventaja comparativa?
interseccion <- intersect(top_volumen$cuci, top_ventaja$cuci)
length(interseccion)
interseccion

# d) Para los bienes de mayor VOLUMEN (no necesariamente los de mayor VCR),
#    calculamos el IIC hacia cada uno de los socios finales -> heatmap
sectores_volumen <- top_volumen$cuci

iic_heatmap <- map_dfr(socios_finales, function(s) {
  calcular_iic_multi(s, sectores_volumen, 2024) |> mutate(socio = s)
})

ggplot(iic_heatmap, aes(x = socio, y = reorder(cuci_desc, iic), fill = iic)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(iic, 2)), size = 3, color = "black") +
  scale_fill_gradient2(
    low = "firebrick", mid = "white", high = "steelblue",
    midpoint = 1, trans = "log10", name = "IIC"
  ) +
  labs(
    title = "IIC de los bienes más exportados por Marruecos, por socio comercial",
    subtitle = "2024 · bienes ordenados por volumen exportado al mundo",
    x = "Socio comercial", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

# e) Contraste explícito: ¿los bienes de MAYOR volumen son los que tienen mayor VCRN?
comparacion_vol_vcrn <- top_volumen |>
  left_join(vcr_mar |> filter(year == 2024) |> select(cuci, vcrn), by = "cuci") |>
  arrange(desc(value))

comparacion_vol_vcrn


#===============================================================================#
# 7) GRÁFICO INTEGRADOR: VCR x IIC x Volumen, comparando socios
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

cruce_dos_socios <- bind_rows(armar_cruce("ESP"), armar_cruce("BRA"))  # ajustar socio de contraste

destacados_dos <- cruce_dos_socios |>
  filter(cuci %in% c("562", "272", "842", "773", "522"))

ggplot(cruce_dos_socios, aes(x = vcrn, y = iic)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.4) +
  geom_point(aes(size = value), alpha = 0.35, color = "gray40") +
  geom_point(data = destacados_dos, aes(size = value, color = cuci_desc)) +
  scale_y_log10() +
  scale_size_continuous(range = c(1, 12)) +
  scale_color_brewer(palette = "Set1") +
  guides(color = "none", size = "none") +
  facet_wrap(~socio) +
  labs(title = "VCR vs. IIC — comparación entre socios",
       subtitle = "2024 · el tamaño del punto es el valor exportado a cada socio",
       x = "VCRN", y = "IIC (escala log)") +
  theme_minimal(base_size = 12)

ggsave("comparacion_socios_vcr_iic.png", width = 12, height = 6.5, dpi = 300, bg = "white")