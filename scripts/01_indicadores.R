install.packages("tidyverse")
install.packages("haven")
install.packages("ggrepel")

library(tidyverse)
library(haven)
library(dplyr)
library(ggrepel) 

getwd()
setwd("/cloud/project/bases de datos")
comtrade <- read_dta("MAR_ESP_FRA_WLD_full.dta")

names(comtrade) <- tolower(names(comtrade))

#-----------------------------------------------------------------------------#
# Limpieza de Bases: Uso dos bases, una con MAR como reporter y FRA, SPN y WLD
# como partners, y otra con WLD como reporter y partner. Después las uno con 
# left_join y armo el VCR.
#-----------------------------------------------------------------------------#

# Primero para la parte de MAR -> FRA, SPN, WLD:

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

comtrade_2 <- comtrade |>
  mutate(value = value / 1000)

comtrade_exp <- comtrade_2 |>
  filter(flow == "Export")

comtrade_exp <- comtrade_exp |>
  group_by(year, r, p) |>
  mutate(total_expo = sum(value, na.rm = TRUE)) |>
  ungroup()

comtrade_exp <- comtrade_exp |>
  mutate(share = value / total_expo)

df_share <- comtrade_exp |>
  select(year, r, p, cuci, cuci_desc, share)

vcr_aux_mar <- df_share |>
  filter(r == "MAR", p == "WLD") |>
  rename(share_mar = share)

# Ahora para la parte de WLD -> WLD:

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

wld_exp <- comtrade_wld |>
  filter(flow == "Export", r == "All", p == "All") |>
  group_by(year) |>
  mutate(total_wld = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share_wld = value / total_wld) |>
  select(year, cuci, share_wld)

#-----------------------------------------------------------------------------#
# Y ahora sí construyo el VCR:
#-----------------------------------------------------------------------------#

vcr_mar <- vcr_aux_mar |>
  select(year, cuci, cuci_desc, share_mar) |>
  left_join(wld_exp, by = c("year", "cuci")) |>
  mutate(
    vcr  = share_mar / share_wld,
    vcrn = (vcr - 1) / (vcr + 1)
  ) |>
  arrange(desc(vcr))

sum(is.na(vcr_mar$share_wld)) # chequeo que no quede ningún NA suelto

top5_mar <- vcr_mar |>
  filter(year == 2025) |>
  slice_max(vcrn, n = 5)

# Analisis de los resultados del VCR

vcr_mar |> 
  group_by(year) |> 
  summarise(n = n(), na_share_wld = sum(is.na(share_wld)))

# Top 5 sectores con mayor ventaja comparativa (VCRN más alto)
top5_ventaja <- vcr_mar |>
  filter(year == 2024) |>
  slice_max(vcrn, n = 5)


# Top 5 sectores con mayor desventaja comparativa (VCRN más bajo)
top5_desventaja <- vcr_mar |>
  filter(year == 2024) |>
  slice_min(vcrn, n = 5)


# Me parecen medio una poronga estos gráficos, pero bueno, hay que verlo

ggplot(data = top5_ventaja) +
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "steelblue") +
  coord_flip() +
  labs(title = "Marruecos (2024) — Mayor ventaja comparativa",
       x = "Producto",
       y = "VCRN")

ggplot(data = top5_desventaja) +
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "firebrick") +
  coord_flip() +
  labs(title = "Marruecos (2024) — Mayor desventaja comparativa",
       x = "Producto",
       y = "VCRN")


# Grafico de evolución de los sectores top

top_sectores <- top5_ventaja$cuci  # códigos de los 5 sectores top en 2024

vcr_mar |>
  filter(cuci %in% top_sectores) |>
  ggplot(aes(x = year, y = vcrn, color = cuci_desc)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Evolución del VCRN — principales sectores de Marruecos (2021-2025)",
       x = "Año", y = "VCRN", color = "Producto")

#-----------------------------------------------------------------------------#
# Siguiente indicador: IIC (Intensidad Comercial)
#-----------------------------------------------------------------------------#


# Elegimos el socio y el sector a analizar
socio_1 <- "ESP"
sector <- "562"  # Manufactured fertilizers
anio   <- 2024

socio_2 <- "FRA"

# X^{i,j}_k: exportaciones de Marruecos a ESPAÑA en el sector k
xijk_esp <- comtrade_exp |>
  filter(r == "MAR", p == socio_1, cuci == sector, year == anio) |>
  pull(value)

# X^{i,j}_k: exportaciones de Marruecos a FRANCIA en el sector k
xijk_fra <- comtrade_exp |>
  filter(r == "MAR", p == socio_2, cuci == sector, year == anio) |>
  pull(value)

# X^i_k: exportaciones totales de Marruecos al mundo en el sector k
xik <- comtrade_exp |>
  filter(r == "MAR", p == "WLD", cuci == sector, year == anio) |>
  pull(value)

# X^{i,j}: exportaciones totales de Marruecos a España (todos los productos)
xij_esp <- comtrade_exp |>
  filter(r == "MAR", p == socio_1, year == anio) |>
  distinct(total_expo) |>
  pull(total_expo)

# X^{i,j}: exportaciones totales de Marruecos a FRANCIA (todos los productos)
xij_fra <- comtrade_exp |>
  filter(r == "MAR", p == socio_2, year == anio) |>
  distinct(total_expo) |>
  pull(total_expo)

# X^i: exportaciones totales de Marruecos al mundo (todos los productos)
xi <- comtrade_exp |>
  filter(r == "MAR", p == "WLD", year == anio) |>
  distinct(total_expo) |>
  pull(total_expo)

iic_mar_esp <- (xijk_esp / xik) / (xij_esp / xi)
iic_mar_esp <- as.numeric(iic_mar_esp)
iic_mar_esp

iic_mar_fra <- (xijk_fra / xik) / (xij_fra / xi)
iic_mar_fra <- as.numeric(iic_mar_fra)
iic_mar_fra


# EN FORMA DE TABLA: 

# PRIMERO ESPAÑA

sectores_top5 <- top5_ventaja$cuci

mar_esp <- comtrade_exp |>
  filter(r == "MAR", p == socio_1, year == anio, cuci %in% sectores_top5) |>
  select(cuci, cuci_desc, xijk = value, xij = total_expo)

mar_wld <- comtrade_exp |>
  filter(r == "MAR", p == "WLD", year == anio, cuci %in% sectores_top5) |>
  select(cuci, xik = value, xi = total_expo)

iic_top5 <- mar_esp |>
  left_join(mar_wld, by = "cuci") |>
  mutate(iic = as.numeric((xijk / xik) / (xij / xi))) |>
  select(cuci, cuci_desc, iic) |>
  arrange(desc(iic))

# AHORA FRANCIA

# mar_fra <- comtrade_exp |>
#   filter(r == "MAR", p == socio_1, year == anio, cuci %in% sectores_top5) |>
#   select(cuci, cuci_desc, xijk = value, xij = total_expo)
# 
# mar_wld <- comtrade_exp |>
#   filter(r == "MAR", p == "WLD", year == anio, cuci %in% sectores_top5) |>
#   select(cuci, xik = value, xi = total_expo)
# 
# iic_top5 <- mar_fra |>
#   left_join(mar_wld, by = "cuci") |>
#   mutate(iic = as.numeric((xijk / xik) / (xij / xi))) |>
#   select(cuci, cuci_desc, iic) |>
#   arrange(desc(iic))

#-----------------------------------------------------------------------------#
# Siguiente indicador: ICC (Compatibilidad/Complementariedad Comercial)
#-----------------------------------------------------------------------------#

#ICC AGREGADO

calcular_icc <- function(socio, anio) {
  
  # m_k^i: participación del bien k en las IMPORTACIONES totales de Marruecos
  m_mar <- comtrade |>
    filter(r == "MAR", p == "WLD", flow == "Import", year == anio) |>
    mutate(m = value / sum(value, na.rm = TRUE)) |>
    select(cuci, cuci_desc, m)
  
  # x_k^j: participación del bien k en las EXPORTACIONES totales del socio al mundo
  x_socio <- comtrade |>
    filter(r == socio, p == "WLD", flow == "Export", year == anio) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, x)
  
  # Full join: un producto ausente de un lado cuenta como share = 0, no se descarta
  tabla <- full_join(m_mar, x_socio, by = "cuci") |>
    mutate(
      m = replace_na(m, 0),
      x = replace_na(x, 0),
      dif_abs = abs(m - x)
    )
  
  icc <- 100 * (1 - sum(tabla$dif_abs, na.rm = TRUE) / 2)
  
  list(icc = icc, detalle = tabla)
}

# Uso:
icc_esp_mar <- calcular_icc("ESP", 2024)
icc_esp_mar$icc

# 1) La suma de shares de cada lado debería dar 1 (o muy cerca, por redondeo)
sum(icc_esp_mar$detalle$m, na.rm = TRUE)
sum(icc_esp_mar$detalle$x, na.rm = TRUE)


# 2) El ICC teóricamente está acotado entre 0 y 100 — confirmá que no se te fue de rango
icc_esp_mar$icc

resultado_fra <- calcular_icc("FRA", 2024)
resultado_fra$icc

icc_evolucion <- map_dfr(2021:2025, function(a) {
  tibble(
    year = a,
    icc_esp = calcular_icc("ESP", a)$icc,
    icc_fra = calcular_icc("FRA", a)$icc
  )
})

# ICC POR PRODUCTO

calcular_icc_producto <- function(socio, anio) {
  
  m_mar <- comtrade |>
    filter(r == "MAR", p == "WLD", flow == "Import", year == anio) |>
    mutate(m = value / sum(value, na.rm = TRUE)) |>
    select(cuci, cuci_desc, m)
  
  x_socio <- comtrade |>
    filter(r == socio, p == "WLD", flow == "Export", year == anio) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, cuci_desc_x = cuci_desc, x)  # guardamos la descripción también acá
  
  full_join(m_mar, x_socio, by = "cuci") |>
    mutate(
      cuci_desc = coalesce(cuci_desc, cuci_desc_x),  # si falta de un lado, usa el otro
      m = replace_na(m, 0),
      x = replace_na(x, 0),
      icc_k = 100 * (1 - abs(m - x) / 2)
    ) |>
    select(cuci, cuci_desc, m, x, icc_k) |>
    arrange(desc(icc_k))
}

icc_prod_esp <- calcular_icc_producto("ESP", 2024)
view(icc_prod_esp)
icc_prod_fra <- calcular_icc_producto("FRA", 2024)
view(icc_prod_fra)


# Top 10 de mejor matching con cada socio
icc_prod_esp |> slice_max(icc_k, n = 10) |> select(cuci, cuci_desc, icc_k)
icc_prod_fra |> slice_max(icc_k, n = 10) |> select(cuci, cuci_desc, icc_k)


icc_prod_esp |> 
  filter(m > 0.001, x > 0.001) |>
  arrange(desc(icc_k)) |>
  head(10)

icc_prod_fra |> 
  filter(m > 0.001, x > 0.001) |>
  arrange(desc(icc_k)) |>
  head(10)


#------------------------------------------------------------------------#
# GRAFICOS

# evoluciónm agregada del icc

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

# evolución del iic para los mismos 5 sectores top con España

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

iic_evolucion <- map_dfr(2021:2025, ~calcular_iic_multi("ESP", sectores_top5, .x))

ggplot(iic_evolucion, aes(x = year, y = iic, color = cuci_desc)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  labs(title = "Evolución del IIC — Marruecos hacia España",
       subtitle = "Top 5 sectores con mayor VCR de Marruecos, 2021-2025",
       x = "Año", y = "IIC", color = "Producto") +
  theme_minimal()


# cruce VCRN con IIC
# Armamos la base combinando VCR 2024 con IIC 2024 hacia España, para todos los productos en común
cruce_vcr_iic <- vcr_mar |>
  filter(year == 2024) |>
  select(cuci, cuci_desc, vcrn) |>
  inner_join(
    calcular_iic_multi("ESP", unique(vcr_mar$cuci), 2024) |> select(cuci, iic),
    by = "cuci"
  ) |>
  filter(!is.na(iic), is.finite(iic))

ggplot(cruce_vcr_iic, aes(x = vcrn, y = iic)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_log10() +  # el IIC suele tener rango muy amplio, el log ayuda a leerlo
  labs(title = "Ventaja comparativa global vs. intensidad bilateral con España",
       subtitle = "Marruecos, 2024 — cada punto es un producto (CUCI 3 dígitos)",
       x = "VCRN (ventaja comparativa revelada normalizada)",
       y = "IIC (escala log) — línea punteada en IIC = 1") +
  theme_minimal()

# solo puntos extremos

destacados <- cruce_vcr_iic |>
  filter(cuci %in% c("562", "272", "842", "773", "522"))

ggplot(cruce_vcr_iic, aes(x = vcrn, y = iic)) +
  geom_point(alpha = 0.3, color = "gray60") +
  geom_point(data = destacados, color = "firebrick", size = 3) +
  geom_text_repel(data = destacados, aes(label = cuci_desc), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  scale_y_log10() +
  labs(title = "Ventaja comparativa global vs. intensidad bilateral con España",
       subtitle = "Marruecos, 2024",
       x = "VCRN", y = "IIC (escala log)") +
  theme_minimal()


# mismo grafico, intento 2

cruce_vcr_iic_filtrado <- cruce_vcr_iic |>
  filter(iic > 0, is.finite(iic))

#--------------------------------------------------------------#

ggplot(cruce_vcr_iic_filtrado, aes(x = vcrn, y = iic)) +
  # Sombreado de cuadrantes para que se lean de un vistazo
  annotate("rect", xmin = 0, xmax = Inf, ymin = 1, ymax = Inf,
           fill = "steelblue", alpha = 0.06) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 1,
           fill = "firebrick", alpha = 0.06) +
  geom_point(alpha = 0.35, color = "gray40", size = 2) +
  geom_point(data = destacados, aes(color = cuci_desc), size = 4) +
  geom_text_repel(
    data = destacados, aes(label = cuci_desc, color = cuci_desc),
    size = 3.5, fontface = "bold", show.legend = FALSE,
    box.padding = 0.6, point.padding = 0.3,
    segment.color = "gray50", min.segment.length = 0
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Ventaja comparativa global vs. intensidad bilateral con España",
    subtitle = "Marruecos, 2024 · cada punto gris es un producto (CUCI 3 dígitos)",
    x = "VCRN — ventaja comparativa revelada normalizada",
    y = "IIC (escala log)",
    caption = "Línea punteada horizontal: IIC = 1 (comercio ni más ni menos intenso de lo esperado)\nLínea punteada vertical: VCRN = 0 (frontera entre ventaja y desventaja comparativa)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray30", size = 11),
    plot.caption = element_text(color = "gray50", size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92")
  )



ultimo_grafico <- last_plot()  # toma el último gráfico ggplot que corriste

ggsave(
  filename = "vcr_vs_iic_espana.png",
  plot = ultimo_grafico,
  width = 10, height = 6.5,
  dpi = 300,           # buena resolución para proyectar/imprimir
  bg = "white"          # evita que quede con fondo transparente
)





