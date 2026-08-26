install.packages("tidyverse")
install.packages("haven")

library(tidyverse)
library(haven)
library(dplyr)

getwd()
setwd("/cloud/project/bases de datos")
comtrade <- read_dta("MAR_SPN_FRA_WLD.dta")

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
view(vcr_mar)

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

view(top5_ventaja)

# Top 5 sectores con mayor desventaja comparativa (VCRN más bajo)
top5_desventaja <- vcr_mar |>
  filter(year == 2024) |>
  slice_min(vcrn, n = 5)

view(top5_desventaja)


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



