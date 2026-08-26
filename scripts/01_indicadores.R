install.packages("tidyverse")
install.packages("haven")

library(tidyverse)
library(haven)
library(dplyr)

getwd()
setwd("/cloud/project/bases de datos")
comtrade <- read_dta("MAR_SPN_FRA_WLD.dta")

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

view(comtrade)

comtrade_2 <- comtrade |>
  mutate(value = value / 1000)

#VCR 

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
  rename(share_mys = share)

#NOTA: VER CÓMO SEGUIR SOLO PARA MARRUECOS



