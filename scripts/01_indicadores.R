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
  rename(share_mar = share)

#AHORA TODA LA PARTE DE WLD-WLD

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

vcr_mar <- vcr_aux_mar |>
  select(year, cuci, cuci_desc, share_mar) |>
  left_join(wld_exp, by = c("year", "cuci")) |>
  mutate(
    vcr  = share_mar / share_wld,
    vcrn = (vcr - 1) / (vcr + 1)
  ) |>
  arrange(desc(vcr))

sum(is.na(vcr_mar$share_wld))

view(vcr_mar)


