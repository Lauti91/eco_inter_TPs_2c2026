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
