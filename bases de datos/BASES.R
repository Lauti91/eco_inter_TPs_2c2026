
#-------------------------------------------------
#PRIMERO CHEQUEO QUÉ TIENEN MIS BASES "ORIGINALES"
#-------------------------------------------------


archivos <- c(
  "MAR_ESP_FRA_WLD_full.dta",
  "MAR_P_WLD.dta",
  "MAR_SPN_FRA_WLD.dta",
  "WLD_WLD.dta"
)

for (archivo in archivos) {
  cat("\n===========================\n")
  cat("Archivo:", archivo, "\n")
  cat("===========================\n")
  
  base <- read_dta(archivo)
  names(base) <- tolower(names(base))
  
  base <- base %>%
    mutate(
      r = as.character(as_factor(reporteriso3)),
      p = as.character(as_factor(partneriso3))
    )
  
  cat("Reporters:\n")
  print(distinct(base, r))
  
  cat("\nPartners (primeros 20, puede haber muchos):\n")
  print(distinct(base, p) |> head(20))
  
  cat("\nCantidad de partners únicos:", nrow(distinct(base, p)), "\n")
  cat("Años disponibles:", paste(sort(unique(as_factor(base$year))), collapse = ", "), "\n")
  cat("Filas totales:", nrow(base), "\n")
}


#-------------------------------------------------
#AHORA USO LAS QUE ARMÉ DE FORMA "SIMPLIFICADA"
#-------------------------------------------------

#Carga y limpieza de bases (versión consolidada)

# 3 bases: MAR_ALLPARTNERS_WLD, SOCIOS_WLD, WLD_WLD

library(tidyverse)
library(haven)
library(ggrepel)

setwd("/cloud/project/bases de datos")

# Función de limpieza reutilizable: mismo pipeline para las 3 bases,
# así evitamos repetir el mismo bloque de mutate/rename/select tres veces.
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
# 1) Marruecos como reporter, 77 países + World como partners
#    Sirve para: VCR (numerador), ranking de socios, destino de productos
#    específicos, sectores a 2 dígitos, evolución del comercio total.
#===============================================================================#

comtrade_mar <- limpiar_wits("MAR_ALLPARTNERS_WLD.dta")

# Chequeo rápido antes de seguir
distinct(comtrade_mar, r)
n_distinct(comtrade_mar$p)
range(comtrade_mar$year)

#===============================================================================#
# 2) España, Francia, Alemania, Brasil e India como reporters, World y
#    77 países como partners.
#    Sirve para: ICC (denominador, usando p == "WLD"), IIC de cada socio.
#===============================================================================#

comtrade_socios <- limpiar_wits("SOCIOS_WLD.dta")

distinct(comtrade_socios, r)
range(comtrade_socios$year)

#===============================================================================#
# 3) World como reporter y partner (agregado mundial)
#    Sirve para: VCR (denominador)
#===============================================================================#

comtrade_wld <- limpiar_wits("WLD_WLD.dta", incluir_desc = FALSE)

# Ojo: acá el código de "World" suele venir como "All", no "WLD" - confirmar:
distinct(comtrade_wld, r, p)

#===============================================================================#
# 4) Bases de trabajo derivadas (shares y totales, listas para usar)
#===============================================================================#

# Exportaciones de Marruecos, con share sobre el total de cada partner
mar_exp <- comtrade_mar |>
  filter(flow == "Export") |>
  group_by(year, r, p) |>
  mutate(total_expo = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  mutate(share = value / total_expo)

# Importaciones de Marruecos, con share (necesario para el ICC)
mar_imp <- comtrade_mar |>
  filter(flow == "Import") |>
  group_by(year, r, p) |>
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

cat("Bases cargadas y listas: comtrade_mar, comtrade_socios, comtrade_wld,",
    "mar_exp, mar_imp, wld_exp\n")

#-----------------------------------------
# RANKING ICC
#-----------------------------------------

candidatos <- c("ESP", "FRA", "DEU", "BRA", "IND", "CHN", "ARE", "GBR", "USA")

ranking_icc_completo <- map_dfr(candidatos, function(s) {
  x_socio <- comtrade_socios |>
    filter(r == s, p == "WLD", flow == "Export", year == 2024) |>
    mutate(x = value / sum(value, na.rm = TRUE)) |>
    select(cuci, x)
  
  m_mar <- mar_imp |>
    filter(p == "WLD", year == 2024) |>
    select(cuci, m = share)
  
  tabla <- full_join(m_mar, x_socio, by = "cuci") |>
    mutate(m = replace_na(m, 0), x = replace_na(x, 0), dif_abs = abs(m - x))
  
  tibble(socio = s, icc = 100 * (1 - sum(tabla$dif_abs, na.rm = TRUE) / 2))
})

ranking_icc_completo |> arrange(desc(icc))

