#===============================================================================#
# TP5 - Economias de escala - EJERCICIO 2 (a)-(b): INDICE DE GRUBEL-LLOYD
# ARGENTINA CON BRASIL Y CHINA, 2024 (SA/HS 2022, capitulos a 2 digitos)
#===============================================================================#
# OBJETIVO
#   Para cada capitulo i y cada socio (Brasil, China): GL_i = 1 - |X_i - M_i| / (X_i + M_i),
#   el GL agregado del comercio bilateral, la tabla de los 10 capitulos de mayor
#   comercio total (X + M) y una seccion de robustez.
#
# BASES (carpeta "bases de datos/", WITS, HS 2022, miles de USD)
#   - ARG_BRA_CHN_comercio_HS2022_cap_2024.dta : Argentina (reportero) con socios
#     Brasil y China, por capitulo (2 digitos), flujo y ano 2024.
#
# CONVENCIONES PROPIAS DE ESTE SCRIPT (no vienen del enunciado del TP):
#   - Capitulo 99 ("mercaderia no clasificada") no es un capitulo del SA: se excluye.
#   - Corte GL_i >= 0.5 para "predominantemente intraindustrial" (se prueban 0.33 y 0.66).
#
# SALIDAS
#   output/tablas/3.1/  : GL_por_capitulo.csv, GL_agregado.csv, tabla_top10.csv, robustez.csv
#   output/graficos/3.1/: (reservada para los graficos de este ejercicio)
#===============================================================================#

for (p in c("haven", "dplyr", "tidyr", "readr")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}
library(haven)
library(dplyr)
library(tidyr)
library(readr)

# Directorio de trabajo = carpeta del proyecto (la que contiene "bases de datos/").
# Ej.: "C:/Users/cece.DOMINIO/Desktop/eco_inter_TPs_2c2026"
if (!dir.exists("bases de datos")) stop("No encuentro la carpeta 'bases de datos/' en el directorio de trabajo (", getwd(),
                                        "). Abri el proyecto o usa setwd() hacia su carpeta.")
ruta_datos    <- "bases de datos/"
ruta_graficos <- "output/graficos/3.1/"
ruta_tablas   <- "output/tablas/3.1/"
dir.create(ruta_graficos, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_tablas,   recursive = TRUE, showWarnings = FALSE)

nombre_base <- "ARG_BRA_CHN_comercio_HS2022_cap_2024.dta"
if (!file.exists(paste0(ruta_datos, nombre_base))) {
  cat("Buscado:", paste0(ruta_datos, nombre_base), "| getwd():", getwd(), "\nContenido de la carpeta:\n")
  print(list.files(ruta_datos))
  stop("No encuentro la base. Comparar el nombre buscado con el contenido listado arriba.")
}

# ---- 1. Lectura y chequeos ----------------------------------------------------
raw <- read_dta(paste0(ruta_datos, nombre_base)) %>% mutate(across(where(is.labelled), as_factor)) %>%
  mutate(across(c(Nomenclature, ReporterISO3, ProductCode, PartnerISO3,
                  TradeFlowName, ProductDescription), as.character))

stopifnot(nrow(raw) != 100000)                       # limite de filas de WITS
stopifnot(all(raw$Year == 2024), all(raw$ReporterISO3 == "ARG"))
cat("Filas:", nrow(raw), "| nomenclatura:", unique(raw$Nomenclature), "\n")
stopifnot(!any(duplicated(raw[c("PartnerISO3", "TradeFlowName", "ProductCode")])))

print(raw %>% group_by(PartnerISO3, TradeFlowName) %>%
        summarise(capitulos = n_distinct(ProductCode),
                  total_miles_USD = sum(TradeValuein1000USD), .groups = "drop"))
cat("COMPARAR estos totales con los de WITS/INDEC antes de seguir.\n\n")

# ---- 2. Preparacion --------------------------------------------------------------
# Capitulo 99 = "mercaderia no clasificada": no es un capitulo del SA; se excluye de
# los indices (declararlo en el TP). Las combinaciones ausentes = comercio cero.
desc <- raw %>% distinct(ProductCode, ProductDescription)
gl <- raw %>%
  filter(ProductCode != "99") %>%
  select(PartnerISO3, ProductCode, TradeFlowName, TradeValuein1000USD) %>%
  pivot_wider(names_from = TradeFlowName, values_from = TradeValuein1000USD) %>%
  complete(PartnerISO3, ProductCode, fill = list(Export = 0, Import = 0)) %>%
  mutate(Export = coalesce(Export, 0), Import = coalesce(Import, 0),
         comercio = Export + Import,
         GL_i = ifelse(comercio > 0, 1 - abs(Export - Import) / comercio, NA_real_),
         tipo = case_when(is.na(GL_i) ~ NA_character_,
                          GL_i >= 0.5 ~ "predom. intraindustrial",
                          TRUE        ~ "predom. interindustrial")) %>%
  left_join(desc, by = "ProductCode")

# ---- 3. GL agregado -----------------------------------------------------------------
GL_agr <- gl %>% group_by(PartnerISO3) %>%
  summarise(X_miles_USD = sum(Export), M_miles_USD = sum(Import),
            GL_agregado = sum(comercio - abs(Export - Import)) / sum(comercio),
            peso_comercio_GL_mayor_0.5 = sum(comercio[GL_i >= 0.5], na.rm = TRUE) / sum(comercio),
            .groups = "drop")
print(GL_agr)

# ---- 4. Top 10 por comercio total ----------------------------------------------------
tot <- gl %>% group_by(PartnerISO3) %>% summarise(tot = sum(comercio), .groups = "drop")
top10 <- gl %>% group_by(PartnerISO3) %>%
  slice_max(comercio, n = 10, with_ties = FALSE) %>% ungroup() %>%
  left_join(tot, by = "PartnerISO3") %>%
  arrange(PartnerISO3, desc(comercio)) %>%
  transmute(Socio = PartnerISO3, Capitulo = ProductCode, Descripcion = ProductDescription,
            `X (MUSD)` = round(Export / 1e3, 1), `M (MUSD)` = round(Import / 1e3, 1),
            `X+M (MUSD)` = round(comercio / 1e3, 1),
            `% comercio bilateral` = round(100 * comercio / tot, 1),
            GL_i = round(GL_i, 3), tipo)
print(top10, n = 20, width = Inf)


# ---- 5. Robustez (para declarar en el TP) ----------------------------------------------
# (i) GL agregado ajustado por desequilibrio bilateral (Aquino): el GL agregado "crudo"
#     se subestima cuando hay saldo bilateral; el ajuste lo corrige.
# (ii) media simple de GL_i (cada capitulo pesa igual) vs. ponderada (= GL agregado).
# (iii) sensibilidad al corte que separa intra de interindustrial (0.33 / 0.5 / 0.66).
# (iv) GL agregado sin el capitulo 87 (concentracion en autos).
rob <- gl %>% group_by(PartnerISO3) %>%
  summarise(
    saldo_X_menos_M_pct_comercio = 100 * (sum(Export) - sum(Import)) / sum(comercio),
    GL_agregado        = sum(comercio - abs(Export - Import)) / sum(comercio),
    GL_Aquino          = sum(comercio - abs(Export - Import)) /
      (sum(comercio) - abs(sum(Export) - sum(Import))),
    GL_media_simple    = mean(GL_i, na.rm = TRUE),
    pct_comercio_corte_0.33 = 100 * sum(comercio[GL_i >= 0.33], na.rm = TRUE) / sum(comercio),
    pct_comercio_corte_0.50 = 100 * sum(comercio[GL_i >= 0.50], na.rm = TRUE) / sum(comercio),
    pct_comercio_corte_0.66 = 100 * sum(comercio[GL_i >= 0.66], na.rm = TRUE) / sum(comercio),
    .groups = "drop")
sin87 <- gl %>% filter(ProductCode != "87") %>% group_by(PartnerISO3) %>%
  summarise(GL_agregado_sin_cap87 = sum(comercio - abs(Export - Import)) / sum(comercio),
            .groups = "drop")
rob <- left_join(rob, sin87, by = "PartnerISO3")
print(as.data.frame(rob))
write_csv(rob, paste0(ruta_tablas, "robustez.csv"))

write_csv(gl,     paste0(ruta_tablas, "GL_por_capitulo.csv"))
write_csv(GL_agr, paste0(ruta_tablas, "GL_agregado.csv"))
write_csv(top10,  paste0(ruta_tablas, "tabla_top10.csv"))
cat("\nListo. Tablas en:", ruta_tablas, "\n")
