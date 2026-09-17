if (!requireNamespace("rsdmx", quietly = TRUE)) install.packages("rsdmx", repos = "https://cloud.r-project.org")
library(rsdmx)

url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NASU@DF_USEVA_T1600,1.0/A.MAR..?format=genericdata"
resultado <- readSDMX(url)
as.data.frame(resultado)





library(readr)
library(dplyr)

# Intento 1: con versión fijada
url_useva <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NASU@DF_USEVA_T1600,1.0/all?format=csvfilewithlabels"
datos_useva <- read_csv(url_useva)

# Si el intento 1 da error 404 (versión incorrecta), probar sin fijar versión:
# url_useva <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NASU@DF_USEVA_T1600/all?format=csvfilewithlabels"

names(datos_useva)  # confirmar cómo se llama la columna de país (REF_AREA, ref_area, Reference area...)

# Ver si aparece Marruecos, por código o por nombre
datos_useva |> filter(REF_AREA == "MAR" | grepl("Morocco", `Reference area`, ignore.case = TRUE)) |> 
  distinct(REF_AREA, `Reference area`)

datos_useva |> 
  filter(REF_AREA == "MAR") |> 
  distinct(TRANSACTION, `Transaction`) 

datos_useva |> 
  filter(REF_AREA == "MAR") |> 
  distinct(ACTIVITY, `Economic activity`)

datos_useva |> 
  filter(REF_AREA == "MAR") |> 
  distinct(TIME_PERIOD) |> 
  arrange(TIME_PERIOD)


datos_useva |> 
  filter(REF_AREA == "MAR") |> 
  distinct(ACTIVITY, `Economic activity`) |> 
  filter(grepl("mining|quarrying|extract", `Economic activity`, ignore.case = TRUE) | grepl("^B", ACTIVITY))

datos_useva |> 
  filter(REF_AREA == "MAR") |> 
  distinct(ACTIVITY, `Economic activity`) |> 
  filter(grepl("motor vehicle|automotive|transport equipment", `Economic activity`, ignore.case = TRUE))



alpha_oecd <- datos_useva |>
  filter(REF_AREA == "MAR",
         ACTIVITY %in% c("B08", "B", "C29"),
         TRANSACTION %in% c("D1", "B1G", "B2A3G")) |>
  select(ACTIVITY, `Economic activity`, TRANSACTION, TIME_PERIOD, OBS_VALUE) |>
  pivot_wider(names_from = TRANSACTION, values_from = OBS_VALUE) |>
  mutate(
    alpha_capital = B2A3G / B1G,
    share_trabajo = D1 / B1G
  ) |>
  arrange(ACTIVITY, TIME_PERIOD)

alpha_oecd
print(alpha_oecd, n = 24)


###########################
#REGRESIÓN
###########################


library(broom)

alpha_b08 <- alpha_oecd |> filter(ACTIVITY == "B08")

# Tendencia con todos los años
modelo_completo <- lm(alpha_capital ~ TIME_PERIOD, data = alpha_b08)
tidy(modelo_completo)
glance(modelo_completo)  # fijate el r.squared y el p.value

# Robustez: la misma tendencia, pero sacando 2020 (el año COVID)
modelo_sin_covid <- lm(alpha_capital ~ TIME_PERIOD, data = filter(alpha_b08, TIME_PERIOD != 2020))
tidy(modelo_sin_covid)
glance(modelo_sin_covid)

# Comparación (placebo): ¿automotor muestra la misma caída, o es específico de fosfatos?
modelo_auto <- lm(alpha_capital ~ TIME_PERIOD, data = filter(alpha_oecd, ACTIVITY == "C29"))
tidy(modelo_auto)


# Proyectar el alpha de cada sector al año de referencia de la FPP (2023),
# usando la tendencia estimada en vez de la foto fija de HCP
alpha_fosfatos_2023 <- predict(modelo_completo, newdata = tibble(TIME_PERIOD = 2023))
alpha_auto_2023 <- predict(modelo_auto, newdata = tibble(TIME_PERIOD = 2023))

c(fosfatos = alpha_fosfatos_2023, automotor = alpha_auto_2023)
