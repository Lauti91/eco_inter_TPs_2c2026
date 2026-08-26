
###   Curso Economia Internacional 
###   Prof. Maria Priscila Ramos
###   Ayudantes: Pablo F. Bertin - Juan Amaya

#----------------------------------------------------- Introducción al uso de R ------------------------------------------------

#R se usa mucho para análisis de datos. Pero no está restringido únicamente a ese uso.


# Funcionamiento básico ---------------------------------------------------

#Como todo lenguaje de programación popular, R trabaja con inputs ("insumos") y outputs ("resultados/salidas")
#Con shift + enter, o directamente seleccionando la línea de código a correr y presionando shift + enter o el botón run, corremos la parte del código que queramos.

#Podemos hacer varias operaciones matemáticas sencillas: suma, resta, división, multiplicación, potenciación...

2+2
2+(3*2)
4/3

4^2
sin(pi / 2)

#Para crear objetos/variables, debe usarse el siguiente formato:
X <- 3

#Podemos verlo en el "entorno" a la derecha. También podemos pedirle a R que nos muestre qué contiene el objeto.
X

#R es MUY estricto con los nombres de las variables. Por ejemplo:
variable <- pi

Variable #No hallado

VARIABLE #Tampoco

#También, por supuesto, pueden crearse variables que no sean números.
Y <- "Economía"
Y

#Podemos hacer comparaciones entre números o variables, de este modo:
2 == 3
X == Y

Y <- c(1, 2, 3) #c permite concatenar números: es decir, hago que mean me tome TODA la secuencia de números dentro de c(), en lugar de solo el 1.
X == Y
2 != 3

# Funciones ---------------------------------------------------------------

#R tiene un montón de funciones escritas dentro de sí. Cada función tiene, por supuesto, su propósito específico. Por ejemplo:
seq(1, 10) #seq() es una función que escribe todos los números entre los 2 ARGUMENTOS que le otorgas.

#En este caso, seq() pide 2 argumentos: el límite inferior, y el límite superior.
sum(1, 2, 3)
sum(1:6)

min(1, 2, 3)
max(1, 2, 3)

round(1.5)

mean(c(1, 3, 5, 7, 9))
median(c(1, 3, 5, 7, 9))


# Transformación de datos -------------------------------------------------

# Por supuesto, los datos no siempre nos vienen perfectamente preparados. A veces, hay que modificar cosas por cuenta propia.
# Para ello, haremos uso de una base de comercio internacional descargada de WITS/COMTRADE, y de la librería tidyverse.
# tidyverse es probablemente la librería más utilizada, al contener dentro de sí paquetes clave, como dplyr y ggplot.
# También usaremos haven, que nos permite leer bases en formato .dta (Stata), que es el formato en el que suele venir la base de WITS.

install.packages("tidyverse")
install.packages("haven")

library(tidyverse)
library(haven)
library(dplyr)

# Leemos la base descargada de WITS/COMTRADE (formato .dta).
# NOTA: reemplazar "base_comtrade.dta" por el nombre/ruta real del archivo que vayamos a usar.
comtrade <- read_dta("base_comtrade.dta")

setwd("C:/Users/pablo/OneDrive/Escritorio/PNY ELITE PSSD/Clases/UBA_Econ/EcoInter/EI VIRTUAL/clase datos")
comtrade <- read_dta("base_comtrade.dta")

# Pasamos los nombres de las variables a minúscula, para trabajar más cómodos.
names(comtrade) <- tolower(names(comtrade))

# Dado el formato en el que vienen algunas variables al descargar desde WITS (los valores quedan codificados como factores),
# tenemos que acomodarlas en R para poder visualizar las etiquetas (nombres de países, productos, etc.) que nos interesan.
comtrade <- comtrade %>%
  mutate(
    cuci      = as.character(as_factor(productcode)),
    cuci_desc = as.character(as_factor(productdescription)),
    r         = as.character(as_factor(reporteriso3)),
    p         = as.character(as_factor(partneriso3)),
    flow      = as.character(as_factor(tradeflowname))
  ) %>%
  rename(value = tradevaluein1000usd) %>% #El valor comerciado viene expresado en miles de USD.
  select(r, p, flow, cuci, cuci_desc, value, year)

comtrade
View(comtrade) #Una forma mucho mejor de observar la dataset.

#tidyverse contiene, como mencioné, a dplyr. La librería que más usaremos en el curso (y probablemente el resto de sus vidas).
comtrade |>
  filter(flow == "Export")

comtrade |>
  count(year, p)

table(comtrade$year, comtrade$p)
#Arriba hicimos 2 cosas: primero, usamos "|>", que le dice a R que vamos a modificar la variable de la izquierda (el dataset).
#Abajo, usamos la función filter; que, obviamente, filtra los datos en base a LA o las condiciones que usamos.
#En este caso, le pedimos mostrar SOLO los flujos que sean exportaciones.

#También podemos usar %>% en vez de |>. No hay diferencia.

comtrade |>
  filter(flow == "Export" & year == 2019) #Le pido filtrar por los flujos que sean exportaciones...Y que hayan sido en 2019.

#El "Y" lo hice con &.
comtrade |>
  filter(r == "ARG" | p == "WLD") #Le pido filtrar por los datos en los que el reportante haya sido Argentina...O Malasia.

#El "O" lo hacemos con |.
#Por supuesto, podemos pedirle a R más de 2 condiciones.
comtrade |>
  filter(flow == "Export" & year == 2019 & value < 0) #con > hacemos mayor que.

#El comando arrange() me permite cambiar el ORDEN de las columnas y/o filas.
orden <- comtrade %>% 
  arrange(desc(value)) #Estamos ordenando los datos en función de los flujos con MAYOR valor comerciado.

#Con select(), le pedimos a R seleccionar solo una o varias columnas de todas las existentes.
select <- comtrade |>
  select(year, r, p, cuci, value)

#Con rename(), cambiamos el nombre de las variables.
comtrade |>
  rename(año = year) #A la izquierda, el nuevo nombre de la variable.

#Veamos el comando más importante de la librería: mutate().
#Mutate nos sirve para añadir o modificar columnas. Casi todo lo que es transformar/modificar pasa por esta función.

comtrade_2 <- comtrade |>
  mutate(value = value / 1000) #Creo una variable que expresa el valor comerciado en millones de dólares (la base original viene en miles de USD).

comtrade_3 <- comtrade |>
  mutate(high_value = ifelse(value > 1000, 1, 0)) #ifelse es una función importante: su primer argumento te pide una condición.

#En este caso, la condición es que el valor comerciado sea mayor a 10.000 (miles de USD). Su segundo argumento te pide el valor que debe tomar esta variable SÍ SE CUMPLE esa condición.
#Su último argumento te pide el valor que debe tomar sí NO se cumple.
#Quedamos, entonces, con una variable que vale 1 si el valor comerciado de ese flujo es mayor a 10.000 (miles de USD).

#Por último, summarise() puede ser usado para testear propiedades estadísticas de la muestra.
comtrade_2 |>
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    count = n())

#También podemos usar summarise() para obtener estadística descriptiva más completa de la base de comercio.

# Hago unas tablas para ver que tienen estas variables
table(comtrade_2$flow)
table(comtrade_2$p)

#table(comtrade_2$cuci)
comtrade_2 |>
  count(year, cuci)

comtrade_2 |>
  summarise(
    valor_total   = sum(value, na.rm = TRUE),
    valor_prom    = mean(value, na.rm = TRUE),
    valor_mediana = median(value, na.rm = TRUE),
    valor_min     = min(value, na.rm = TRUE),
    valor_max     = max(value, na.rm = TRUE),
    valor_sd      = sd(value, na.rm = TRUE),
    n_productos   = n_distinct(cuci),
    n_flujos      = n())

#Podemos, además, agrupar la estadística descriptiva por año, o por país reportante, por ejemplo.
comtrade_2 |>
  group_by(year) |>
  summarise(
    valor_total = sum(value, na.rm = TRUE),
    valor_prom  = mean(value, na.rm = TRUE),
    n_productos = n_distinct(cuci),
    .groups = "drop")

comtrade |>
  group_by(r, flow) |>
  summarise(
    valor_total = sum(value, na.rm = TRUE),
    n_productos = n_distinct(cuci),
    .groups = "drop")


# Visualización de datos --------------------------------------------------

#Para terminar la breve introducción, veamos un poco de visualización de datos.
#Para graficar, usaremos principalmente ggplot; paquete que viene incluido con tidyverse.

comtrade_g <- comtrade_2 |>
  filter(flow == "Export") 
  
options(scipen = 999)

ggplot(data = comtrade_g)+
  geom_col(aes(x = year, y = value))+
  labs(title = "Valor exportado por año",
       x = "Año",
       y = "Millones de USD")

table(comtrade_g$r)

ggplot(data = comtrade_g)+
  geom_col(aes(x = year, y = value))+
  facet_wrap(~ r)+
  labs(title = "Valor exportado por año",
       x = "Año",
       y = "Millones de USD")

ggplot(data = comtrade_g)+
  geom_col(aes(x = year, y = value))+
  facet_wrap(~ r, scales = "free_y")+
  labs(title = "Valor exportado por año",
       x = "Año",
       y = "Millones de USD")

ggsave("C:/Users/pablo/OneDrive/Escritorio/PNY ELITE PSSD/Clases/UBA_Econ/EcoInter/EI VIRTUAL/clase datos/valor_exportado.png",
       width = 8, height = 5, dpi = 300)

# Aplicación: Índice de Ventaja Comparativa Revelada (VCR) -----------------

#A modo de aplicación de todo lo anterior, construyamos el Índice de Ventaja Comparativa Revelada Normalizado (VCR/RCA)
#usando la misma base de comercio que ya cargamos y ordenamos (comtrade).
#La idea del índice es comparar la participación de un producto en las exportaciones totales de un país (Argentina, en este caso)
#contra la participación de ese mismo producto en las exportaciones totales de un país/referencia de comparación (Malasia, en este caso).

# Nos quedamos solo con exportaciones.
comtrade_exp <- comtrade_2 |>
  filter(flow == "Export")

# Total exportado por año, país reportante y socio.
comtrade_exp <- comtrade_exp |>
  group_by(year, r, p) |>
  mutate(total_expo = sum(value, na.rm = TRUE)) |>
  ungroup()

# Participación de cada producto (cuci) en el total exportado.
comtrade_exp <- comtrade_exp |>
  mutate(share = value / total_expo)

df_share <- comtrade_exp |>
  select(year, r, p, cuci, cuci_desc, share)

# Base auxiliar: participación de Malasia (país de referencia) hacia el mundo.
vcr_aux_mys <- df_share |>
  filter(r == "MYS", p == "WLD") |>
  rename(share_mys = share)

# Participación de Argentina hacia el mundo.
df_arg <- df_share |>
  filter(r == "ARG", p == "WLD") |>
  rename(share_arg = share)

# Unimos ambas participaciones por año y producto.
df_vcr <- df_arg |>
  left_join(
    vcr_aux_mys |> select(year, cuci, cuci_desc, share_mys),
    by = c("year", "cuci", "cuci_desc")
  )

# Construcción del índice VCR y su versión normalizada (VCRN), que va de -1 a 1.
df_vcr <- df_vcr |>
  mutate(
    vcr  = share_arg / share_mys,
    vcrn = (vcr - 1) / (vcr + 1)
  ) |>
  select(cuci, cuci_desc, year, vcr, vcrn) |>
  arrange(cuci, year)

df_vcr
View(df_vcr)

#Un vcrn > 0 indica que el producto muestra ventaja comparativa revelada de Argentina respecto de Malasia; un vcrn < 0, lo contrario.

# Top 5 productos con ventaja para Argentina (vcrn más alto)
top5_arg <- df_vcr |>
  filter(year == 2019) |>
  slice_max(vcrn, n = 5)

# Top 5 productos con ventaja para Malasia (vcrn más bajo/negativo)
top5_mys <- df_vcr |>
  filter(year == 2019) |>
  slice_min(vcrn, n = 5)

ggplot(data = top5_arg)+
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "steelblue")+
  coord_flip()+ #Para que las descripciones de los productos se lean horizontalmente.
  labs(title = "Argentina (2019)",
       x = "Producto",
       y = "VCRN")

ggplot(data = top5_mys)+
  geom_col(aes(x = reorder(cuci_desc, vcrn), y = vcrn), fill = "firebrick")+
  coord_flip()+
  labs(title = "Malasia (2019)",
       x = "Producto",
       y = "VCRN")
