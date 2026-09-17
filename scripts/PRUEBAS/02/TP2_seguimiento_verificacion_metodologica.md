# TP2 — Seguimiento: de la auditoría de datos a la verificación metodológica

Documento para Gemini Spark. Cubre todo lo ocurrido desde la auditoría
empírica de los datos crudos hasta la verificación metodológica de la
construcción del modelo. Complementa a `TP2_fuentes_de_datos.md`,
`TP2_seguimiento_investigacion_ronda2.md` y `TP2_estado_y_plan.md`.

---

## 1. Cierre de las tres alertas de la auditoría

### 1.1 Reservas de fosfatos: resuelto, la planilla tenía razón

La auditoría marcó como posible error de unidades el valor `reservas_mt`.
Se resolvió estructuralmente: en vez de que el script asumiera una unidad,
se hizo que leyera e imprimiera la celda `Unidad` **tal como figura en la
planilla**. Resultado del render:

- Valor: `5e+07` (50.000.000)
- Unidad en la planilla: "Miles de toneladas métricas"
- Es decir: 50.000.000 miles de toneladas = 50 Gt ≈ 70% de las reservas
  mundiales. **Coincide con la cifra real conocida.**

La alerta era infundada: la planilla estaba bien. Lo que sí hubo fue un
error previo de transcripción manual (se había tipeado 50.000 en vez de
50.000.000, un factor de 1000x), que quedó expuesto justamente al pasar a
leer la fuente por código en vez de escribirla de memoria.

### 1.2 Cobertura de PWT 10.01: incorporada

Se agregó al caption de los gráficos de K/L y `labsh` la aclaración de que
la Penn World Table 10.01 cubre hasta 2019 y que eso es el límite de la
versión pública del paquete, no un descuido de descarga.

### 1.3 Componentes de exportaciones automotrices: incorporada

Se agregó a la tabla la nota de que `cableado + ensamblaje` no suman el
total porque son los dos subsegmentos más grandes (~75-80%), no una
descomposición exhaustiva.

---

## 2. Bug encontrado por el propio chequeo de cobertura

El script imprime, después del parseo, las filas que no se pudieron
convertir a número y que no son "NO ENCONTRADO". Ese chequeo detectó una
fila que sí debía convertirse y no lo hacía:

- `Crecimiento volumen VA - Minería (B00)`, 2023, valor `-4.2` → quedaba
  como `NA`.
- **Causa**: el parser detectaba rangos buscando cualquier guion, y un
  número negativo también tiene guion. Confundía `-4.2` con un rango tipo
  `"75% - 85%"` y lo descartaba.
- **Fix**: exigir que el guion tenga un dígito o `%` **antes** para contar
  como rango (`[%\\d]\\s*-\\s*\\d`). Verificado en el render posterior:
  `mineria_pct` 2023 ahora muestra -4,2.

Tras el fix, la única fila que queda sin convertir es el régimen fiscal de
AMDIE, que es texto descriptivo y es correcto que no sea numérico.

---

## 3. Nuevo dato conseguido: serie OCDE de intensidades factoriales

### 3.1 Qué se encontró

Marruecos **sí está** en el dataset `SUT_USEVA` de la OCDE ("SUT Use,
Value added and its components by activity"), con serie **2014-2021** y
las variables exactas que hacían falta:

- `B1G` = Value added, gross
- `D1` = Compensation of employees
- `B2A3G` = Operating surplus and mixed income, gross
- Industrias: `B08` (Other mining and quarrying → fosfatos, mineral no
  metálico; `B07` es metálica, no OCP) y `C29` (Motor vehicles).

Acceso: el endpoint SDMX con filtro de país devolvió **403**; el CSV
completo sin filtro (`.../DF_USEVA_T1600,1.0/all?format=csvfilewithlabels`)
sí funciona, y se filtra por `REF_AREA == "MAR"` en R.

Esto resuelve la "Opción B" que la ronda anterior había dejado abierta
(calcular alpha desde VA y remuneración de asalariados, en vez de vía los
coeficientes agregados del HCP).

### 3.2 Hallazgo: las intensidades factoriales convergen

| Año | Alpha capital fosfatos (B08) | Alpha capital automotor (C29) |
|---|---|---|
| 2014 | 0,707 | 0,488 |
| 2021 | 0,539 | 0,568 |

Regresión de alpha contra año:

| Modelo | Pendiente anual | p-valor | R² | n |
|---|---|---|---|---|
| Fosfatos (B08), 2014-2021 | -0,0251 | 0,0019 | 0,821 | 8 |
| Fosfatos (B08), sin 2020 | **-0,0251** | 0,0073 | 0,792 | 7 |
| Automotor (C29), 2014-2021 | +0,0114 | 0,0077 | 0,721 | 8 |

Dos cosas importantes:
1. La pendiente de fosfatos es **idéntica** sacando 2020 → no es un
   efecto COVID, es tendencia estructural.
2. Los sectores se mueven en **direcciones opuestas** → no es una deriva
   macro general de la economía marroquí.

Interpretación adoptada: esto no rompe el análisis, lo enriquece. El MFE
toma las intensidades factoriales como **dadas** (corto plazo); el HO las
ve **moverse** (largo plazo). La convergencia observada es material del
Bloque 3, no un problema del Bloque 2.

### 3.3 Contraste entre fuentes: la discrepancia era menor de lo que parecía

| Sector | HCP (base 2014) | OCDE 2014 | OCDE 2021 |
|---|---|---|---|
| Fosfatos | 0,80 | 0,707 | 0,539 |
| Automotor | 0,55 | 0,488 | 0,568 |

Comparando HCP contra el **mismo año** (OCDE 2014), la diferencia en
fosfatos baja de 0,26 a 0,09. Buena parte de la aparente contradicción
entre fuentes era estar comparando años distintos, no metodologías
incompatibles. En automotor ambas fuentes coinciden casi exactamente.

### 3.4 Lo que se descartó explícitamente

- **Extrapolar alpha a 2023** con la recta estimada. Daría 0,469
  (fosfatos) y 0,611 (automotor), invirtiendo la premisa del trabajo. Se
  descartó porque proyectar dos años más allá del último dato atraviesa
  el ciclo de precios 2022-2023 (margen EBITDA de OCP: 43,7% → 32,2%) que
  la regresión nunca vio.
- **LACEX (WITS)**: tiene contenido factorial del comercio para Marruecos,
  pero su último año es **2011**. Demasiado viejo para anclar un análisis
  de 2023.

---

## 4. Verificación metodológica de la construcción de VPMgL

### 4.1 Lo que está correcto

El álgebra de la construcción es válida. Partiendo de una Cobb-Douglas con
el factor específico fijo, `Q = A · L^(1-α)`:

```
PMgL  = (1-α) · Q/L
VPMgL = P · PMgL,  con P = Ingresos/Q
      = (1-α) · Ingresos/L
```

Verificado numéricamente contra los resultados del script: escenario A da
VPMgL_fosfatos = 1,0738 y VPMgL_automotor = 0,2774 (brecha 3,872);
escenario B da 2,4752 y 0,2663 (brecha 9,296). Ambos coinciden.

Nótese que el precio implícito se cancela: la VPMgL en el punto observado
**no depende de los datos de producción física**, solo de la participación
factorial, los ingresos y el empleo.

### 4.2 Problema 1 (menor): α + participación del trabajo ≠ 1

El valor agregado se reparte en tres, no en dos: compensación de
empleados, excedente de explotación **e impuestos netos sobre la
producción**.

| Serie | α (B2A3G/B1G) | Trabajo (D1/B1G) | Suma |
|---|---|---|---|
| B08 2021 | 0,5392 | 0,4472 | 0,9865 |
| C29 2021 | 0,5682 | 0,4282 | 0,9965 |

Usar `(1-α)` como participación del trabajo introduce un error de ~1,4%
en fosfatos. **Corrección sugerida**: usar `share_trabajo` (D1/B1G)
directamente, que es un dato observado, en vez de derivarlo como `1-α`.

### 4.3 Problema 2 (serio): mezcla de valor agregado con producción bruta

Las participaciones α y (1-α) son **sobre valor agregado**, pero en el
script se multiplican por ingresos de OCP y exportaciones automotrices,
que son **producción bruta**. No son la misma magnitud: la diferencia es
el consumo intermedio.

Y el ratio VA/producción difiere dramáticamente entre los dos sectores
(según los propios `io_coeficientes` del HCP):

| Sector | Consumo intermedio / producción | VA / producción | Factor de sobreestimación |
|---|---|---|---|
| Minería | 0,35-0,45 (medio 0,40) | 0,60 | ×1,67 |
| Automotor | 0,68-0,78 (medio 0,73) | 0,27 | ×3,70 |

Como el factor de sobreestimación es más del doble en automotor, el sesgo
opera **en contra** de la tesis del trabajo: subestima la brecha.

Efecto de corregirlo (multiplicar por VA en vez de por producción bruta):

| Escenario | Brecha actual (producción bruta) | Brecha corregida (valor agregado) |
|---|---|---|
| A (HCP) | 3,87x | **8,60x** |
| B (OCDE 2021) | 9,30x | **20,66x** |

La conclusión cualitativa se sostiene **a fortiori**: corregir el error
refuerza el hallazgo en vez de debilitarlo.

### 4.4 La validación externa que aparece al corregir

Bajo Cobb-Douglas con mercados competitivos, la VPMgL debería igualar el
salario efectivo. Contrastando la VPMgL corregida contra los salarios
reales de CNSS:

| Escenario | VPMgL fosfatos | VPMgL automotor |
|---|---|---|
| A (HCP), sobre VA | 53.667 MAD/mes | **6.242 MAD/mes** |
| B (OCDE 2021), sobre VA | 123.750 MAD/mes | **5.992 MAD/mes** |

Referencias CNSS: "Industrie" (proxy automotor) = **5.002 MAD/mes**
(2020); promedio general del régimen = **5.871 MAD/mes** (2024).

**El modelo predice el salario del sector automotor con notable
precisión** (6.000 MAD/mes predichos vs. 5.000-5.900 observados), y falla
por un factor de 9 a 25 exactamente en fosfatos. Es decir: el modelo
funciona donde el mercado de trabajo opera competitivamente, y se rompe
justo donde el trabajo argumenta que hay segmentación institucional
(monopolio estatal, empleo no fijado por productividad marginal, renta
capturada vía dividendos al Tesoro).

Esto convierte una discrepancia que parecía un problema del modelo en una
**validación cruzada del diagnóstico institucional**, y es empíricamente
mucho más fuerte que la versión sin corregir.

---

## 5. Estado de robustez del Bloque 2

Se recalculó todo el MFE (FPP, caja de asignación, equilibrio, techo
Leontief) bajo ambos escenarios de alpha, con la misma función, de modo
que cualquier diferencia venga solo del parámetro. Resultado:

| Pregunta | Escenario A | Escenario B |
|---|---|---|
| ¿La brecha VPMgL observada sigue siendo > 1? | ✅ | ✅ |
| ¿El equilibrio teórico sigue muy por encima del empleo observado? | ✅ | ✅ |
| ¿El equilibrio teórico sigue por encima del techo técnico Leontief? | ✅ | ✅ |

El análisis es robusto a la elección de la fuente del parámetro. Se
presentará el escenario A como principal y el B como robustez, citando la
brecha como un **rango** ("entre 4 y 9 veces", o entre 9 y 21 una vez
aplicada la corrección de la sección 4.3) en vez de un número único.

---

## 6. Qué se le pide a Spark en esta ronda

1. **Verificar el ratio VA/producción de ambos sectores** contra fuentes
   primarias (HCP, Comptes Nationaux: producción, consumo intermedio y
   valor agregado por rama para minería B00 y automotor/IMME C00, años
   disponibles). Los valores usados (0,60 y 0,27) salen de los rangos de
   `io_coeficientes`; conviene tener el dato directo y por año, no el
   punto medio de un rango.
2. **Confirmar si existe el valor agregado de OCP específicamente** (no de
   toda la rama minera) en sus reportes financieros: ingresos menos
   consumos intermedios, o "valeur ajoutée" si lo reportan directamente.
   Eso permitiría usar el VA real de la empresa en vez de aproximarlo con
   un coeficiente sectorial.
3. **Salario medio efectivo de OCP** (masa salarial / plantilla, de los
   estados financieros). Es la contraparte directa del test de la sección
   4.4: si OCP paga muy por debajo de la VPMgL corregida, el argumento de
   renta no arbitrada queda cerrado con datos de la propia empresa.
4. **Datos de consumo intermedio del sector automotor** (Office des
   Changes o Ministère de l'Industrie): el sector importa la mayoría de
   sus insumos, lo que explicaría el VA/producción bajo. Confirmar el
   orden de magnitud.

Mismas reglas de siempre: no inventar, marcar NO ENCONTRADO con el
obstáculo, series completas cuando existan, cada dato con URL exacta y
fecha de consulta.
