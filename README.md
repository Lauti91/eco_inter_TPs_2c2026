# Economía Internacional — FCE-UBA (2C 2026) — Grupo 3

Repositorio del grupo para los Trabajos Prácticos de la materia
**Economía Internacional** (cátedra M.P. Ramos, Ma/Vi 17-19h). País asignado
al grupo: **Marruecos**.

---

## Integrantes

- Gonzalez, Paulo Lautaro  
- Molina, Ramiro Ernesto
- Ortiz, Franco Ignacio

## Sobre la materia

Entregas principales de la cursada:

| Entrega | Fecha |
|---|---|
| TP1 — Factsheet + Modelo Ricardiano | 01/09/2026 |
| Presentación Modelo Heckscher-Ohlin / Factores Específicos | 22/09/2026 |
| Ejercicios Modelo Estándar | 25/09/2026 |
| Ejercicios Movilidad Internacional de Factores | 02/10/2026 |
| 1er Parcial | 06/10/2026 |
| Presentación Política Comercial | 20/10/2026 |
| Ejercicios Economías de Escala / Modelos de Gravedad (R) | 03/11/2026 |
| TPO — referee report y presentación | 17/11/2026 |
| 2do Parcial | 24/11/2026 |
| Recuperatorio | 01/12/2026 |
| Final | 04/12/2026 |

## Estructura del repositorio

```
eco_inter_TPs_2c2026/
├── bases de datos/      # Bases .dta descargadas de WITS (Comtrade) y fuentes .xlsx, compartidas entre TPs
├── docs/                # Consignas de la cátedra (TP 1.pdf, TP 2.pdf)
├── prompts/              # Prompts de investigación enviados a Gemini Spark (rondas 1-4, TP2)
├── output/
│   ├── graficos/
│   │   ├── 01/           # Gráficos del TP1
│   │   ├── 02/           # Gráficos del TP2
│   │   └── 03/, 04/      # (a completar cuando corresponda)
│   └── tablas/
│       ├── 01/           # Tablas finales del TP1 (CSV) + objetos_heredados_tp1.RData
│       ├── 02/           # Tablas finales del TP2 (CSV)
│       └── 03/, 04/      # (a completar cuando corresponda)
├── scripts/
│   ├── FINAL/
│   │   ├── 01/            # 01_INDICADORES_FINAL.R + informe TP1 (.Rmd/.md)
│   │   └── 02/            # SCRIPT 1/2/3 del TP2 (datos y modelo base / OCDE y robustez / gráficos), en ese orden
│   └── PRUEBAS/
│       ├── 01/            # Borradores del TP1
│       └── 02/            # Borradores, auditorías y renders del TP2
├── .gitignore
├── project.Rproj
└── README.md
```

Convención: `FINAL/<entrega>` tiene el/los script(s) validados de esa entrega;
`PRUEBAS/<entrega>` guarda borradores y material de auditoría, no el
entregable. Cada entrega numerada (01, 02, 03, 04) corresponde a una de las
cuatro presentaciones/entregas grandes de la cursada (ver cronograma arriba).

## Paquetes necesarios

```r
# Comunes a todos los TPs
install.packages(c("tidyverse", "haven", "ggrepel", "scales", "RColorBrewer"))

# Específicos del TP2 (Script 1 de scripts/FINAL/02/ los instala solo si faltan)
install.packages(c("WDI", "pwt10", "readxl", "broom", "readr", "showtext"))
```

---

## TP1 — Factsheet e Indicadores de Comercio (Modelo Ricardiano)

**Entrega:** 01/09/2026

### Objetivo

Analizar el perfil de comercio exterior de Marruecos y contrastar su
ventaja comparativa global con la intensidad de su comercio bilateral,
para identificar si sus socios comerciales más relevantes en volumen
(España y Francia) son también los socios más "naturales" en su sector de
mayor especialización (fertilizantes y derivados).

Se calculan y analizan tres indicadores:

- **VCR / VCRN** (Ventaja Comparativa Revelada, Balassa 1965)
- **ICC** (Índice de Complementariedad Comercial, Michaely 1996)
- **IIC** (Índice de Intensidad Comercial, Yeats 1997)

### Datos

- **Fuente:** [World Integrated Trade Solution (WITS) — World Bank](https://wits.worldbank.org/)
- **Nomenclatura:** SITC Revisión 3, nivel de 3 dígitos (Grupo)
- **Reporter:** Marruecos (MAR)
- **Partners:**
  - España (ESP) y Francia (FRA) — principales socios comerciales por volumen
  - Brasil (BRA), India (IND) y un conjunto ampliado de ~77 países de África,
    Europa Occidental y del resto del mundo — usados para validar empíricamente
    la elección de socios y para identificar los verdaderos destinos del sector
    de mayor ventaja comparativa
  - World (WLD) — patrón exportador mundial y total exportado por
    Marruecos, necesario para el VCR
- **Flujo de comercio:** Exportaciones e Importaciones
- **Años:** desde 2021 hasta 2025

> Nota: las bases de World (`p == "WLD"`) y la base ampliada de países
> (`p == "All"` países individuales) provienen de descargas separadas en
> WITS, ya que la consulta con Partner = "All Countries" no siempre incluye
> el agregado mundial bajo la misma etiqueta.

### Análisis realizado

1. Descarga y limpieza de bases (`.dta` de WITS → R, vía `haven::read_dta()`).
2. Cálculo de VCR y VCRN de Marruecos contra el patrón exportador mundial.
3. Cálculo de ICC entre Marruecos (importador) y España/Francia (exportadores).
4. Cálculo de IIC de Marruecos hacia España, Francia, Brasil e India.
5. Cruce VCRN × IIC (bubble charts) por socio.
6. Comparación volumen vs. especialización (top 10 por valor exportado vs.
   top 10 por VCRN).
7. Validación empírica de socios comerciales y destinos del sector
   fertilizantes con la base ampliada de partners.

### Cómo correrlo

1. Descargar las bases desde WITS siguiendo los filtros de la sección
   [Datos](#datos) y guardarlas en `bases de datos/`.
2. Correr `scripts/FINAL/01/01_INDICADORES_FINAL.R`, que lee y limpia las
   bases, calcula VCR/VCRN, ICC e IIC, guarda los gráficos en
   `output/graficos/01/`, las tablas finales en `output/tablas/01/`, y deja
   guardado un `.RData` con los objetos livianos que heredan los TPs
   siguientes (evita tener que re-correr todo el TP1 desde cero).

### Conclusiones principales

- España y Francia son, en volumen, el 1° y 2° socio comercial real de
  Marruecos por amplio margen — validado empíricamente, no asumido por
  cercanía geográfica.
- El sector de mayor ventaja comparativa de Marruecos (fertilizantes,
  VCRN cercano a 1) no tiene como principal destino a España ni Francia,
  sino a economías agrícolas extra-regionales como Brasil e India.
- Existe una distinción clara entre sectores que explican **volumen**
  exportado (ej. automotriz, por integración industrial con la UE) y
  sectores que explican **especialización genuina** (ej. fertilizantes,
  pesca, corcho).

---

## TP2 — Modelo de Factores Específicos y Heckscher-Ohlin

**Presentación oral:** 22/09/2026

### Objetivo

Aplicar los dos modelos que integra la consigna —Factores Específicos (MFE,
corto plazo) y Heckscher-Ohlin (HO, largo plazo)— a los dos sectores
exportadores de Marruecos identificados en el TP1: **fosfatos** (fertilizantes
crudos y manufacturados, CUCI 272/562) y **automotor** (autos de pasajeros y
autopartes, CUCI 781/784), y comparar qué predice cada modelo sobre el
patrón exportador y los efectos distributivos de corto vs. largo plazo.

### Datos

Además de las bases de WITS heredadas del TP1, el TP2 suma:

- **World Bank WDI** — términos de intercambio (`TT.PRI.MRCH.XD.WD`)
- **Penn World Table 10.01** (paquete `pwt10`) — dotación K/L, capital
  humano y participación del trabajo en el ingreso (cobertura hasta 2019 en
  la versión pública del paquete)
- **OCDE, dataset `SUT_USEVA`** — Valor Agregado y Remuneración de
  Asalariados por actividad para Marruecos (sectores B08 y C29, 2014-2021);
  fuente de un alpha alternativo al de HCP para las intensidades factoriales
- **Datos duros investigados con Gemini Spark** (informes de OCP, USGS,
  OICA, Office des Changes, HCP, CNSS, UNCTADstat) — ver `prompts/` para los
  prompts de cada ronda de investigación y
  `scripts/PRUEBAS/02/TP2_seguimiento_verificacion_metodologica.md` para el
  detalle de qué se auditó, qué bugs se encontraron y cómo se corrigieron

### Análisis realizado

Los tres scripts de `scripts/FINAL/02/` se corren en orden (cada uno depende
del anterior):

1. **`SCRIPT 1 - DATOS Y MODELO BASE.R`** — hereda objetos del TP1 (via
   `.RData` si existe, si no corre el TP1 completo), carga los datos duros
   del TP2 desde el Excel de fuentes, y arma el modelo base del MFE/HO con
   alpha de HCP (Escenario A): FPP calibrada con Cobb-Douglas, desplazamiento
   de la FPP año a año, caja de asignación del trabajo (VPMgL), y comparación
   contra el techo técnico de Leontief.
2. **`SCRIPT 2 - OECD Y ROBUSTEZ.R`** — agrega el alpha alternativo de OCDE
   (Escenario B), corrige los perímetros de OCP/automotor y la fórmula de
   VPMgL (participación del trabajo observada en vez de `1-alpha`, valor
   agregado en vez de producción bruta — ver el documento de verificación
   metodológica), y contrasta la VPMgL corregida contra salarios reales de
   CNSS.
3. **`SCRIPT 3 - GRAFICOS.R`** — concentra los 13 gráficos del TP2
   (`ggplot` + `ggsave`), usando los objetos que dejan los dos scripts
   anteriores.

### Cómo correrlo

1. Confirmar que `bases de datos/` tiene el Excel de fuentes del TP2
   (`TP2_Economia_Internacional_Marruecos_Fuentes_de_Datos.xlsx`).
2. Correr, en orden, los tres scripts de `scripts/FINAL/02/`. Los gráficos
   quedan en `output/graficos/02/` y las tablas en `output/tablas/02/`.

### Estado

En elaboración de cara a la presentación del 22/09. El análisis de
robustez (Escenario A vs. B) ya muestra que la brecha de VPMgL entre
fosfatos y automotor, y la comparación contra el equilibrio teórico y el
techo de Leontief, son cualitativamente robustas a la fuente del parámetro
alpha — se van a presentar como un rango, no como un número único. Falta
volver a generar el informe final en PDF
(`scripts/PRUEBAS/02/TP2_MFE_HO_Marruecos (1).Rmd`) incorporando las
correcciones más recientes, que por ahora solo están en el render
intermedio `(4)`.
