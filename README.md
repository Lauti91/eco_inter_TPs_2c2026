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
├── bases de datos/     # Bases .dta descargadas de WITS (Comtrade), compartidas entre TPs
├── graficos/
│   └── 01/              # Gráficos del TP1
│   └── 02/              # (a agregar cuando corresponda)
├── scripts/
│   └── 01_indicadores_clean.R   # Scripts del TP1
├── .gitignore
├── project.Rproj
└── README.md
```

A medida que avance la cursada, cada entrega suma su propia subcarpeta
numerada dentro de `graficos/` y su propio script en `scripts/`, siguiendo
la misma convención de numeración.

## Paquetes necesarios (comunes a todos los TPs)

```r
install.packages(c("tidyverse", "haven", "ggrepel", "scales"))
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
   [Datos](#datos-1) y guardarlas en `bases de datos/`.
2. Correr `scripts/01_indicadores_clean.R`, que lee y limpia las bases,
   calcula VCR/VCRN, ICC e IIC, y guarda los gráficos en `graficos/01/`.

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
