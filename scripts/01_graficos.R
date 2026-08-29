#===============================================================================#
# 0) ESTILO GLOBAL: paleta de socios (basada en banderas) + tema unificado
#===============================================================================#

paleta_socios <- c(
  "ESP" = "#AA151B",  # rojo de la bandera española
  "FRA" = "#0055A4",  # azul de la bandera francesa
  "DEU" = "#FFCE00",  # dorado de la bandera alemana
  "USA" = "#3C3B6E",  # azul marino de la bandera estadounidense
  "BRA" = "#009739"   # verde de la bandera brasileña
)

theme_tp1 <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2, color = "gray10"),
      plot.subtitle = element_text(color = "gray35", size = base_size - 1.5, margin = margin(b = 10)),
      plot.caption = element_text(color = "gray55", size = base_size - 5, hjust = 0, margin = margin(t = 10)),
      axis.title = element_text(color = "gray25", size = base_size - 1),
      axis.text = element_text(color = "gray40"),
      legend.title = element_text(face = "bold", size = base_size - 1),
      legend.text = element_text(size = base_size - 2),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray92", linewidth = 0.3),
      strip.text = element_text(face = "bold", size = base_size - 1)
    )
}


#===============================================================================#
# 1) EVOLUCIÓN DEL VCRN — sectores clave y de mayor caída
#===============================================================================#

variacion_vcrn <- vcr_mar |>
  filter(cuci %in% sectores_top, year %in% c(2021, 2025)) |>
  select(cuci, cuci_desc, year, vcrn) |>
  pivot_wider(names_from = year, values_from = vcrn, names_prefix = "y") |>
  mutate(caida = y2025 - y2021) |>
  arrange(caida)

variacion_vcrn

top_vcrn_2024 <- vcr_mar |> filter(year == 2024) |> slice_max(vcrn, n = 3) |> pull(cuci)
mayor_caida <- variacion_vcrn |> slice_min(caida, n = 2) |> pull(cuci)
sectores_reducidos <- union(top_vcrn_2024, mayor_caida)

g_vcrn <- vcr_mar |>
  filter(cuci %in% sectores_reducidos) |>
  ggplot(aes(x = year, y = vcrn, color = cuci_desc)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.2) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Evolución del VCRN — sectores clave y de mayor caída",
    subtitle = "Marruecos, 2021-2025",
    x = "Año", y = "VCRN", color = "Producto"
  ) +
  theme_tp1()

g_vcrn
ggsave("grafico_evolucion_vcrn.png", g_vcrn, width = 10, height = 6.5, dpi = 300, bg = "white")


#===============================================================================#
# 2) EVOLUCIÓN DEL IIC POR SOCIO (para explorar antes de elegir cuál mostrar)
#===============================================================================#

iic_evolucion_por_socio <- map_dfr(
  c("BRA", "DEU", "USA", "FRA", "ESP"),
  ~map_dfr(2021:2025, function(a) calcular_iic_multi(.x, sectores_top, a)) |> mutate(socio = .x)
)

nombres_socio <- c(BRA = "Brasil", DEU = "Alemania", USA = "EE.UU.", FRA = "Francia", ESP = "España")

graficar_iic_evolucion <- function(socio_code) {
  datos <- iic_evolucion_por_socio |> filter(socio == socio_code)
  ggplot(datos, aes(x = year, y = iic, color = cuci_desc)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    scale_y_log10() +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = paste("Evolución del IIC — Marruecos hacia", nombres_socio[socio_code]),
      subtitle = "2021-2025 · principales sectores de ventaja comparativa",
      x = "Año", y = "IIC (escala log)", color = "Producto"
    ) +
    theme_tp1()
}

g_iic_bra <- graficar_iic_evolucion("BRA"); g_iic_bra
g_iic_deu <- graficar_iic_evolucion("DEU"); g_iic_deu
g_iic_usa <- graficar_iic_evolucion("USA"); g_iic_usa
g_iic_fra <- graficar_iic_evolucion("FRA"); g_iic_fra


#===============================================================================#
# 3) EVOLUCIÓN DEL ICC POR SOCIO (excluyendo Brasil, distorsiona la escala)
#===============================================================================#

g_icc <- icc_evolucion |>
  filter(socio != "BRA") |>
  ggplot(aes(x = year, y = icc, color = socio)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = paleta_socios, labels = nombres_socio) +
  labs(
    title = "Evolución del ICC de Marruecos por socio comercial",
    subtitle = "España, Francia, Alemania y EE.UU. — 2021-2025",
    x = "Año", y = "ICC", color = "Socio",
    caption = "Se excluye Brasil (ICC estructuralmente bajo) para no distorsionar la escala."
  ) +
  theme_tp1()

g_icc
ggsave("grafico_evolucion_icc.png", g_icc, width = 10, height = 6.5, dpi = 300, bg = "white")


#===============================================================================#
# 4) HEATMAP — IIC de los bienes más exportados, por socio
#===============================================================================#

iic_heatmap_cat <- iic_heatmap |>
  mutate(
    socio_label = factor(socio, levels = names(paleta_socios), labels = nombres_socio[names(paleta_socios)]),
    categoria = case_when(
      iic == 0 ~ "Sin comercio",
      iic < 0.5 ~ "Muy subrepresentado (<0.5)",
      iic < 1 ~ "Subrepresentado (0.5-1)",
      iic < 2 ~ "Intenso (1-2)",
      TRUE ~ "Muy intenso (>2)"
    ) |> factor(levels = c("Sin comercio", "Muy subrepresentado (<0.5)",
                           "Subrepresentado (0.5-1)", "Intenso (1-2)", "Muy intenso (>2)"))
  )

g_heatmap <- ggplot(iic_heatmap_cat, aes(x = socio_label, y = reorder(cuci_desc, iic), fill = categoria)) +
  geom_tile(color = "gray95", linewidth = 0.8) +   # borde más sutil, casi invisible
  geom_text(aes(label = ifelse(iic == 0, "—", round(iic, 2))), size = 3, color = "gray15") +
  scale_fill_manual(values = c(
    "Sin comercio" = "gray88",
    "Muy subrepresentado (<0.5)" = "#fee0d2",
    "Subrepresentado (0.5-1)" = "#fc9272",
    "Intenso (1-2)" = "#a1d99b",
    "Muy intenso (>2)" = "#31a354"
  )) +
  labs(
    title = "IIC de los bienes más exportados por Marruecos, por socio comercial",
    subtitle = "2024 · bienes ordenados por volumen exportado al mundo",
    x = "Socio comercial", y = NULL, fill = "Intensidad",
    caption = "IIC = 1 indica comercio bilateral proporcional al patrón exportador global de Marruecos."
  ) +
  theme_tp1(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.ticks = element_blank()
  )

g_heatmap
ggsave("grafico_heatmap_iic.png", g_heatmap, width = 11, height = 7, dpi = 300, bg = "white")


#===============================================================================#
# 5) BUBBLE CHART — VCR vs. IIC, uno por socio (para elegir cuál presentar)
#===============================================================================#

#===============================================================================#
# Bubble chart final: VCR vs. IIC, con destacados fijos + destacados dinámicos
# por socio (productos de alto volumen con IIC > 1 propios de cada país)
#===============================================================================#

destacados_ids <- c("562", "272", "842", "773", "522")

graficar_bubble <- function(socio_code, iic_min = 0.02, n_extra = 3) {
  datos <- armar_cruce(socio_code) |> filter(iic >= iic_min)
  
  # Los 5 productos de referencia, iguales en todos los gráficos
  destacados_fijos <- datos |> filter(cuci %in% destacados_ids)
  
  # + los n_extra de mayor volumen entre los que tienen IIC > 1 y no son ya un destacado fijo
  # (esto es lo que saca a la luz las burbujas grandes propias de cada socio, ej. Francia)
  destacados_extra <- datos |>
    filter(iic > 1, !cuci %in% destacados_ids) |>
    slice_max(value, n = n_extra)
  
  destacados_s <- bind_rows(destacados_fijos, destacados_extra) |>
    distinct(cuci, .keep_all = TRUE)
  
  n_dest <- nrow(destacados_s)
  colores_solidos <- RColorBrewer::brewer.pal(max(n_dest, 3), "Set1")[1:n_dest]
  names(colores_solidos) <- destacados_s$cuci_desc
  colores_relleno <- scales::alpha(colores_solidos, 0.35)
  
  rango_valor <- range(sqrt(datos$value))
  destacados_s <- destacados_s |>
    mutate(size_real = scales::rescale(sqrt(value), from = rango_valor, to = c(1, 14)))
  
  ggplot(datos, aes(x = vcrn, y = iic)) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = 1, ymax = Inf, fill = "steelblue", alpha = 0.06) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 1, fill = "firebrick", alpha = 0.06) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_point(aes(size = value), shape = 21, color = "gray50", fill = scales::alpha("gray50", 0.15), stroke = 0.4) +
    geom_point(
      data = destacados_s, aes(size = value, color = cuci_desc, fill = cuci_desc),
      shape = 21, stroke = 1.1
    ) +
    geom_text_repel(
      data = destacados_s, aes(label = cuci_desc, color = cuci_desc),
      size = 3.3, fontface = "bold", show.legend = FALSE,
      point.size = destacados_s$size_real,
      box.padding = 1, min.segment.length = 0,
      segment.color = "gray50", force = 10, max.iter = 15000
    ) +
    scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
    scale_x_continuous(limits = c(-1.05, 1.05), breaks = seq(-1, 1, 0.5)) +
    scale_size_continuous(range = c(1, 14)) +
    scale_color_manual(values = colores_solidos) +
    scale_fill_manual(values = colores_relleno) +
    guides(color = "none", fill = "none", size = "none") +
    labs(
      title = paste("VCR vs. IIC — Marruecos hacia", nombres_socio[socio_code]),
      subtitle = "2024 · el tamaño del punto es el valor exportado",
      x = "VCRN — ventaja comparativa revelada normalizada",
      y = "IIC (escala log)",
      caption = paste0(
        "Línea punteada horizontal: IIC = 1 · vertical: VCRN = 0\n",
        "Se excluyen productos con IIC < ", iic_min, ". Etiquetas: 5 productos de referencia",
        if (n_extra > 0) paste0(" + hasta ", n_extra, " de mayor volumen con IIC > 1 propios de este socio.") else "."
      )
    ) +
    theme_tp1()
}

# Umbrales de IIC ajustados por socio: Alemania necesita uno mucho más bajo
# para no perder "Fertilizers crude" y "Elements/oxides/hal salt" (ver conversación previa)
umbrales_iic <- c(ESP = 0.02, FRA = 0.02, DEU = 0.001, USA = 0.02, BRA = 0.02)

for (s in names(umbrales_iic)) {
  p <- graficar_bubble(s, iic_min = umbrales_iic[[s]])
  print(p)
  ggsave(paste0("bubble_", s, ".png"), plot = p, width = 10, height = 6.5, dpi = 300, bg = "white")
}

