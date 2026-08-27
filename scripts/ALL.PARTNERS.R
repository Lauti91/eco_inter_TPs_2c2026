comtrade_mar_p_all <- read_dta("MAR_P_WLD.dta")
names(comtrade_mar_p_all) <- tolower(names(comtrade_mar_p_all))

comtrade_mar_p_all <- comtrade_mar_p_all %>%
  mutate(
    cuci      = as.character(as_factor(productcode)),
    cuci_desc = as.character(as_factor(productdescription)),
    p         = as.character(as_factor(partneriso3)),
    flow      = as.character(as_factor(tradeflowname))
  ) %>%
  rename(value = tradevaluein1000usd) %>%
  select(p, flow, cuci, cuci_desc, value, year)

comtrade_mar_p_all <- comtrade_mar_p_all |>
  filter(p != "All")

# 1) Ranking real de socios comerciales de Marruecos (exportaciones totales, 2024)
top_socios <- comtrade_mar_p_all |>
  filter(flow == "Export", year == 2024, p != "WLD") |>
  group_by(p) |>
  summarise(total = sum(value, na.rm = TRUE)) |>
  arrange(desc(total)) |>
  slice_max(total, n = 15)

top_socios

# 2) La pregunta clave: ¿a quién le exporta Marruecos fertilizantes (272 y 562)?
destino_fertilizantes <- comtrade_mar_p_all |>
  filter(flow == "Export", year == 2024, cuci %in% c("272", "562"), p != "WLD") |>
  group_by(p, cuci_desc) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total)) |>
  slice_max(total, n = 15)

destino_fertilizantes

comtrade_mar_p_all %>% distinct(p) %>% print(n = Inf)


#---------------------------------
# volumenes de expo coinciden con vcr? 

# Top 10 por volumen exportado 
top10_volumen <- comtrade_exp |>
  filter(r == "MAR", p == "WLD", year == 2024) |>
  select(cuci, cuci_desc, value) |>
  arrange(desc(value)) |>
  slice_max(value, n = 10)

top10_volumen

# Top 10 por VCRN
top10_vcrn <- vcr_mar |>
  filter(year == 2024) |>
  arrange(desc(vcrn)) |>
  slice_max(vcrn, n = 10) |>
  select(cuci, cuci_desc, vcr, vcrn)

top10_vcrn

interseccion <- intersect(top10_volumen$cuci, top10_vcrn$cuci)
length(interseccion)
interseccion

#---------
# grafico

# Unión de ambos rankings, con etiqueta de a qué grupo pertenece cada producto
productos_relevantes <- union(top10_volumen$cuci, top10_vcrn$cuci)

grafico_data <- comtrade_exp |>
  filter(r == "MAR", p == "WLD", year == 2024, cuci %in% productos_relevantes) |>
  select(cuci, cuci_desc, value) |>
  left_join(
    vcr_mar |> filter(year == 2024) |> select(cuci, vcrn),
    by = "cuci"
  ) |>
  mutate(
    grupo = case_when(
      cuci %in% top10_volumen$cuci & cuci %in% top10_vcrn$cuci ~ "Core: volumen + especialización",
      cuci %in% top10_volumen$cuci ~ "Solo volumen (integración industrial)",
      cuci %in% top10_vcrn$cuci ~ "Solo especialización (nicho)"
    )
  )

ggplot(grafico_data, aes(x = value, y = vcrn, color = grupo)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = median(grafico_data$vcrn),
           fill = "gray50", alpha = 0.04) +
  geom_point(size = 5, alpha = 0.85) +
  geom_text_repel(aes(label = cuci_desc), size = 3.4, fontface = "bold",
                  show.legend = FALSE, box.padding = 0.6,
                  segment.color = "gray50", min.segment.length = 0) +
  scale_x_log10(labels = scales::label_number(scale = 1, suffix = "k US$")) +
  scale_color_manual(values = c(
    "Core: volumen + especialización"        = "#2c7fb8",
    "Solo volumen (integración industrial)"  = "#f03b20",
    "Solo especialización (nicho)"           = "#31a354"
  )) +
  labs(
    title = "Volumen exportado vs. especialización — Marruecos 2024",
    subtitle = "Los autos dominan en volumen sin ser especialización real; los fertilizantes crudos son al revés",
    x = "Valor exportado a nivel mundial (escala log, miles US$)",
    y = "VCRN — ventaja comparativa revelada normalizada",
    color = NULL,
    caption = "Solo se muestran productos que integran el top 10 en volumen y/o en VCRN (2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 10.5),
    panel.grid.minor = element_blank()
  )

ggsave("scatter_volumen_vs_vcrn.png", width = 11, height = 7, dpi = 300, bg = "white")


#GRAFICO 2

# Armamos el dataset con ranking en volumen y en VCRN para el mismo set de productos
# slope_data <- comtrade_exp |>
#   filter(r == "MAR", p == "WLD", year == 2024, cuci %in% productos_relevantes) |>
#   select(cuci, cuci_desc, value) |>
#   left_join(vcr_mar |> filter(year == 2024) |> select(cuci, vcrn), by = "cuci") |>
#   mutate(
#     rank_volumen = rank(-value),
#     rank_vcrn    = rank(-vcrn),
#     grupo = case_when(
#       cuci %in% top10_volumen$cuci & cuci %in% top10_vcrn$cuci ~ "Core",
#       cuci %in% top10_volumen$cuci ~ "Solo volumen",
#       cuci %in% top10_vcrn$cuci ~ "Solo especialización"
#     )
#   ) |>
#   pivot_longer(cols = c(rank_volumen, rank_vcrn), names_to = "eje", values_to = "rank") |>
#   mutate(eje = recode(eje, rank_volumen = "Ranking\npor Volumen", rank_vcrn = "Ranking\npor VCRN"))
# 
# ggplot(slope_data, aes(x = eje, y = rank, group = cuci_desc, color = grupo)) +
#   geom_line(linewidth = 1, alpha = 0.7) +
#   geom_point(size = 3.5) +
#   geom_text(
#     data = slope_data |> filter(eje == "Ranking\npor Volumen"),
#     aes(label = cuci_desc), hjust = 1.1, size = 3.3, fontface = "bold", show.legend = FALSE
#   ) +
#   geom_text(
#     data = slope_data |> filter(eje == "Ranking\npor VCRN"),
#     aes(label = cuci_desc), hjust = -0.1, size = 3.3, fontface = "bold", show.legend = FALSE
#   ) +
#   scale_y_reverse(breaks = 1:10) +
#   scale_color_manual(values = c(
#     "Core"                  = "#2c7fb8",
#     "Solo volumen"          = "#f03b20",
#     "Solo especialización"  = "#31a354"
#   )) +
#   expand_limits(x = c(0.3, 2.7)) +
#   labs(
#     title = "¿Lo que Marruecos más exporta es lo que mejor sabe exportar?",
#     subtitle = "Comparación entre ranking por volumen y ranking por ventaja comparativa (VCRN), 2024",
#     x = NULL, y = "Posición en el ranking (1 = primero)",
#     color = NULL
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(
#     legend.position = "top",
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     axis.text.x = element_text(face = "bold", size = 12),
#     plot.title = element_text(face = "bold", size = 14),
#     plot.subtitle = element_text(color = "gray30", size = 10.5)
#   )
# 
# ggsave("slope_volumen_vs_vcrn.png", width = 10, height = 8, dpi = 300, bg = "white")


# BUBBLE CHART CON BRASIL COMO EL QUE HICE CON ESPAÑA

#===============================================================================#
# IIC y bubble chart — Marruecos hacia Brasil (usando comtrade_mar_p_all)
#===============================================================================#

# Función adaptada: bilateral desde comtrade_mar_p_all, total mundial desde comtrade_exp
calcular_iic_multi_all <- function(socio, sectores, anio) {
  total_socio <- comtrade_mar_p_all |>
    filter(flow == "Export", p == socio, year == anio) |>
    summarise(total = sum(value, na.rm = TRUE)) |>
    pull(total)
  
  mar_soc <- comtrade_mar_p_all |>
    filter(flow == "Export", p == socio, year == anio, cuci %in% sectores) |>
    select(cuci, cuci_desc, xijk = value) |>
    mutate(xij = total_socio)
  
  mar_wld <- comtrade_exp |>
    filter(r == "MAR", p == "WLD", year == anio, cuci %in% sectores) |>
    select(cuci, xik = value, xi = total_expo)
  
  mar_soc |>
    left_join(mar_wld, by = "cuci") |>
    mutate(iic = as.numeric((xijk / xik) / (xij / xi)), year = anio) |>
    select(year, cuci, cuci_desc, iic)
}

# Chequeo rápido antes de armar el gráfico completo
calcular_iic_multi_all("BRA", c("272", "562"), 2024)

# Cruce VCRN x IIC hacia Brasil, para todos los productos en común
cruce_vcr_iic_bra <- vcr_mar |>
  filter(year == 2024) |>
  select(cuci, cuci_desc, vcrn) |>
  inner_join(
    calcular_iic_multi_all("BRA", unique(vcr_mar$cuci), 2024) |> select(cuci, iic),
    by = "cuci"
  ) |>
  filter(!is.na(iic), is.finite(iic), iic > 0)

# Sumamos el volumen exportado a Brasil (tamaño de la burbuja)
cruce_vcr_iic_bra_vol <- cruce_vcr_iic_bra |>
  left_join(
    comtrade_mar_p_all |> filter(flow == "Export", p == "BRA", year == 2024) |> select(cuci, value),
    by = "cuci"
  ) |>
  filter(!is.na(value), value > 0)

# Excluimos productos con IIC muy bajo para mejorar legibilidad
n_excluidos_bra <- sum(cruce_vcr_iic_bra_vol$iic < 0.02, na.rm = TRUE)
cruce_vcr_iic_bra_recortado <- cruce_vcr_iic_bra_vol |> filter(iic >= 0.02)

# Mismos productos destacados que en el gráfico de España, para comparar directo
destacados_bra <- cruce_vcr_iic_bra_recortado |>
  filter(cuci %in% c("562", "272", "842", "773", "522"))

ggplot(cruce_vcr_iic_bra_recortado, aes(x = vcrn, y = iic)) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 1, ymax = Inf,
           fill = "steelblue", alpha = 0.06) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 1,
           fill = "firebrick", alpha = 0.06) +
  geom_point(aes(size = value), alpha = 0.35, color = "gray40") +
  geom_point(data = destacados_bra, aes(size = value, color = cuci_desc)) +
  geom_text_repel(
    data = destacados_bra, aes(label = cuci_desc, color = cuci_desc),
    size = 3.5, fontface = "bold", show.legend = FALSE,
    box.padding = 0.8, point.padding = 0.4,
    segment.color = "gray50", min.segment.length = 0
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
  scale_y_log10(labels = scales::label_number(accuracy = 0.01)) +
  scale_size_continuous(range = c(1, 14),
                        labels = scales::label_number(suffix = "k US$")) +
  scale_color_brewer(palette = "Set1") +
  guides(color = "none", size = "none") +
  labs(
    title = "Ventaja comparativa global, intensidad bilateral y volumen — Marruecos hacia Brasil",
    subtitle = "2024 · el tamaño del punto es el valor exportado a Brasil",
    x = "VCRN — ventaja comparativa revelada normalizada",
    y = "IIC (escala log)",
    caption = paste0(
      "Línea punteada horizontal: IIC = 1 (comercio ni más ni menos intenso de lo esperado)\n",
      "Línea punteada vertical: VCRN = 0 (frontera entre ventaja y desventaja comparativa)\n",
      "Se excluyen ", n_excluidos_bra, " productos con IIC < 0.02 para mejorar la legibilidad del gráfico"
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 11),
    plot.caption = element_text(color = "gray50", size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92")
  )

ggsave("bubble_vcr_iic_brasil.png", width = 11, height = 7, dpi = 300, bg = "white")

ultimo_grafico <- last_plot()

ggsave(
  filename = "vcr_vs_iic_espana.png",
  plot = ultimo_grafico,
  width = 10, height = 6.5,
  dpi = 300,          
  bg = "white"          
)
