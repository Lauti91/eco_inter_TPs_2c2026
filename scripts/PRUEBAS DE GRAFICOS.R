#----------------------------------------------------------------------
# grafico de evolución del VCRN
#----------------------------------------------------------------------

# Variación del VCRN entre el primer y último año disponible, por producto
variacion_vcrn <- vcr_mar |>
  filter(cuci %in% sectores_top, year %in% c(2021, 2025)) |>
  select(cuci, cuci_desc, year, vcrn) |>
  pivot_wider(names_from = year, values_from = vcrn, names_prefix = "y") |>
  mutate(caida = y2025 - y2021) |>
  arrange(caida)

variacion_vcrn  # mirá cuáles son los 2-3 con mayor caída real

# Nos quedamos con: los 3 de mayor VCRN estructural + los 2 de mayor caída
top_vcrn_2024 <- vcr_mar |> filter(year == 2024) |> slice_max(vcrn, n = 3) |> pull(cuci)
mayor_caida <- variacion_vcrn |> slice_min(caida, n = 2) |> pull(cuci)
sectores_reducidos <- union(top_vcrn_2024, mayor_caida)

vcr_mar |>
  filter(cuci %in% sectores_reducidos) |>
  ggplot(aes(x = year, y = vcrn, color = cuci_desc)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2) +
  labs(title = "Evolución del VCRN — sectores clave y de mayor caída",
       subtitle = "2021-2025", x = "Año", y = "VCRN", color = "Producto") +
  theme_minimal()




#----------------------------------------------------------------------
# evolución del IIC
#----------------------------------------------------------------------
iic_evolucion_bra <- map_dfr(2021:2025, ~calcular_iic_multi("BRA", sectores_top, .x))

iic_evolucion_deu <- map_dfr(2021:2025, ~calcular_iic_multi("DEU", sectores_top, .x))

iic_evolucion_usa <- map_dfr(2021:2025, ~calcular_iic_multi("USA", sectores_top, .x))

iic_evolucion_fra <- map_dfr(2021:2025, ~calcular_iic_multi("FRA", sectores_top, .x))


#brasil
ggplot(iic_evolucion_bra, aes(x = year, y = iic, color = cuci_desc)) +
  geom_line(linewidth = 1) + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_y_log10() +
  labs(title = "Evolución del IIC — Marruecos hacia Brasil",
       x = "Año", y = "IIC (escala log)", color = "Producto") +
  theme_minimal()

#alemania
ggplot(iic_evolucion_deu, aes(x = year, y = iic, color = cuci_desc)) +
  geom_line(linewidth = 1) + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_y_log10() +
  labs(title = "Evolución del IIC — Marruecos hacia Alemania",
       x = "Año", y = "IIC (escala log)", color = "Producto") +
  theme_minimal()


#USA
ggplot(iic_evolucion_usa, aes(x = year, y = iic, color = cuci_desc)) +
  geom_line(linewidth = 1) + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_y_log10() +
  labs(title = "Evolución del IIC — Marruecos hacia EE.UU.",
       x = "Año", y = "IIC (escala log)", color = "Producto") +
  theme_minimal()

#francia
ggplot(iic_evolucion_fra, aes(x = year, y = iic, color = cuci_desc)) +
  geom_line(linewidth = 1) + geom_point() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_y_log10() +
  labs(title = "Evolución del IIC — Marruecos hacia Francia",
       x = "Año", y = "IIC (escala log)", color = "Producto") +
  theme_minimal()


#----------------------------------------------------------------------
# ICC sin Brasil
#----------------------------------------------------------------------

icc_evolucion |>
  filter(socio != "BRA") |>
  ggplot(aes(x = year, y = icc, color = socio)) +
  geom_line(linewidth = 1) + geom_point() +
  labs(title = "Evolución del ICC de Marruecos por socio (excl. Brasil)",
       x = "Año", y = "ICC", color = "Socio") +
  theme_minimal()



#----------------------------------------------------------------------
# heatmap
#----------------------------------------------------------------------

iic_heatmap_cat <- iic_heatmap |>
  mutate(categoria = case_when(
    iic == 0 ~ "Sin comercio",
    iic < 0.5 ~ "Muy subrepresentado (<0.5)",
    iic < 1 ~ "Subrepresentado (0.5-1)",
    iic < 2 ~ "Intenso (1-2)",
    TRUE ~ "Muy intenso (>2)"
  ) |> factor(levels = c("Sin comercio", "Muy subrepresentado (<0.5)",
                         "Subrepresentado (0.5-1)", "Intenso (1-2)", "Muy intenso (>2)")))

ggplot(iic_heatmap_cat, aes(x = socio, y = reorder(cuci_desc, iic), fill = categoria)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(iic == 0, "—", round(iic, 2))), size = 3) +
  scale_fill_manual(values = c(
    "Sin comercio" = "gray85",
    "Muy subrepresentado (<0.5)" = "#fee5d9",
    "Subrepresentado (0.5-1)" = "#fcae91",
    "Intenso (1-2)" = "#a1d99b",
    "Muy intenso (>2)" = "#31a354"
  )) +
  labs(title = "IIC de los bienes más exportados por Marruecos, por socio comercial",
       subtitle = "2024 · bienes ordenados por volumen exportado al mundo",
       x = "Socio comercial", y = NULL, fill = "Intensidad") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())


#----------------------------------------------------------------------
# interacción ICC con VCRN
#----------------------------------------------------------------------

socios_finales <- c("ESP", "FRA", "DEU", "USA", "BRA")
destacados_ids <- c("562", "272", "842", "773", "522")

for (s in socios_finales) {
  datos <- armar_cruce(s)
  destacados_s <- datos |> filter(cuci %in% destacados_ids)
  
  p <- ggplot(datos, aes(x = vcrn, y = iic)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = value), alpha = 0.35, color = "gray40") +
    geom_point(data = destacados_s, aes(size = value, color = cuci_desc)) +
    geom_text_repel(data = destacados_s, aes(label = cuci_desc, color = cuci_desc),
                    size = 3, fontface = "bold", show.legend = FALSE) +
    scale_y_log10() +
    scale_size_continuous(range = c(1, 14)) +
    scale_color_brewer(palette = "Set1") +
    guides(color = "none", size = "none") +
    labs(title = paste("VCR vs. IIC — Marruecos hacia", s),
         subtitle = "2024 · tamaño = valor exportado", x = "VCRN", y = "IIC (escala log)") +
    theme_minimal(base_size = 13)
  
  print(p)
  ggsave(paste0("bubble_", s, ".png"), plot = p, width = 10, height = 6.5, dpi = 300, bg = "white")
}


#S