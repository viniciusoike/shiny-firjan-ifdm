prep_histogram <- function(year_sel, series_geo) {
  series_geo |>
    dplyr::filter(year == year_sel) |>
    dplyr::select(name_muni_full, index_type, hdi)
}

plot_histogram <- function(
  city = "São Paulo (SP)",
  year_sel = 2016,
  series_geo
) {
  df <- prep_histogram(year_sel, series_geo)

  ggplot(df, aes(x = hdi)) +
    geom_histogram(
      aes(fill = index_type),
      color = "white",
      binwidth = 0.05
    ) +
    geom_hline(yintercept = 0, color = EKIO_INK) +
    geom_vline(
      data = filter(df, name_muni_full == city),
      aes(xintercept = hdi),
      linetype = 2,
      linewidth = 0.5,
      color = EKIO_INK
    ) +
    scale_x_continuous(breaks = seq(0.1, 1, 0.1)) +
    scale_y_continuous(expand = expansion(c(0, 0.1))) +
    scale_fill_manual(values = INDEX_PAL_LABELLED) +
    guides(fill = "none") +
    facet_wrap(vars(index_type)) +
    labs(
      subtitle = "Linha tracejada: município selecionado.",
      x = "IFDM",
      y = "Contagem"
    ) +
    theme_ekio(base_size = 12) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      strip.text = element_text(size = 11, face = "bold", color = EKIO_SURFACE)
    )
}
