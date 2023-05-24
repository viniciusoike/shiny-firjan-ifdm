prep_benchmark <- function(df, type = "brazil") {

  if (type == "brazil") {
    benchmark <- df |>
      dplyr::select(year, index_type, name_muni_full, hdi) |>
      dplyr::bind_rows(benchmark_bra)
  }
  if (type == "state") {
    benchmark <- df |>
      dplyr::select(year, index_type, name_muni_full, hdi) |>
      dplyr::bind_rows(benchmark_state)
  }
  # Round numbers for better presentation
  benchmark <- dplyr::mutate(benchmark, hdi = round(hdi, 3))
  return(benchmark)
}

prep_series_data <- function(city) {
  df <- series_data |>
    dplyr::filter(name_muni_full == city) |>
    dplyr::mutate(hdi = round(hdi, 3))
  return(df)
}

plot_series <- function(df) {

  p <-
    ggplot(
      df,
      aes(
        year,
        hdi,
        color = index_type,
        text = paste("Ano:", year, "<br>IDH:", hdi, "<br>Indicador:", index_type))
      ) +
    geom_line(aes(group = index_type)) +
    geom_point() +
    scale_x_continuous(breaks = 2005:2016) +
    scale_y_continuous(breaks = seq(0, 1, 0.05)) +
    scale_color_manual(name = "", values = colors_qual) +
    labs(
      title = "Indicadores",
      x = NULL,
      y = "IDH (Firjan)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  plotly::ggplotly(p, tooltip = "text")

}

plot_series_comparison <- function(df) {

  p <-
    ggplot(
      df,
      aes(
        year,
        hdi,
        group = name_muni_full,
        color = name_muni_full,
        text = paste(name_muni_full, "<br>Ano:", year, "<br>IDH:", hdi))
    ) +
    geom_line() +
    geom_point() +
    scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
    facet_wrap(vars(index_type)) +
    scale_color_manual(name = "", values = colors_div) +
    guides(color = "none") +
    labs(
      title = "Benchmark",
      x = NULL,
      y = "IDH (Firjan)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  plotly::ggplotly(p, tooltip = "text")

}
