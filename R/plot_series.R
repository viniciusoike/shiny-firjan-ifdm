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
        text = paste("Ano:", year, "<br>IFDM:", hdi, "<br>Indicador:", index_type))
      ) +
    geom_line(aes(group = index_type), linewidth = 0.7) +
    geom_point(size = 1.6) +
    scale_x_continuous(breaks = 2013:2023) +
    scale_y_continuous(breaks = seq(0, 1, 0.05)) +
    scale_color_manual(name = "", values = INDEX_PAL_LABELLED) +
    labs(
      x = NULL,
      y = "IFDM"
    ) +
    theme_ekio()

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
        text = paste(name_muni_full, "<br>Ano:", year, "<br>IFDM:", hdi))
    ) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.4) +
    scale_y_continuous(breaks = seq(0.1, 1, 0.1)) +
    facet_wrap(vars(index_type)) +
    scale_color_manual(name = "", values = BENCH_PAL) +
    guides(color = "none") +
    labs(
      x = NULL,
      y = "IFDM"
    ) +
    theme_ekio()

  plotly::ggplotly(p, tooltip = "text")

}
