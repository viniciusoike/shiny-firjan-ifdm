prep_ranking <- function(city = "São Paulo (SP)", hdi_year, geo = "Estado") {

  if (geo == "Região") {

    region <- id_muni |>
      dplyr::filter(name_muni_full == city) |>
      dplyr::pull(name_region)

    df <- series_data |>
      dplyr::filter(name_region == region, year == hdi_year) |>
      dplyr::group_by(index_type) |>
      dplyr::mutate(rank = base::rank(-hdi)) |>
      dplyr::ungroup()
  }

  if (geo == "Estado") {
    state <- id_muni |>
      dplyr::filter(name_muni_full == city) |>
      dplyr::pull(code_state)

    df <- series_data |>
      dplyr::filter(code_state == state, year == hdi_year) |>
      dplyr::group_by(index_type) |>
      dplyr::mutate(rank = base::rank(-hdi)) |>
      dplyr::ungroup()
  }

  if (geo == "Brasil") {
    df <- series_data |>
      filter(year == hdi_year)
  }

  # Get lower and upper limits for each index (draw the lines)
  df_lines <- df |>
    dplyr::group_by(index_type) |>
    dplyr::summarise(
      xmin = min(rank, na.rm = TRUE),
      xmax = max(rank, na.rm = TRUE)
    )
  # Filter only the selected city (draw the squares)
  df_city <- df |>
    dplyr::filter(name_muni_full == city) |>
    dplyr::select(index_type, rank)

  return(list(limits = df_lines, ranking = df_city))

}

plot_ranking <- function(city, year, geo) {

  df <- prep_ranking(city, year, geo)

  xbreak = ifelse(geo == "Estado", 50, ifelse(geo == "Região", 100, 500))

  ggplot() +
    geom_segment(
      data = df$limits,
      aes(x = xmin, xend = xmax, y = index_type, yend = index_type)
    ) +
    geom_point(
      data = df$ranking,
      aes(x = rank, y = index_type),
      shape = 22,
      size = 3
    ) +
    geom_text(
      data = df$ranking,
      aes(x = rank, y = index_type, label = rank),
      size = 3,
      nudge_y = 0.15
    ) +
    labs(
      x = "Ranking",
      y = NULL,
      title = glue::glue("Ranking: {city}")
    ) +
    # scale_x_continuous(breaks = c(1, seq(50, round(max(df$limits$xmax), -2), by = xbreak))) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      panel.grid.major.x = element_line(
        linewidth = 0.25,
        linetype = 2,
        color = "gray80"
        )
    )
}
