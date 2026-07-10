# Map input$variable (an INDEX_CHOICES value) to the index_type factor label
# used in series_data.
VARIABLE_TO_FACTOR <- c(
  "IDH" = "Geral (IFDM)",
  "IDH - Educação" = "Educação",
  "IDH - Saúde" = "Saúde",
  "IDH - Renda" = "Emprego & Renda"
)

# Ranked municipalities for the selected index within the comparison geography
# (Estado / Região / Brasil). Feeds the DT next to the map.
prep_ranking_table <- function(city, hdi_year, variable, series_geo) {
  factor_lbl <- unname(VARIABLE_TO_FACTOR[variable])

  series_geo |>
    dplyr::filter(index_type == factor_lbl, year == hdi_year) |>
    dplyr::mutate(rank = as.integer(base::rank(-hdi, na.last = "keep", ties.method = "min"))) |>
    dplyr::arrange(rank) |>
    dplyr::transmute(
      rank,
      name_muni,
      hdi,
      is_city = name_muni_full == city
    )
}

prep_ranking <- function(city, hdi_year, series_geo) {
  df <- series_geo |>
    dplyr::filter(year == hdi_year) |>
    dplyr::group_by(index_type) |>
    dplyr::mutate(rank = base::rank(-hdi, ties.method = "min")) |>
    dplyr::ungroup()

  df_lines <- df |>
    dplyr::group_by(index_type) |>
    dplyr::summarise(
      xmin = min(rank, na.rm = TRUE),
      xmax = max(rank, na.rm = TRUE)
    )

  df_city <- df |>
    dplyr::filter(name_muni_full == city) |>
    dplyr::select(index_type, rank)

  list(limits = df_lines, ranking = df_city)
}

plot_ranking <- function(city, year, geo, series_geo) {
  df <- prep_ranking(city, year, series_geo)

  subtitle <- switch(
    geo,
    "Estado" = "Posição entre os municípios do estado",
    "Região" = "Posição entre os municípios da região",
    "Brasil" = "Posição no ranking nacional"
  )

  ggplot() +
    geom_segment(
      data = df$limits,
      aes(x = xmin, xend = xmax, y = index_type, yend = index_type),
      color = EKIO_GRID,
      linewidth = 1.1
    ) +
    geom_point(
      data = df$ranking,
      aes(x = rank, y = index_type, fill = index_type),
      shape = 22,
      size = 5,
      color = "white"
    ) +
    geom_text(
      data = df$ranking,
      aes(x = rank, y = index_type, label = paste0(rank, "º")),
      size = 4,
      color = EKIO_INK,
      nudge_y = 0.3
    ) +
    scale_x_reverse() +
    scale_fill_manual(values = INDEX_PAL_LABELLED) +
    guides(fill = "none") +
    labs(
      x = "Ranking (1 = melhor)",
      y = NULL,
      subtitle = subtitle
    ) +
    theme_ekio(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(
        linewidth = 0.5,
        linetype = 2,
        color = EKIO_GRID
      )
    )
}
