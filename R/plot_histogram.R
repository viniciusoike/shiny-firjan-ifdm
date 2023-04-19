prep_histogram <- function(city, year_sel, geo) {

  if (geo == "Estado") {
    # Get the state abbreviation from the city name
    state_name <- stringr::str_extract(city, "(?<=\\()[A-Z]{2}(?=\\))")

    # Get only cities within the state and relevant columns
    df <- series_data |>
      dplyr::filter(abbrev_state == state_name, year == year_sel) |>
      dplyr::select(name_muni_full, index_type, hdi)
  }

  if (geo == "Região") {
    region <- id_muni |>
      dplyr::filter(name_muni_full == city) |>
      dplyr::pull(name_region)
    # Get only cities within the region and relevant columns
    df <- series_data |>
      dplyr::filter(name_region == region, year == year_sel) |>
      dplyr::select(name_muni_full, index_type, hdi)
  }

  if (geo == "Brasil") {
    # Filter only by year
    df <- series_data |>
      dplyr::filter(year == year_sel) |>
      dplyr::select(name_muni_full, index_type, hdi)
  }

  return(df)

}

plot_histogram <- function(city = "São Paulo (SP)", year_sel = 2016, geo = "Estado") {

  df <- prep_histogram(city, year_sel, geo)

  ggplot(df, aes(x = hdi)) +
    geom_histogram(
      aes(fill = index_type),
      color = "white",
      binwidth = 0.05) +
    geom_hline(yintercept = 0) +
    geom_vline(
      data = filter(df, name_muni_full == city),
      aes(xintercept = hdi),
      linetype = 2,
      linewidth = 0.4,
      color = "black"
    ) +
    scale_x_continuous(breaks = seq(0.1, 1, 0.1)) +
    scale_fill_manual(values = colors_qual) +
    guides(fill = "none") +
    facet_wrap(vars(index_type)) +
    labs(
      title = glue::glue("Distribuição do IDH ({year_sel})"),
      subtitle = "Histogramas com distribuição do IDH.\nLinha traçejada indica a posição do município selecionado.",
      x = "IDH",
      y = "Contagem"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

}
