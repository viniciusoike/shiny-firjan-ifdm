prep_mapdata <- function(city, geo = "Estado") {

  if (geo == "Estado") {
    # Get the state abbreviation from the city name
    state <- dplyr::filter(id_muni, name_muni_full == city)$code_state
    # Filter the shape file
    shp <- dplyr::filter(firjan_full, code_state %in% state)
  } else if (geo == "Região") {
    # Get the state abbreviation from the city name
    region <- dplyr::filter(id_muni, name_muni_full == city)$code_region
    # Filter the shape file
    shp <- dplyr::filter(firjan_full, code_region %in% region)
  } else if (geo == "Brasil") {
    # Pass on the full objects
    shp <- firjan_full
  }

  return(shp)

}

get_state_border <- function(city, geo = "Região") {

  if (geo == "Região") {
    region <- dplyr::filter(id_muni, name_muni_full == city)$code_region
    state <- dplyr::filter(state_border, code_region == region)
  } else if (geo == "Brasil"){
    state <- state_border
  } else {
    state <- NULL
  }
  return(state)

}

get_map_variable <- function(year, variable) {

  vl <- c(
    "IDH" = "overall",
    "IDH - Educação" = "education",
    "IDH - Renda" = "income",
    "IDH - Saúde" = "health"
  )

  glue::glue("{vl[variable]}_{year}")
}

map_hdi <- function(
    shp = NULL,
    city = "Porto Alegre (RS)",
    year = 2023,
    variable = "IDH",
    title = variable,
    pal = "3 (Vermelho-Azul)",
    style = "Cluster",
    n = 6,
    geo = "Estado",
    border = NULL) {

  if (is.null(shp)) {
    shp <- prep_mapdata(city, geo)
  }

  coords_city <- dplyr::filter(cities, name_muni_full == city)
  coords_city <- c(coords_city$x, coords_city$y)

  fill_col <- get_map_variable(year, variable)

  popup_vars <- paste(c("overall", "education", "income", "health"), year, sep = "_")
  names(popup_vars) <- c(
    "IFDM", "IFDM - Educação", "IFDM - Emprego & Renda", "IFDM - Saúde"
  )

  # tmap v4: scale + legend are objects passed to tm_polygons(); the visual
  # variable is `fill`, outlines are `col`, opacity is `fill_alpha`.
  fill_scale <- tm_scale_intervals(
    style = unname(styles[style]),
    n = n,
    values = unname(pals[pal])
  )

  m <- tm_shape(shp) +
    tm_polygons(
      fill = fill_col,
      fill.scale = fill_scale,
      fill.legend = tm_legend(title = title),
      fill_alpha = 0.7,
      col = "gray50",
      lwd = 0.5,
      id = "name_muni",
      popup.vars = popup_vars,
      popup.format = list(digits = 3)
    )

  # Region/country comparisons overlay the bounding state/region borders.
  if (!is.null(border)) {
    m <- m +
      tm_shape(border) +
      tm_borders(col = "gray30")
  }

  m +
    tm_basemap(server = "CartoDB.Positron") +
    tm_view(set_view = c(coords_city, 10))
}
