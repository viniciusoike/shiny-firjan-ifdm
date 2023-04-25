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
    shp,
    city = "Porto Alegre (RS)",
    year = 2010,
    variable = "IDH",
    title = variable,
    pal = "3 (Vermelho-Azul)",
    style = "Cluster",
    n = 6,
    geo = "Estado",
    border = NULL) {


  # Get the centroid of the selected city
  coords_city <- shp_hdi |>
    dplyr::filter(name_muni_full == city) |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.numeric()

  popup_vars <- paste(c("overall", "education", "income", "health"), year, sep = "_")
  names(popup_vars) <- c("IDH", "IDH - Educação", "IDH - Renda", "IDH - Saúde")

  # Map
  if (geo == "Estado") {
    tm_shape(shp) +
      tm_fill(
        col = get_map_variable(year, variable),
        style = styles[style],
        n = n,
        palette = pals[pal],
        alpha = 0.7,
        title = title,
        id = "name_muni",
        name = "name_muni",
        popup.vars = popup_vars,
        popup.format = list(digits = 3)
      ) +
      tm_borders(col = "gray50") +
      tm_basemap(server = "CartoDB.Positron") +
      tm_view(set.view = c(coords_city, 10))
  } else {
    tm_shape(shp) +
      tm_fill(
        col = get_map_variable(year, variable),
        style = styles[style],
        n = n,
        palette = pals[pal],
        alpha = 0.7,
        title = title,
        id = "name_muni",
        popup.vars = popup_vars,
        popup.format = list(digits = 3)
      ) +
      tm_borders(col = "gray50") +
      tm_shape(border) +
      tm_borders(col = "gray30") +
      tm_basemap(server = "CartoDB.Positron") +
      tm_view(set.view = c(coords_city, 10))
  }

}

# map_hdi <- function(shp, city, variable, title, pal = "Paleta 3", style = "Básico", n = 6) {
#
#   # Get the centroid of the selected city
#   coords_city <- map_data |>
#     dplyr::filter(name_muni_full == city) |>
#     sf::st_centroid() |>
#     sf::st_coordinates() |>
#     as.numeric()
#
#   # Map
#   tm_shape(shp) +
#     tm_fill(
#       col = vl[variable],
#       style = styles[style],
#       n = n,
#       palette = pals[pal],
#       alpha = 0.7,
#       title = title,
#       id = "name_muni",
#       popup.vars = vl,
#       popup.format = list(digits = 3)
#     ) +
#     tm_borders(col = "gray50") +
#     tm_basemap(server = "CartoDB.Positron") +
#     tm_view(set.view = c(coords_city, 10))
#
# }
