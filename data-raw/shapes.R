library(geobr)

shp <- read_municipality(year = 2020)

str_to_title2 <- function(string) {

  x <- stringr::str_to_title(string)
  pat <- c("Da", "Do", "Dos", "Das", "De", "E")
  pat <- stringr::str_c("( ", pat, " )")
  pat <- paste(pat, collapse = "|")
  x <- stringr::str_replace_all(x, pat, tolower)
  return(x)

}

shp <- shp |>
  dplyr::mutate(
    name_muni = str_to_title2(name_muni),
    name_muni_full = stringr::str_glue("{name_muni} ({abbrev_state})"),
    name_state = stringr::str_replace(name_state, "Amazônas", "Amazonas")
    )

id_muni <- sf::st_drop_geometry(shp)

sf::st_write(shp, "data/shape_muni.gpkg", append = FALSE)
readr::write_csv(id_muni, "data/id_muni.csv")

# Download state borders shapefile
state_borders <- read_state()
sf::st_write(state_borders, "data/shape_state_border.gpkg", append = FALSE)
