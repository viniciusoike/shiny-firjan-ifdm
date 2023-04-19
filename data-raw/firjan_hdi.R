library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

# Import Firjan HDI data from the Excel sheet
firjan <- read_excel(
  path = "data-raw/Ranking IFDM - Ordem de pontua__o.xlsx",
  range = "D11:I5575",
  col_names = c("abbrev_state", "name_muni", "idhm", "idhm_e", "idhm_r", "idhm_s"),
  na = "ND"
)

# Import city identifiers
id <- readr::read_csv("data/id_muni.csv")
shp <- sf::st_read("data/shape_muni.gpkg")

# Clean IDs

# Select columns and simplify the city name (will serve as the key to join the tables)
subid <- id |>
  mutate(
    name_simplified = stringi::stri_trans_general(name_muni, id = "latin-ascii"),
    name_simplified = paste(abbrev_state, name_simplified),
    name_simplified = str_to_lower(name_simplified),
    name_simplified = str_replace_all(name_simplified, " ", "_")
  ) |>
  select(code_muni, name_simplified, name_state, code_state, name_muni_full)

# Clean Firjan HDI

# OBS: Firjan includes a city called Augusto Severo (RN) which I was unable to find
firjan <- firjan |>
  mutate(
    # Fix some peculiar spelling problems from Firjan's database
    name_muni = case_when(
      abbrev_state == "SC" & name_muni == "Grão Pará" ~ "Grão-Pará",
      abbrev_state == "TO" & name_muni == "Fortaleza do Tabocão" ~ "Tabocão",
      abbrev_state == "SP" & name_muni == "Florínia" ~ "Florínea",
      abbrev_state == "MG" & name_muni == "São Thomé das Letras" ~ "São Tomé das Letras",
      abbrev_state == "SP" & name_muni == "Biritiba-Mirim" ~ "Biritiba Mirim",
      abbrev_state == "MG" & name_muni == "Dona Eusébia" ~ "Dona Euzébia",
      abbrev_state == "RN" & name_muni == "Olho-d'Água do Borges" ~ "Olho d'Água do Borges",
      abbrev_state == "MG" & name_muni == "Passa-Vinte" ~ "Passa Vinte",
      abbrev_state == "SP" & name_muni == "São Luís do Paraitinga" ~ "São Luíz do Paraitinga",
      abbrev_state == "BA" & name_muni == "Santa Teresinha" ~ "Santa Terezinha",
      abbrev_state == "BA" & name_muni == "Muquém de São Francisco" ~ "Muquém do São Francisco",
      TRUE ~ name_muni
    ),
    # Simplify city names to join with ids
    name_simplified = stringi::stri_trans_general(name_muni, id = "latin-ascii"),
    name_simplified = paste(abbrev_state, name_simplified),
    name_simplified = str_to_lower(name_simplified),
    name_simplified = str_replace_all(name_simplified, " ", "_")
  )

# Join with IDS, remove 2 cities that fail to match and rearrange column order
firjan <- firjan |>
  left_join(subid, by = "name_simplified") |>
  filter(!is.na(code_muni)) |>
  select(
    code_muni, name_muni, name_muni_full, name_simplified, code_state, abbrev_state,
    name_state, idhm, idhm_e, idhm_r, idhm_s)

# Rank cities

# Select only codes and IDH values and convert to long
tbl_rank <- firjan |>
  select(code_muni, code_state, starts_with("idh")) |>
  pivot_longer(
    cols = starts_with("idh"),
    names_to = "name_index",
    values_to = "index"
  )

# Compute overall rank and intra-state ranking
tbl_rank <- tbl_rank |>
  group_by(name_index) |>
  mutate(national = rank(-index, na.last = "keep")) |>
  group_by(name_index, code_state) |>
  mutate(state = rank(-index, na.last = "keep")) |>
  ungroup()

# Convert to wide (join with shape file)
tbl_rank <- tbl_rank |>
  pivot_wider(
    id_cols = "code_muni",
    names_from = "name_index",
    values_from = c("national", "state"),
    names_prefix = "rank_"
  )

# Join both absolute values and rankings to the shapefile

# First select only the absolute values from the Firjan table
tbl_idh <- firjan |>
  select(code_muni, idhm, idhm_e, idhm_r, idhm_s)
# Join with rankings
tbl_data <- left_join(tbl_idh, tbl_rank, by = "code_muni")
# Join with shape file
shp_firjan <- left_join(shp, tbl_data, by = "code_muni")

# Export
sf::st_write(shp_firjan, "data/firjan_hdi.gpkg")
readr::write_csv(sf::st_drop_geometry(shp_firjan), "data/firjan_hdi.csv")
