

# Libraries and functions -------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Calculate the verifying digit (7th digit) from the IBGE city code
last_digit_ibge <- Vectorize(function(code) {

  code <- strsplit(as.character(code), split = "", fixed = TRUE)
  code <- as.numeric(unlist(code))

  a <- code[1]
  b <- (code[2] * 2) %% 10 + (code[2] * 2) %/% 10
  c <- code[3]
  d <- (code[4] * 2) %% 10 + (code[4] * 2) %/% 10
  e <- code[5]
  f <- (code[6] * 2) %% 10 + (code[6] * 2) %/% 10
  digit <- (10 - (a + b + c + d + e + f) %% 10) %% 10
  return(as.character(digit))

})
# Wrapper function to get 7-digit city code from the 6-digit city code
get_ibge_code <- function(x) { as.numeric(paste0(x, last_digit_ibge(x))) }

# Wrapper function to properly import all tables
# Since all sheets share similar structure I only need to vary the path argument
import_hdi_sheet <- function(path) {

  df <- suppressWarnings(
    read_excel(
      path = path,
      skip = 3,
      col_names = cnames,
      col_types = ctypes,
      na = "*"
    )
  )
  # Remove missing values
  df <- dplyr::filter(df, !is.na(code_muni6))
  return(df)

}


# Import Data -------------------------------------------------------------

# Import city identifiers
id <- readr::read_csv("data/id_muni.csv")

# Clean IDs

# Select columns and simplify the city name (will serve as the key to join the
# tables)
subid <- id |>
  mutate(
    name_simplified = stringi::stri_trans_general(name_muni, id = "latin-ascii"),
    name_simplified = paste(abbrev_state, name_simplified),
    name_simplified = str_to_lower(name_simplified),
    name_simplified = str_replace_all(name_simplified, " ", "_")
  ) |>
  select(code_muni, name_simplified, name_state, code_state, name_muni_full)

# Import sheets

# Parametrs to import tables

# Paths to files and file names
file_names <- list.files("data-raw", pattern = "^Evolu")
path_names <- list.files("data-raw", pattern = "^Evolu", full.names = TRUE)

# Column names
id_names <- c("code_muni6", "name_region", "abbrev_state", "name_muni")
var_names <- paste0(rep(2005:2016, each = 2), c("_hdi", "_rank"))
cnames <- c(id_names, var_names)
# Column types
id_ctypes <- c("numeric", "text", "text", "text")
var_ctypes <- rep("numeric", length(var_names))
ctypes <- c(id_ctypes, var_ctypes)

# Import all sheets, name the list, and stack into a single tibble
sheets <- lapply(path_names, import_hdi_sheet)
names(sheets) <- c("education", "income", "overall", "health")
hdi_series <- dplyr::bind_rows(sheets, .id = "index_type")

# Clean data --------------------------------------------------------------

# Convert to column and split column names into 'year' and 'name_series'
hdi_series <- hdi_series |>
  pivot_longer(
    cols = starts_with("2"),
    names_sep = "_",
    names_to = c("year", "name_series")
    )

# Compute the 7-digit city code and join with identifiers
hdi_series <- hdi_series |>
  mutate(code_muni = get_ibge_code(code_muni6)) |>
  select(-code_muni6) |>
  left_join(subid, by = "code_muni")

# Convert to wide
tbl_hdi_series <- pivot_wider(
  hdi_series,
  names_from = "name_series",
  values_from = "value"
)

# Export
readr::write_csv(tbl_hdi_series, "data/firjan_series.csv")
