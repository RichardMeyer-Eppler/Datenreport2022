# Libraries ---------------------------------------------------------------

library(dplyr)
library(purrr)
library(readxl)
library(tibble)
library(tidyr)
library(vroom)

# Paths --------------------------------------------------------------

# Load R Environment for file paths
readRenviron(
  file.path(
    path.expand("~"),
    ".Renviron"
  )
)

in_folder <- Sys.getenv("DIR_INPUT_VORBEREITUNG")
out_folder <- Sys.getenv("DIR_INPUT_DWH")
in_file <- Sys.getenv("FILE_DWH")


# Read excel data ---------------------------------------------------------------

# Erzeuge Tabelle mit Dateipfaden inkl. der Datenblätter
dwh_paths <- tibble::as_tibble_col(
    paste0(
      in_folder,
      in_file
    ),
    column_name = "path_from"
  )  %>% 
  dplyr::mutate(
    # Einlesen der Datenblätter je xlsx-Datei
    sheet = purrr::map(
      path_from,
      readxl::excel_sheets
    ),
    path_from = setNames(
      path_from,
      nm = sheet
    ),
    .before = dplyr::everything()
  ) %>% 
  # Erzeugt für jedes Datenblatt eine eigene Zeile
  tidyr::unnest(
    cols = c(
      sheet
    )
  )

# Einlesen aller Datenblätter aller xlsx-Dateien
dwh_data <- dwh_paths %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    data = list(
      purrr::map2_dfr(
        path_from,
        sheet,
        readxl::read_xlsx,
        col_types = "text"
      )
    )
  ) %>% 
  dplyr::ungroup()

# dwh_data$data
# dwh_paths$sheet

# Write Data --------------------------------------------------------------

purrr::walk2(
  dwh_data$data,
  paste0(
    out_folder,
    dwh_paths$sheet,
    ".csv"
  ),
  vroom::vroom_write,
  delim = "|",
  eol = "\r\n",
  na = "",
  quote = c("all"),
  escape = c("double")
)
