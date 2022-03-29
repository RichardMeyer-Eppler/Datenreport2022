
# Libraries ---------------------------------------------------------------

library(dplyr)
library(fs)
library(janitor)
library(purrr)
library(readxl)
library(tibble)
library(tidyr)
library(vroom)

# File paths --------------------------------------------------------------

# Load R Environment for file paths
readRenviron(
  file.path(
    path.expand("~"),
    ".Renviron"
  )
)

path_ebvb <- Sys.getenv("DIR_EBVB")

paths <- list(
  ebvb = fs::dir_ls(
    path_ebvb
  )
)

# Read csv data ---------------------------------------------------------------

ebvb_paths <- 
  paths$ebvb %>%
  tibble::as_tibble_col(
    column_name = "path_from"
  ) %>% 
  dplyr::filter(
    # Nur xlsx-Dateien
    fs::path_ext(
      path_from
    ) == "xlsx",
    # Keine temporären Office-Dateien
    !stringr::str_detect(
      path_from,
      pattern = "~" 
    )
  ) %>%
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
ebvb_data <- ebvb_paths %>% 
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

ebvb_data$data

# Wrangle Data ------------------------------------------------------------

ebvb_data$data <- purrr::map(
  ebvb_data$data,
  janitor::clean_names
)


ebvb_data$data <- purrr::map(
  ebvb_data$data,
  dplyr::filter,
  !is.na(befragungstyp_id) &
    befragungstyp_id != "a Wert fehlt"
)

# Write Data --------------------------------------------------------------

purrr::walk2(
  ebvb_data$data,
  paste0(
    path_codebuch,
    ebvb_data$sheet,
    ".csv"
  ),
  vroom::vroom_write,
  delim = "|",
  eol = "\r\n",
  na = "",
  quote = c("all"),
  escape = c("double")
)

# Rename data files ---------------------------------------------------------

file_ebvb_num_source <- Sys.getenv("FILE_EBVB_NUM_SOURCE")
file_ebvb_num_target <- Sys.getenv("FILE_EBVB_NUM_TARGET")
file_ebvb_str_source <- Sys.getenv("FILE_EBVB_STR_SOURCE")
file_ebvb_str_target <- Sys.getenv("FILE_EBVB_STR_TARGET")

fs::file_move(
  path = paste0(
    path_ebvb,
    file_ebvb_num_source
  ),
  new_path = paste0(
    path_ebvb,
    file_ebvb_num_target
  )
)

fs::file_move(
  path = paste0(
    path_ebvb,
    file_ebvb_str_source
  ),
  new_path = paste0(
    path_ebvb,
    file_ebvb_str_target
  )
)