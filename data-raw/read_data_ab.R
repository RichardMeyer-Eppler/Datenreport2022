
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

dir_ab <- Sys.getenv("DIR_AB")

paths <- list(
  ab = fs::dir_ls(
    dir_ab
  )
)

# Read excel data ---------------------------------------------------------------

# Erzeuge Tabelle mit Dateipfaden inkl. der Datenblätter
ab_paths <- paths$ab %>%
  tibble::as_tibble_col(
    column_name = "path_from"
  ) %>% 
  dplyr::bind_rows(
    tibble::as_tibble_col(
      paths$ab_fragen,
      column_name = "path_from"
    )
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
ab_data <- ab_paths %>% 
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

# Wrangle Data ------------------------------------------------------------

ab_data[[
  which(
    ab_data$sheet == "VARIABLE_LABEL_AB"
  ), 
  "data"
]] <- list(
  ab_data %>% 
    dplyr::filter(
      sheet == "VARIABLE_LABEL_AB"
    ) %>% 
    tidyr::unnest(
      data
    ) %>% 
    dplyr::filter(
      !is.na(BEFRAGUNGSTYP_ID)
    ) %>% 
    dplyr::select(
      -sheet,
      -path_from
    )
)

ab_data[[
  which(
    ab_data$sheet == "WERT_AB"
  ), 
  "data"
]] <- list(
  ab_data %>% 
    dplyr::filter(
      sheet == "WERT_AB"
    ) %>% 
    tidyr::unnest(
      data
    ) %>% 
    dplyr::filter(
      Name != "a Wert fehlt"
    ) %>% 
    dplyr::select(
      -sheet,
      -path_from
    )
)

# ab_data$data

# Write Data --------------------------------------------------------------

purrr::walk2(
  ab_data$data,
  paste0(
    paths$folders$ab,
    ab_data$sheet,
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

file_ab_num_source <- Sys.getenv("FILE_AB_NUM_SOURCE")
file_ab_num_target <- Sys.getenv("FILE_AB_NUM_TARGET")
file_ab_num_source <- Sys.getenv("FILE_AB_STR_SOURCE")
file_ab_num_target <- Sys.getenv("FILE_AB_STR_TARGET")

fs::file_move(
  path = paste0(
    dir_ab,
    file_ab_num_source
  ),
  new_path = paste0(
    dir_ab,
    file_ab_num_target
  )
)

fs::file_move(
  path = paste0(
    dir_ab,
    file_ab_num_source
  ),
  new_path = paste0(
    dir_ab,
    file_ab_num_target
  )
)