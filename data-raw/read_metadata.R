# Libraries ---------------------------------------------------------------

library(dplyr)
library(purrr)
library(readxl)
library(vroom)

# Paths --------------------------------------------------------------

# Load R Environment for file paths
readRenviron(
  file.path(
    path.expand("~"),
    ".Renviron"
  )
)

folder <- Sys.getenv("DIR_INPUT_VORBEREITUNG")
folder_out <- Sys.getenv("DIR_INPUT_DWH")
file <- Sys.getenv("FILE_METADATA")

sheets <- c(
  BERICHT_AGGREGATION = "BERICHT_AGGREGATION",
  BERICHT_GLIEDERUNG = "BERICHT_GLIEDERUNG",
  BERICHT_STUDIENGAENGE = "BERICHT_STUDIENGAENGE",
  ABBILDUNG = "ABBILDUNG",
  ABBILDUNG_MAP = "ABBILDUNG_MAP",
  FRAGE = "FRAGE"
)

# Read Data --------------------------------------------------------------

list_df <- purrr::map2(
  paste0(
    folder,
    rep(
      file,
      length(sheets)
    )
  ),
  sheets,
  readxl::read_xlsx,
  col_types = "text"
) %>%
  purrr::set_names(
    nm = names(sheets)
  )

# list_df
# sheets

# Write Data --------------------------------------------------------------

# names(list_df)

purrr::walk2(
  list_df,
  paste0(
    folder_out,
    names(list_df),
    ".csv"
  ),
  vroom::vroom_write,
  delim = "|",
  eol = "\r\n",
  na = "",
  quote = c("all"),
  escape = c("double")
)