# Libraries ---------------------------------------------------------------

library(fs)

# Copy to Rmd-Skeleton folder in R Package -------------------------------------

# Load R Environment for file paths
readRenviron(
  file.path(path.expand("~"), ".Renviron")
)

folder_in <- Sys.getenv("DIR_INPUT_DWH")

files <- c(
  Sys.getenv("FILE_INCLUDED_PROGRAMS"),
  Sys.getenv("FILE_EXCLUDED_PROGRAMS"),
  Sys.getenv("FILE_RESPONSE_RATES")
)

folder_out <-  Sys.getenv("DIR_SKELETON")

fs::file_copy(
  path = paste0(
    folder_in,
    files
  ),
  new_path = paste0(
    folder_out,
    files
  ),
  overwrite = TRUE
)


# Copy to data folder -----------------------------------------------------

readRenviron(
  file.path(path.expand("~"), ".Renviron")
)

folder_in <- Sys.getenv("DIR_INPUT_DWH")

files <- c(
  Sys.getenv("FILE_DATENREPORT"),
  Sys.getenv("FILE_REPORTS"),
  Sys.getenv("FILE_STUDENT_CASES")
)

folder_out <- Sys.getenv("DIR_GIT_DATA")

fs::file_copy(
  path = paste0(
    folder_in,
    files
  ),
  new_path = paste0(
    folder_out,
    files
  ),
  overwrite = TRUE
)


# Copy Lehrfoerderungen --------------------------------------------------------

readRenviron(
  file.path(path.expand("~"), ".Renviron")
)

folder_in <- Sys.getenv("DIR_PROGRAMS")
folder_out <- Sys.getenv("DIR_GIT_DATA")
file <- Sys.getenv("FILE_PROGRAMS")

fs::file_copy(
  path = paste0(
      folder_in,
      file
  ),
  new_path = paste0(
    folder_out,
    file
  ),
  overwrite = TRUE
)
