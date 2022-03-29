library(Datenreport2022)
library(extrafont) # Required to correctly calculate graphics::strwidth()

# Load R Environment for file paths
readRenviron(
  file.path(".Renviron")
)

# File paths
folder <- Sys.getenv("DIR_GIT_DATA") # Sys.getenv("DIR_INPUT_DWH")
file <- Sys.getenv("FILE_DATENREPORT")


# Load raw csv data
df <- Datenreport2022::load_raw_csv(
  paste0(
    folder,
    file
  )
)

# Add height columns
df_height <- Datenreport2022::add_variable_and_static_height(
  df
)

# Update figure count column
df_figures <- Datenreport2022::update_figure_count(
  df_height
)

# Save data frame as Rda file
save(
  df_figures,
  file = paste0(
    Sys.getenv("DIR_INPUT_DWH"),
    Sys.getenv("FILE_FIGURES")
  )
)

# Copy from DWH folder to Git Data folder
fs::file_copy(
  path = paste0(
    Sys.getenv("DIR_INPUT_DWH"),
    Sys.getenv("FILE_FIGURES")
  ),
  new_path = paste0(
    Sys.getenv("DIR_GIT_DATA"),
    Sys.getenv("FILE_FIGURES")
  ),
  overwrite = TRUE
)

