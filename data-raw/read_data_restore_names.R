# Libraries ---------------------------------------------------------------

library(fs)

# File paths --------------------------------------------------------------

# Load R Environment for file paths
readRenviron(
  file.path(
    path.expand("~"),
    ".Renviron"
  )
)

# Restore names of AB data files -----------------------------------------------

dir_ab <- Sys.getenv("DIR_AB")
file_ab_num_source <- Sys.getenv("FILE_AB_NUM_SOURCE")
file_ab_num_target <- Sys.getenv("FILE_AB_NUM_TARGET")
file_ab_num_source <- Sys.getenv("FILE_AB_STR_SOURCE")
file_ab_num_target <- Sys.getenv("FILE_AB_STR_TARGET")

fs::file_move(
  path = paste0(
    dir_ab,
    file_ab_num_target
  ),
  new_path = paste0(
    dir_ab,
    file_ab_num_source
  )
)

fs::file_move(
  path = paste0(
    dir_ab,
    file_ab_num_target
  ),
  new_path = paste0(
    dir_ab,
    file_ab_num_source
  )
)

# Restore names of EBVB data files -----------------------------------------------
path_ebvb <- Sys.getenv("DIR_EBVB")
file_ebvb_num_source <- Sys.getenv("FILE_EBVB_NUM_SOURCE")
file_ebvb_num_target <- Sys.getenv("FILE_EBVB_NUM_TARGET")
file_ebvb_str_source <- Sys.getenv("FILE_EBVB_STR_SOURCE")
file_ebvb_str_target <- Sys.getenv("FILE_EBVB_STR_TARGET")

fs::file_move(
  path = paste0(
    path_ebvb,
    file_ebvb_num_target
  ),
  new_path = paste0(
    path_ebvb,
    file_ebvb_num_source
  )
)

fs::file_move(
  path = paste0(
    path_ebvb,
    file_ebvb_str_target
  ),
  new_path = paste0(
    path_ebvb,
    file_ebvb_str_source
  )
)
