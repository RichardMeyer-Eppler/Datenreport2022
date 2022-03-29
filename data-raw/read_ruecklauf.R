
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

folder <- Sys.getenv("DIR_RUECKLAEUFE")
file <- Sys.getenv("FILE_RUECKLAEUFE")
sheet <- "RUECKLAUF"

# Read Data --------------------------------------------------------------

df_ruecklauf <- readxl::read_xlsx(
  path = paste0(
    folder,
    file
  ),
  sheet = sheet,
  col_types = "text"
)

# Wrangle Data --------------------------------------------------------------

df_ruecklauf <- df_ruecklauf %>% 
  dplyr::select(
    befragung_typ_id = BEFRAGUNGSTYP_ID,
    semester = SEMESTER,
    geschlecht_id = Geschlecht,
    hochschulsemester = Hochschulsemester,
    fachsemester_1sf = `Fachsemester (1. Fach)`,
    fachsemester_2sf = `Fachsemester (2. Fach)`,
    abschluss_semester_id = Abschlusssemester,
    fach_id_nr_1sf = FACH1_ID_Nr,
    fach_id_nr_2sf = FACH2_ID_Nr,
    abschluss_id  = ABSCHLUSS_ID,
    faelle = GEANTWORTET,
    maximale_seite = 'AB Maximal besuchte Seite',
  )

df_list <- list(
  RUECKLAUF = df_ruecklauf
)

# names(df_list)

purrr::walk2(
  df_list,
  paste0(
    folder,
    names(df_list),
    ".csv"
  ),
  vroom::vroom_write,
  delim = "|",
  eol = "\r\n",
  na = "",
  quote = c("all"),
  escape = c("double")
)
