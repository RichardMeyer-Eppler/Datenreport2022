#' Get data frame of funded projects
#'
#' @param path Character, path to csv file
#'
#' @return Data frame
#' @export
#'
#' @examples
get_funded_projects <- function(
  path
) {
  df_funded_projects <- readr::read_csv(
    file = path,
    col_types = readr::cols(
      report_nr = readr::col_double(),
      forderzeitraum_von = readr::col_datetime(
        format = ""
      ),
      forderzeitraum_bis = readr::col_datetime(
        format = ""
      ),
      programm = readr::col_character(),
      projekttitel = readr::col_character(),
      antragsteller_innen_verantwortliche_personen = readr::col_character()
    )
  )
    return(df_funded_projects)
}
