#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
load_raw_csv <- function(
  path
) {
  df <- readr::read_csv(
    file = path,
    col_types = readr::cols(
      REPORT_NR = readr::col_integer(),
      REPORT_TYPE_ID = readr::col_character(),
      FIGURE_COUNT = readr::col_integer(),
      X = readr::col_character(),
      Y = readr::col_character(),
      Y_LABEL = readr::col_character(),
      FILL = readr::col_double(),
      FILL_LABEL = readr::col_character(),
      FILL_REVERSE = readr::col_logical(),
      FACET = readr::col_character(),
      GROUP = readr::col_double(),
      GROUP_LABEL = readr::col_character(),
      SOURCE_CAPTION = readr::col_character(),
      QUESTION_TXT = readr::col_character(),
      FIGURE_TYPE_ID = readr::col_integer(),
      FIGURE_CAPTION = readr::col_character(),
      HEADING = readr::col_character(),
      SUBHEADING = readr::col_character(),
      IS_HEADING = readr::col_logical(),
      IS_SUBHEADING = readr::col_logical(),
      REPORT_AUTHOR = readr::col_character(),
      REPORT_TITLE = readr::col_character(),
      FILE_NAME = readr::col_character(),
      FIGURE_FILTER_FLAG = readr::col_integer(),
      AGGREGATION_SORT_1 = readr::col_integer(),
      ABBILDUNG_MAP_SORT = readr::col_integer(),
      WERT_SORT = readr::col_integer()
    )
  ) %>%
    janitor::clean_names()

  return(df)
}

#' Loads csv file with information on cases
#'
#' @param path Path to csv file with raw data
#'
#' @return Data frame
#' @export
#'
#' @examples
load_raw_studienfaelle <- function(
  path
) {

  df_faelle <- readr::read_csv(
    file = path,
    col_types = readr::cols(
      BERICHT_NR = readr::col_integer(),
      BERICHT_TYP_ID = readr::col_character(),
      BEFRAGUNG_TYP_DTXT = readr::col_character(),
      ABSCHLUSS_ID = readr::col_character(),
      ABSCHLUSS_KTXT = readr::col_character(),
      ABSCHLUSS_DTXT = readr::col_character(),
      FACH_ID = readr::col_character(),
      FACH_RUB_KTXT = readr::col_character(),
      FACH_RUB_DTXT = readr::col_character(),
      STUDIENFACHZAEHLER = readr::col_integer()
    )
  ) %>%
    janitor::clean_names(
      .
    )

  return(df_faelle)
}

#' Loads csv file with information on response rates
#'
#' @param path Path to csv file with raw data
#'
#' @return Data frame
#' @export
#'
#' @examples
load_raw_response_rates <- function(
  path
) {
  df_response <- readr::read_csv(
    file = path,
    col_types = readr::cols(
      BEFRAGUNG_TYP_ID = readr::col_character(),
      FGR_NRWBUND_LTXT = readr::col_character(),
      ABSCHLUSS_DTXT = readr::col_character(),
      GESCHLECHT_LTXT = readr::col_character(),
      FACHSEMESTER = readr::col_integer(),
      KOEPFE = readr::col_double(),
      KOEPFE_RUECKLAUF = readr::col_double()
    )
  ) %>%
    janitor::clean_names()

  return(df_response)
}

#' Loads csv file with information on reports
#'
#' @param path Path to csv file with raw data
#'
#' @return Data frame
#' @export
#'
#' @examples
load_raw_reports <- function(
  path
) {
  df_reports <- readr::read_csv(
    file = path,
    col_types = readr::cols(
      REPORT_NR = readr::col_integer(),
      REPORT_TYPE_ID = readr::col_character(),
      REPORT_TITLE = readr::col_character(),
      REPORT_AUTHOR = readr::col_character(),
      FILE_NAME = readr::col_character(),
      SUBFOLDER = readr::col_character(),
      FAK_RUB_ID_3 = readr::col_character(),
      FAK_RUB_DTXT_3 = readr::col_character(),
      FGR_NRWBUND_ID = readr::col_character(),
      FGR_NRWBUND_LTXT = readr::col_character()
    )
  ) %>%
    janitor::clean_names()

  return(df_reports)
}

#' Load csv file on funded projects
#'
#' @param path Character, path to csv file
#'
#' @return Data frame
#' @export
#'
#' @examples
load_raw_funded_projects <- function(
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
