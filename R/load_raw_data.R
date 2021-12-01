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
      REPORT_NR = readr::col_double(),
      FIGURE_COUNT = readr::col_double(),
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
      FIGURE_FILTER_FLAG = readr::col_double(),
      AGGREGATION_SORT_1 = readr::col_double(),
      ABBILDUNG_MAP_SORT = readr::col_double(),
      WERT_SORT = readr::col_double()
    )
  ) %>%
    janitor::clean_names()

  return(df)
}
