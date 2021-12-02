#' Filters data frame based on report_nr and creates factors studiengang and
#' studienfachzaehler.
#'
#' @param df Data frame obtained by load_raw_studienfaelle
#' @param report_nr Report_nr to filter the data
#'
#' @return Filtered data frame with factor columns
#' @export
#'
#' @examples
get_stg_df <- function(
  df,
  report_nr
) {
  df_stg <- df %>%
    dplyr::filter(
      bericht_nr == report_nr
    ) %>%
    dplyr::mutate(
      faelle = 1,
      studiengang = forcats::as_factor(
        paste(
          fach_rub_ktxt,
          abschluss_ktxt
        )
      ),
      studienfachzaehler = factor(
        studienfachzaehler,
        levels = c(
          1,
          2
        ),
        labels = c(
          "1. Studienfach",
          "2. Studienfach"
        )
      )
    )

  return(df_stg)
}

