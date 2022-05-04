#' Get data frame of funded projects
#'
#' @param df Data frame retrieved by `load_raw_funded_projects`
#' @param report_nr Optional integer, result set will be filtered to these report numbers
#'
#' @return Data frame
#' @export
#'
#' @examples
get_funded_projects <- function(
  df,
  report_nr = NULL
) {
  p_report_nr <- report_nr

  if(!is.null(report_nr)) {
    df <- df %>%
      dplyr::filter(
        report_nr %in% p_report_nr
      )
  }

  if(nrow(df) == 0L) return(df)

  df_wrangled <- df %>%
    dplyr::mutate(
      rn = dplyr::row_number()
    ) %>%
    dplyr::group_by(
      report_nr,
      programm
    ) %>%
    dplyr::mutate(
      is_last_row = dplyr::row_number() == max(
        dplyr::row_number()
      )
    ) %>%
    dplyr::ungroup()

  return(df_wrangled)
}
