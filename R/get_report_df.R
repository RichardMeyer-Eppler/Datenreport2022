#' Cleans up the report data
#'
#' @param df Data frame obtained by load_raw_reports
#'
#' @return Data frame
#' @export
#'
#' @examples
get_report_df <- function(
  df
) {
  df_report <- df %>%
    dplyr::mutate(
      file_name = stringi::stri_trans_general(
        file_name,
        "de-ASCII; Latin-ASCII"
      ),
      output_path = here::here(
        "output",
        dplyr::coalesce(
          subfolder,
          ""
        )
      )
    )

  return(df_report)
}
