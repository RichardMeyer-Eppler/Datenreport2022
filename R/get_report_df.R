#' Cleans up the report data
#'
#' @param df Data frame obtained by load_raw_reports
#' @param max_file_name_length Integer, maximum length of file names
#'
#' @return Data frame
#' @export
#'
#' @examples
get_report_df <- function(
  df,
  max_file_name_length = 75L
) {
  max_length_sans_ext <- max_file_name_length - 5L

  df_report <- df %>%
    dplyr::mutate(
      file_name_orig = file_name,
      file_name = stringi::stri_trans_general(
        file_name,
        "de-ASCII; Latin-ASCII"
      ),
      file_name = dplyr::if_else(
        fs::path_ext_remove(
          file_name
        ) > max_length_sans_ext,
        paste0(
          stringr::str_sub(
            fs::path_ext_remove(
              file_name
            ),
            end = max_length_sans_ext
          ),
          ".docx"
        ),
        file_name
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
