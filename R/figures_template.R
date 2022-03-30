#' Get parameters for creating the template chunks plotting all figures
#'
#' @param df Data frame with columns figure_count
#'
#' @return Data frame with columns chunk_label, function_parameters and
#'         function_call
#' @export
#'
#' @examples
#' get_chunk_parameters(RUBer::df_example)
get_chunk_parameters <- function(
  df,
  function_call = "RUBer::plot_figure"
) {
  chunk_parameters <- df %>%
    dplyr::filter(
      !is.na(
        .data[["figure_count"]]
      )
    ) %>%
    dplyr::distinct(
      .data[["figure_count"]],
    ) %>%
    dplyr::mutate(
      chunk_label = .data[["figure_count"]],
      function_parameters = glue::glue(
        "df, figure_count = {figure_count}"
      ),
      function_call = glue::glue(
        "{function_call}({function_parameters})"
      )
    ) %>%
    dplyr::select(
      -.data[["figure_count"]]
    )

  return(chunk_parameters)
}

# RUBer::df_example %>%
#   dplyr::group_by(
#     report_nr,
#     figure_count,
#     is_heading,
#     is_subheading
#   ) %>%
#   dplyr::slice_head(
#     n = 1L
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(
#     report_nr,
#     figure_count,
#     is_heading,
#     is_subheading
#   ) %>% View()
