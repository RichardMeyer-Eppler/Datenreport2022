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
      .data[["is_heading"]],
      .data[["is_subheading"]],
      .data[["heading"]],
      .data[["subheading"]],
      .data[["figure_caption"]],
      .data[["figure_height"]],
    ) %>%
    dplyr::mutate(
      chunk_label = .data[["figure_count"]],
      function_parameters = glue::glue(
        "df, figure_count = {figure_count}"
      ),
      function_call = glue::glue(
        "{function_call}({function_parameters})"
      ),
      .before = .data[["is_heading"]]
    ) %>%
    dplyr::select(
      -.data[["figure_count"]]
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      chunk_figure_df = list(
        tpl_get_figure_df(
          chunk_label = chunk_label,
          function_params = function_parameters
        )
      )
      # ,
      # chunk_heading = dplyr::if_else(
      #   is_heading,
      #   Datenreport2022::tpl_heading(),
      #   NA_character_
      # ),
      # chunk_subheading = dplyr::if_else(
      #   is_subheading,
      #   Datenreport2022::tpl_subheading(),
      #   NA_character_
      # ),
      # chunk_plot_figure = Datenreport2022::tpl_plot_figure()
    ) %>%
    dplyr::ungroup()

  return(chunk_parameters)
}

#
# Datenreport2022::tpl_get_figure_df()
# Datenreport2022::tpl_heading()
# Datenreport2022::tpl_subheading()
# Datenreport2022::tpl_plot_figure()
#
#
# debugonce(tpl_get_figure_df)
#
# test <- get_chunk_parameters(
#   RUBer::df_example %>%
#     dplyr::filter(
#       report_nr == 1L
#     )
#   )
# test
#
# test$chunk_figure_df
#
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
