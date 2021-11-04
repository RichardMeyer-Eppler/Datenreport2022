#' Splits up big figures
#'
#' @param df
#'
#' @return Df
#' @export
#'
#' @examples
#' split_figures(df)
split_figures <- function(df) {

  df <- df %>%
    dplyr::mutate(
      split_min = 0
    )

  split_figure <- function(df) {
    if(df[[1, "figure_height_uncapped"]] <= df[[1, "figure_height"]]) {
      return(df)
    }

    else {
      df_split <- df %>%
        dplyr::mutate(
          rnk = dplyr::dense_rank(facet),
          split = dplyr::if_else(
            rnk > median(rnk),
            1,
            0
          ),
          split_max = max(
            split
          ),
          figure_caption = glue::glue(
            "{figure_caption} ({split+1}/{split_max+1})"
          )
        ) %>%
        dplyr::arrange(
          split,
          aggregation_sort_1,
          facet,
          wert_sort
        ) %>%
        dplyr::group_by(
          split
        ) %>%
        dplyr::mutate(
          rn = dplyr::row_number(),
          is_min_row = dplyr::if_else(
            rn == min(rn),
            TRUE,
            FALSE
          ),
          split_min = dplyr::if_else(
            is_min_row,
            split,
            0
          ),
          is_heading = dplyr::if_else(
            is_min_row & split == 0,
            is_heading,
            FALSE
          ),
          is_subheading = dplyr::if_else(
            is_min_row & split == 0,
            is_subheading,
            FALSE
          )
        ) %>%
        dplyr::ungroup()

      return(df_split)
    }
  }

  df_split <- df %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::group_modify(
      ~ split_figure(
        .x
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      cumsum = cumsum(
        dplyr::coalesce(
          split_min,
          0
        )
      ),
      figure_count = figure_count + cumsum
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::group_modify(
      ~ get_figure_height(
        .x
      )
    ) %>%
    dplyr::ungroup()

  return(df_split)
}
