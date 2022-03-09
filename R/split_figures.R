#' Update figure count
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
update_figure_count <- function(
  df
  ) {
  df_updated <- df %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::group_modify(
      ~ split_figures(.x)
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      shard
    ) %>%
    dplyr::mutate(
      first_row_within_split = dplyr::row_number() == 1L,
      figure_count_addend = dplyr::if_else(
        first_row_within_split &
          shard > 0,
        1,
        0
      )
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
    ) %>%
    dplyr::mutate(
      figure_caption = dplyr::if_else(
        split_flag &
          figure_type_id == 3,
        glue::glue(
          "{figure_caption} ({shard + 1}/{max_shard + 1})"
        ),
        figure_caption
      ),
      is_heading = dplyr::if_else(
        figure_count_addend > 0,
        FALSE,
        is_heading
      ),
      is_subheading = dplyr::if_else(
        figure_count_addend > 0,
        FALSE,
        is_subheading
      )
    ) %>%
    dplyr::group_by(
      report_nr
    ) %>%
    dplyr::mutate(
      figure_count = figure_count + cumsum(figure_count_addend)
    ) %>%
    dplyr::ungroup()

  return(df_updated)
}


#' Split figures
#'
#' @param df
#' @param p_shard
#' @param p_offset
#'
#' @return
#' @export
#'
#' @examples
split_figures <- function(
  df,
  p_shard = 0L,
  p_offset = 0L
) {

  # Add helper columns on first iteration
  if(
    p_shard == 0L
  ) {
    df <- df %>%
      dplyr::mutate(
        shard = NA_integer_,
        figure_height = NA_real_,
        split_flag = FALSE,
        max_shard = NA_integer_
      )
  }

  # Return data frame if figure_type_id == 1L (hard coded values, no splitting)
  if(
    1L %in% unique(df[["figure_type_id"]])
  ) {
    df <- df %>%
      dplyr::mutate(
        shard = 0,
        figure_height = 4,
        split_flag = FALSE,
        max_shard = 0,

      )
    return(df)
  }

  # Return data frame if figure_type_id == 2L (hard coded values)
  if(
    2L %in% unique(df[["figure_type_id"]])
  ) {

    degree_groups <- unique(df[["abbildung_map_sort"]])

    df <- df %>%
      dplyr::mutate(
        shard = dplyr::dense_rank(
          abbildung_map_sort
        ) - 1,
        split_flag = dplyr::dense_rank(
          abbildung_map_sort
        ) != 0,
        max_shard = length(degree_groups),
      ) %>%
      dplyr::group_by(
        shard
      ) %>%
      dplyr::mutate(
        figure_height = 4 + (
          # Every additional facet only 80% of original height of 4
          (
            dplyr::n_distinct(
              aggregation_id_1
            ) - 1
          ) * 3.2
        )
      ) %>%
      dplyr::ungroup()

    return(df)
  }

  # Stop iterating once shard column has values for all rows
  if(
    !anyNA(
      df[["shard"]]
    )
  ) {
    df_split_flag <- df %>%
      dplyr::mutate(
        split_flag = p_shard != 1L,
        max_shard = max(shard)
      )

    return(df_split_flag)
  } else {
    df_shard <- df %>%
      dplyr::mutate(
        shard = dplyr::if_else(
          (
            cumsum_variable_height < (binding_upper_bound + p_offset) |
              near(cumsum_variable_height, (binding_upper_bound + p_offset))
          ) &
            is.na(shard),
          p_shard,
          shard
        )
      )

    offset <- df_shard %>%
      dplyr::filter(
        !is.na(shard)
      ) %>%
      dplyr::summarize(
        offset = max(cumsum_variable_height)
      ) %>%
      dplyr::pull(
        offset
      )

    # Debug code
    # if(is.infinite(offset)) {
    #   browser()
    # }
    #
    # if(p_shard == 15L) {
    #   browser()
    # }

    df_shard_offset <- df_shard %>%
      dplyr::mutate(
        figure_height = dplyr::if_else(
          shard == p_shard,
          offset - p_offset + static_height,
          figure_height
        )
      )

    split_figures(df_shard_offset, p_shard + 1L, offset)
  }
}
