#' Add figure_height column to data frame
#'
#' @param df
#'
#' @return Data frame with additional column
#' @export
#'
#' @examples
#' add_figure_height(df)
add_figure_height <- function(df) {
  df_height <- df %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::group_modify(
      ~ get_figure_height(
        .x,
        .y
      )
    ) %>%
    dplyr::ungroup()

  return(df_height)
}

#' Add figure_height to data frame
#'
#' @param df
#' @param keys
#'
#' @return Data frame with additional column figure_height
#' @export
#'
#' @examples
get_figure_height <- function(
  df,
  keys
) {

  if(
    df[[1, "figure_type_id"]] %in% c(1L, 4L)
  ) {
    return(
      df %>%
        dplyr::mutate(
          figure_height = 4.5
        )
    )
  }

  if(
    df[[1, "figure_type_id"]] == 3L &
      is.na(df[[1, "question_txt"]]) &
      keys[[1, "report_nr"]] %in% c(72L, 74L)
  ) {
    return(
      df %>%
        dplyr::mutate(
          figure_height = 9
        )
    )
  }

  # Number of unique facet labels
  facet_count <- dplyr::n_distinct(
    df[["facet"]]
  )

  df_plus_figure_height <- df %>%
    dplyr::mutate(
      figure_height = static_height + (variable_height * facet_count)
    )

  return(df_plus_figure_height)
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
add_variable_and_static_height <- function(
  df
) {
  df_height <- df %>%
  dplyr::mutate(
    upper_bound = get_max_figure_height(
      figure_caption = figure_caption,
      heading = heading,
      subheading = subheading,
      is_heading = is_heading,
      is_subheading = is_subheading
    )
  ) %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::group_modify(
      ~ get_static_height(.x)
    ) %>%
    dplyr::group_modify(
      ~ get_variable_height(.x)
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count,
      facet,
      upper_bound
    ) %>%
    dplyr::mutate(
      first_row_within_facet = dplyr::row_number() == 1L,
      variable_height_first_row = dplyr::if_else(
        first_row_within_facet,
        variable_height,
        0
      ),
      binding_upper_bound = (upper_bound - static_height)
    ) %>%
    dplyr::group_by(
      report_nr,
      figure_count
    ) %>%
    dplyr::arrange(
      report_nr,
      figure_count,
      abbildung_map_sort,
      aggregation_sort_1,
      wert_sort
    ) %>%
    dplyr::mutate(
      cumsum_variable_height = cumsum(variable_height_first_row)
    ) %>%
    dplyr::ungroup()

  return(df_height)
}

#' Add figure_height to data frame
#'
#' @param df
#'
#' @return Data frame with additional column figure_height
#' @export
#'
#' @examples
get_variable_height <- function(
  df
) {

  # Aktuell nur für ims_t3.csv exportiert, nicht aber für bef_t3.csv
  df <- df %>%
    dplyr::mutate(
      aggregation_id_1 = dplyr::coalesce(
        aggregation_id_1,
        as.character(aggregation_sort_1)
    )
  )

  calculate_variable_height <- function(
    rows_within_facet,
    integer_division_facet_length,
    binding_upper_bound
  ) {

    variable_height <- (0.5 +
      rows_within_facet * 1.3 +
      (integer_division_facet_length - 1) * 0.4
      ) * 0.393701


    variable_height <- dplyr::coalesce(
      variable_height,
      0
    )

    variable_height_capped <- pmin(
      binding_upper_bound,
      variable_height,
      na.rm = TRUE
    )

    return(variable_height_capped)
  }

  df_height_per_facet <- df %>%
  dplyr::group_by(
    facet,
    aggregation_id_1
  )  %>%
  dplyr::filter(
    dplyr::row_number() == 1L
  ) %>%
  dplyr::group_by(
    facet
  ) %>%
  dplyr::mutate(
    rows_within_facet = dplyr::n()
  ) %>%
  dplyr::group_by(
    aggregation_id_1,
    .add = TRUE
  ) %>%
  dplyr::slice_head(
    n = 1
  ) %>%
  dplyr::mutate(
    facet_length = stringr::str_length(
      facet
    ),
    cum_max_facet_length = cummax(
      facet_length
    ),
    integer_division_facet_length = cum_max_facet_length %/% 80,
    variable_height = calculate_variable_height(
      rows_within_facet,
      integer_division_facet_length,
      binding_upper_bound = (upper_bound - static_height)
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    facet,
    aggregation_id_1,
    variable_height
  )

  df_variable_height <- df %>%
    dplyr::left_join(
      df_height_per_facet,
      by = c(
        "facet",
        "aggregation_id_1"
      )
    )

  # Unique facet labels
  # facet_unique <- unique(
  #   df[["facet"]]
  # )

  # Gets maximum number of rows (labels with more than 80 characters)
  # over all unique facet labels
  # facet_max_rows <- max(
  #   stringr::str_length(
  #     facet_unique
  #   )
  # ) %/% 80

  # Counts number of unique values on the y-axis
  # y-values have n count specific to each facet, so helper column is used
  # y_count <- dplyr::n_distinct(
  #   df[["aggregation_id_1"]]
  # )

  # Basishöhe von 0.5 cm je Facet
  # Jede zusätzliche Zeile auf der y-Achse 1.3 cm
  # Falls das Facet Label mehr als eine Zeile hat, zusätzlich 0.4 cm je Zeile
  # cm to inch 0.393701
  # variable_height <- (0.5 +
  #   y_count * 1.3 +
  #   (facet_max_rows - 1) * 0.4
  #   ) * 0.393701

  # df_variable_height <- df %>%
  #   dplyr::mutate(
  #     variable_height = variable_height
  #   )

  return(df_variable_height)
}

#' Add figure_height to data frame
#'
#' @param df
#'
#' @return Data frame with additional column figure_height
#' @export
#'
#' @examples
get_static_height <- function(
  df
) {

  # Additional rows for the question text
  question_extra_rows <- stringr::str_length(
    unique(
      df[["question_txt"]]
    )
  ) %/% 70

  if(
    is.na(
      question_extra_rows
    )
  ) {
    question_extra_rows <- 0
  }

  legend_unique <- as.character(
    unique(
      df[["fill_label"]]
    )
  )

  if(
    is.na(
      legend_unique[1]
    )
  ) {
    legend_unique <- as.character(
      unique(
        df[["fill"]]
      )
    )
  }

  legend_cols <- RUBer::get_legend_columns(
    legend_text = legend_unique,
    y_axis_text = df[["y"]]
  )

  legend_rows <- ceiling(
    length(legend_unique) / legend_cols
  )

  # Jede zusätzliche Zeile Fragetext 0.9 cm
  # Jede Zeile Legendentext 1.15 cm
  # Abbildungsbeschriftung / Quellenangabe 1.1 cm
  # cm to inch 0.393701
  static_height <- (question_extra_rows * 0.9 +
    legend_rows * 1 +
    1.1
    ) * 0.393701

  df_static_height <- df %>%
    dplyr::mutate(
      static_height = static_height
    )

  return(df_static_height)
}


#' Get max figure height
#'
#' @param figure_caption Text
#' @param heading Text
#' @param subheading Text
#' @param is_heading Boolean
#' @param is_subheading Boolean
#'
#' @return Max figure height in inches
#' @export
#'
#' @examples
#' get_max_figure_height(
#'   figure_caption = "Beurteilung der Studienangebote und -bedingungen - Struktur des Studiums (Bachelor)",
#'   heading = NA_character_,
#'   is_heading = FALSE,
#'   subheading = "Beurteilung von Studium und Lehre Beurteilung von Studium und Lehre asdfs",
#'   is_subheading = TRUE
#' )
get_max_figure_height <- function(
  figure_caption,
  heading,
  is_heading,
  subheading,
  is_subheading
) {

  # https://stackoverflow.com/questions/4042413/vectorized-if-statement-in-r
  rows_heading <- ifelse(
    is_heading,
    ceiling(
      stringr::str_length(
        heading
      ) / 55
    ),
    0
  )

  rows_subheading <- ifelse(
    is_subheading,
    ceiling(
      stringr::str_length(
        subheading
      ) / 70
    ),
    0
  )

  rows_figure_caption <- ceiling(
    stringr::str_length(
      figure_caption
    ) / 70
  )

  df_params <- tibble::tibble(
    rows_heading,
    rows_subheading,
    rows_figure_caption
  )

  df_max_height <- tibble::tribble(
    ~rows_heading, ~rows_subheading, ~rows_figure_caption, ~max_height,
    0L,               0L,                   1L,        24.38,
    0L,               0L,                   2L,         23.9,
    0L,               0L,                   3L,         23.3,
    0L,               0L,                   4L,         22.8,
    0L,               0L,                   5L,         22.3,
    1L,               0L,                   1L,         23.5,
    1L,               0L,                   2L,          23,
    2L,               0L,                   1L,         22.9,
    2L,               0L,                   2L,         22.4,
    1L,               1L,                   1L,         22.5,
    1L,               1L,                   2L,          22,
    0L,               1L,                   1L,         23.8,
    0L,               1L,                   2L,         23.2,
    0L,               2L,                   1L,         23.1,
    0L,               2L,                   2L,         22.6
  )

  max_height_cm <- df_params %>%
    dplyr::inner_join(
      df_max_height,
      by = c(
        "rows_heading",
        "rows_subheading",
        "rows_figure_caption"
      )
    ) %>%
    dplyr::pull(
      max_height
    )

  ## Inner Join above should not change the number of rows. If it does,
  # df_max_height does not have all required rows.
  if(
    length(max_height_cm) != length(figure_caption)
  ) {
    rlang::abort(
      message = rlang::format_error_bullets(
          c(
          x = "Parameter values found in data have no prespecified match in df_max_height. Please update function body of get_max_figure_height with appropriate mappings."
        )
      )
    )
  }

  max_height <- max_height_cm * 0.393701

  return(max_height)
}
