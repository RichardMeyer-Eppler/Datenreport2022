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
        .x
      )
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
get_figure_height <- function(
  df
) {

  if(df[[1, "figure_type_id"]] == 1L)  {
    return(
      df %>%
        dplyr::mutate(
          figure_height_uncapped = 4.5,
          figure_height = 4.5
        )
    )
  }

  # browser()
  # Number of unique facet labels
  facet_count <- dplyr::n_distinct(
    df[["facet"]]
  )

  # Unique facet labels
  facet_unique <- unique(
    df[["facet"]]
  )

  # Gets maximum number of rows (labels with more than 80 characters)
  # over all unique facet labels
  facet_max_rows <- max(
    stringr::str_length(
      facet_unique
    )
  ) %/% 80

  # Counts number of unique y-Axis-Facet combinations
  y_facet_count <- dplyr::n_distinct(
    df %>%
      dplyr::mutate(
        y_facet = paste(
          y,
          facet,
          sep = "_"
        )
      ) %>%
      dplyr::pull(
        y_facet
      )
  )

  # Additional rows for the question text
  question_extra_rows <- stringr::str_length(
    unique(
      df[["question_txt"]]
    )
  ) %/% 70

  legend_unique <- as.character(unique(df[["fill_label"]]))

  legend_cols <- 1

  legend_cols <- RUBer::get_legend_columns(
    legend_text = legend_unique,
    y_axis_text = df[["y"]]
  )

  legend_rows <- ceiling(
    length(legend_unique) / legend_cols
  )

  if(df[[1, "figure_type_id"]] == 2)  {
    y_facet_count <- 5 * facet_count
  }

  # Je Facet_label Zeile 0.5 cm + 0.3 cm je Zeile
  # Je Fragetext Zeile 0.5 cm + 0.3 cm je Zeile
  # Je zusätzlicher Zeile Legende + 0.5 cm
  # inch to cm 2.54
  # cm to inch 0.393701

  height_calculated <- (y_facet_count * 1.3 * 0.393701) +
    (facet_count * 0.5 * 0.393701) +
    # If one facet has more than one row,
    # all facets get scaled to more than one row
    (facet_count * (facet_max_rows - 1) * 0.4 * 0.393701) +
    (question_extra_rows * 0.9 * 0.393701) +
    (legend_rows * 1.15 * 0.393701)

  df_plus_figure_height <- df %>%
    dplyr::mutate(
      # figure_caption = paste(
      #   figure_caption,
      #   round(height_calculated * 2.54, digits = 1)
      # ),
      figure_height_uncapped = height_calculated
    )

  upper_bound <- get_max_figure_height(
    figure_caption = df[[1, "figure_caption"]],
    heading = df[[1, "heading"]],
    subheading = df[[1, "subheading"]],
    is_heading = df[[1, "is_heading"]],
    is_subheading = df[[1, "is_subheading"]]
  )

  lower_bound <- 2
  delta <- 0

  if(length(upper_bound) == 0) {
    browser()
  }

  if (height_calculated > upper_bound) {
    delta <- height_calculated - upper_bound
    height_calculated <- upper_bound
  }

  else if (height_calculated < lower_bound) {
    height_calculated <- lower_bound
  }

  df_plus_figure_height <- df_plus_figure_height %>%
    dplyr::mutate(
      figure_height = height_calculated
    )

  return(df_plus_figure_height)
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

  rows_heading <- 0
  rows_subheading <- 0

  if(is_heading) {
    rows_heading <- ceiling(
      stringr::str_length(
        heading
      ) / 55
    )
  }

  if(is_subheading) {
    rows_subheading <- ceiling(
      stringr::str_length(
        subheading
      ) / 70
    )
  }

  rows_figure_caption <- ceiling(
    stringr::str_length(
      figure_caption
    ) / 70
  )

  df_params <- dplyr::bind_cols(
    rows_heading = rows_heading,
    rows_subheading = rows_subheading,
    rows_figure_caption = rows_figure_caption
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

  max_height_cm <- df_max_height %>%
    dplyr::inner_join(
      df_params,
      by = c(
        "rows_heading",
        "rows_subheading",
        "rows_figure_caption"
      )
    ) %>%
    dplyr::pull(
      max_height
    )

  max_height <- max_height_cm * 0.393701

  return(max_height)
}
