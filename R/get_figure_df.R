#' Prepare data frame for plotting of a particular figure by filtering and
#'    setting factors.
#'
#' @param df Data frame
#' @param figure_count Figure count of the figure
#'
#' @return Data frame
#' @export
#'
#' @examples
#' get_figure_df(df_fake, figure_count = 16)
get_figure_df <- function(df, figure_count) {
  figure_df <- dplyr::filter(
    df,
    figure_count == {{ figure_count }}
  )

  figure_df <- set_factors(figure_df)

  return(figure_df)
}

#' Type conversions for the 2022 data
#'
#' @param df Data frame
#'
#' @return Data frame with the factors y, fill, fill_label and group
#' @export
#'
#' @examples
#' \dontrun{
#' set_factors(df)
#' }
set_factors <- function(df) {

  if(df[[1, "figure_type_id"]] %in% c(1,4)) {

    df[["x"]] <- factor(
      df[["x"]],
      levels = unique(
        df[["x"]]
      )
    )

    df[["y"]] <- as.numeric(
      df[["y"]]
    )
  }

  if(df[[1, "figure_type_id"]] == 3) {

    df[["x"]] <- as.numeric(
      df[["x"]]
    )

    df[["y"]] <- factor(
      df[["y"]],
      levels = unique(
        df[["y"]]
      )
    )

    df[["y"]] <- forcats::fct_rev(
      df[["y"]]
    )
  }

  df[["fill"]] <- factor(
    df[["fill"]],
    levels = unique(
      df[["fill"]]
    )
  )

  df[["fill_label"]] <- factor(
    df[["fill_label"]],
    levels = unique(
      df[["fill_label"]]
    )
  )

  df[["group"]] <- factor(
    df[["group"]],
    levels = unique(
      df[["group"]]
    )
  )

  if(df[[1, "fill_reverse"]]) {

    df[["fill"]] <- forcats::fct_rev(
      df[["fill"]]
    )

    df[["fill_label"]] <- rev(
      df[["fill_label"]]
    )

    df[["group"]] <- rev(
      df[["group"]]
    )
  }

  return(df)
}
