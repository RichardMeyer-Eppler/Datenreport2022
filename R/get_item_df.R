#' Get data frame with survey items that strongly diverge from their reference group
#'
#' @param df Data frame with data for one report_nr
#' @param n_items Number of items to retrieve for each degree
#' @param cutoff Optional, when provided n_items is ignored. Keeps all rows where
#'    distance in standard deviation is equal to or larger than the cutoff.
#' @param excluded_reports Character vector containing values for report_type_id that should be
#'    excluded.
#'
#' @return Data frame
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
get_item_df <- function(
  df,
  n_items = 8L,
  cutoff = NULL,
  excluded_reports = c(
    "M_ED", "FGR", "SZMA"
  )
) {
  pattern <- "(?<=n\\=)[[:digit:]]{0,1}\\.?[[:digit:]]{0,3}"
  replace_pattern <- "[[:space:]]\\(.*\\)"


  df_filtered <- df %>%
    dplyr::filter(
      figure_type_id == 3L,
      # M Ed Bericht und Faechergruppenbericht
      !(
        report_type_id %in% excluded_reports
      ),
      stringr::str_starts(
        string = .data$source_caption,
        pattern = "Informationsmanagement-System",
        negate = TRUE
      )
    )

  if(
    nrow(
      df_filtered
    ) == 0L
  ) {
    return(df_filtered)
  }

  df_item <- df_filtered %>%
    dplyr::distinct(
      report_nr,
      report_type_id,
      figure_nr,
      figure_caption,
      heading,
      subheading,
      facet,
      x,
      y,
      fill,
      fill_label,
      aggregation_id_1,
      aggregation_sort_1
    ) %>%
    dplyr::mutate(
      x = as.integer(
        x
      ),
      n = readr::parse_number(
        stringr::str_extract(
          y,
          pattern
        ),
        locale = readr::locale(
          decimal_mark = ",",
          grouping_mark = "."
        )
      ),
      y = stringr::str_replace(
        y,
        replace_pattern,
        ""
      ),
      .after = x
    ) %>%
    # Alle Items mit 0 Fällen ausschließen
    dplyr::group_by(
      figure_nr,
      facet
    ) %>%
    dplyr::filter(
      !is.na(
        min(n)
      )
    ) %>%
    dplyr::ungroup() %>%
    tidyr::uncount(
      weights = x
    ) %>%
    dplyr::group_by(
      report_nr,
      report_type_id,
      figure_nr,
      figure_caption,
      heading,
      subheading,
      facet,
      y,
      aggregation_id_1,
      aggregation_sort_1
    ) %>%
    dplyr::summarize(
      mean = mean(
        fill
      ),
      sd = sd(
        fill
      )
    ) %>%
    dplyr::arrange(
      figure_nr,
      facet,
      aggregation_id_1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mean_fgr = dplyr::lead(
        mean
      ),
      sd_fgr = dplyr::lead(
        sd
      ),
      mean_delta = abs(
        mean - mean_fgr
      ),
      distance = mean_delta / sd_fgr,
      figure_caption = stringr::str_replace(
        string = figure_caption,
        pattern = "[[:blank:]]\\(.*\\)$",
        replacement = ""
      )
    ) %>%
    dplyr::filter(
      dplyr::row_number() %% 2 == 1L
    ) %>%
    dplyr::filter(
      # Berichte, wo Berichtsstudiengaenge = Faechergruppe
      mean != mean_fgr
    ) %>%
    dplyr::group_by(
      report_nr,
      aggregation_sort_1
    )

  if(
    is.null(cutoff)
  ) {
    return(
      df_item %>%
        dplyr::slice_max(
          order_by = distance,
          n = n_items,
          with_ties = TRUE
        ) %>%
          dplyr::ungroup()
    )
  } else {
    return(
      df_item %>%
        dplyr::filter(
          distance >= cutoff
        ) %>%
      dplyr::ungroup()
    )
  }

  return(df_item)
}
