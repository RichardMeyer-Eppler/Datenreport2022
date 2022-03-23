#' Transforms response rates to long format, creates factors
#'
#' @param df Data frame obtained by `load_raw_response_rates`
#'
#' @return Data frame
#' @export
#'
#' @examples
wrangle_response_rates <- function(
  df
) {
  df_factor <- df %>%
    dplyr::mutate(
      fachsemester = glue::glue(
        "{fachsemester}. Fachsemester"
      ),
      status = "Angeschrieben",
      dplyr::across(
        where(
          is.character
        ),
        forcats::as_factor
      )
    )

  df_response_long <- df_factor %>%
    dplyr::bind_rows(
      df_factor %>%
        dplyr::filter(
          dplyr::near(
            koepfe_ruecklauf,
            1
          )
        ) %>%
        dplyr::mutate(
          status = "G\u00FCltige Frageb\u00F6gen"
        )
    )

  return(df_response_long)
}

#' Creates gt object of response rates for a given `survey_type_id`
#'
#' @param df Data frame obtained by `wrangle_response_rates`
#' @inheritParams get_response_rate_data
#'
#' @return Gt object
#' @export
#'
#' @examples
response_rate_gt <- function(
  df,
  survey_type_id
) {

  if(survey_type_id != "AB") {
    survey_participants <- "Studierende"
  } else {
    survey_participants <- "Absolvent:innen"
  }

  gtsummary::theme_gtsummary_language(
    language = "de",
    decimal.mark = ",",
    big.mark = "."
  )

  df_filtered <- df %>%
    dplyr::filter(
      befragung_typ_id == survey_type_id
    ) %>%
    dplyr::select(
      geschlecht_ltxt,
      abschluss_dtxt,
      fgr_nrwbund_ltxt,
      fachsemester,
      status,
      koepfe
    )

  summarize_response_rates <- function(
    df,
    survey_participants
  ) {
    df %>%
      gtsummary::tbl_summary(
        by = status,
        label = list(
          geschlecht_ltxt ~ "Geschlecht",
          abschluss_dtxt ~ "Abschlussart",
          fgr_nrwbund_ltxt ~ "F\u00E4chergruppe (erstes Studienfach)",
          koepfe ~ glue::glue(
            "Gesamtzahl angeschriebene {survey_participants} / g\u00FCltige Frageb\u00F6gen"
          )
        )
      )
  }

  if(
    survey_type_id %in% c(
      "EB", "AB"
    )
  ) {
    gt <- df_filtered %>%
      dplyr::select(
        -fachsemester
      ) %>%
      summarize_response_rates(
        survey_participants = survey_participants
      )
  } else {
    gt <- df_filtered %>%
      gtsummary::tbl_strata(
        strata = fachsemester,
        .tbl_fun =
          ~ .x %>%
          summarize_response_rates(
            survey_participants = survey_participants
          )
      )
  }

  return(gt)
}

#' Converts gt object to ft object and extracts data frame
#'
#' @param gt Gt object
#' @inheritParams get_response_rate_data
#'
#' @return Data frame
#' @export
#'
#' @examples
extract_ft_data_from_gt <- function(
  gt,
  label,
  col_index,
  into_list
) {


  ft <- gt %>%
    gtsummary::as_flex_table()

  ncol <- ncol(ft$body$dataset)
  label_lower <- tolower(label)

  ft_data <- ft$body$dataset %>%
    dplyr::mutate(
      dplyr::across(
        2:ncol,
        ~ stringr::str_remove_all(
          .x,
          "\\)"
        )
      )
    )

  df_reduced <- purrr::reduce2(
    .x = col_index,
    .y = into_list,
    .f = tidyr::separate,
    sep = "\\(",
    .init = ft_data
  )

  df <- df_reduced %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ stringr::str_trim(
          .x
        )
      ),
      dplyr::across(
        2:tidyselect::last_col(),
        ~ dplyr::recode(
          .x,
          "0" = NA_character_,
          "0%" = NA_character_
        )
      )
    ) %>%
    dplyr::select(
      {{ label_lower }} := label,
      dplyr::everything()
    )

  return(df)
}

#' Adds summary row with overall response rates to data frame
#'
#' @param df Data frame obtained by `extract_ft_data_from_gt`
#' @inheritParams get_response_rate_data
#'
#' @return Data frame
#' @export
#'
#' @examples
add_response_rate <- function(
  df,
  label
) {

  label_lower <- tolower(label)

  if(label != "Studienabschluss") {
    response_rate_text <- glue::glue(
      "R\u00FCcklaufquote {label}sbefragungen"
    )
  } else {
    response_rate_text <- glue::glue(
      "R\u00FCcklaufquote Absolvent:innenbefragungen"
    )
  }

  calculate_response_rate <- function(
    df,
    label,
    dividend_col,
    divisor_col
  ) {

    dividend_col_sym <- rlang::sym(dividend_col)
    divisor_col_sym <- rlang::sym(divisor_col)

    response_rate <- df %>%
      dplyr::slice_tail(
        n = 1L
      ) %>%
      dplyr::mutate(
        response_rate = readr::parse_number(
          x = !!dividend_col_sym,
          locale = readr::locale(
            grouping_mark  = ".",
            decimal_mark  = ","
          )
        ) /
          readr::parse_number(
            x = !!divisor_col_sym,
            locale = readr::locale(
              grouping_mark  = ".",
              decimal_mark  = ","
            )
          ),
        response_rate = format(
          scales::label_percent()(
            response_rate
          )
        )
      ) %>%
      dplyr::pull(
        response_rate
      )

    return(response_rate)
  }

  # https://stackoverflow.com/questions/48833807/initialize-an-empty-tibble-with-column-names-and-0-rows
  df_response_rate <- names(df) %>%
    purrr::map_dfc(
      setNames,
      object = list(
        character()
      )
    ) %>%
    tibble::add_row() %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::everything(),
        ~ calculate_response_rate(
          df = df,
          label = label,
          dividend_col = names(df)[4],
          divisor_col = names(df)[2]
        )
      ),
      {{ label_lower }} := response_rate_text,
      dplyr::across(
        tidyselect::contains(
          "5fs"
        ),
        ~ calculate_response_rate(
          df = df,
          label = "Studienverlauf",
          divisor_col = names(df)[6],
          dividend_col = names(df)[8]
        )
      )
    )

  df_bound <- df %>%
    dplyr::bind_rows(
      df_response_rate
    ) %>%
    dplyr::mutate(
      row_id = dplyr::row_number()
    )

  return(df_bound)
}

#' Get data on response rates for a given `survey_type_id`
#'
#' @param survey_type_id One of `c("AB", "EB", "VB")`
#' @param label Label for that survey type
#' @param col_index Integer vector of gt stat columns to split
#' @param into_list List of gt stat column names for each split
#' @inheritParams load_raw_response_rates
#'
#' @return Data frame
#' @export
#'
#' @examples
get_response_rate_data <- function (
  path,
  survey_type_id,
  label,
  col_index = c(
    2L,
    4L
  ),
  into_list = list(
    c(
      "koepfe_rub",
      "koepfe_rub_perc"
    ),
    c(
      "koepfe_bef",
      "koepfe_bef_perc"
    )
  )
) {

  df_response <- load_raw_response_rates(
    path = path
  ) %>%
    wrangle_response_rates()

  gt <- df_response %>%
    response_rate_gt(
      df = .,
      survey_type_id = survey_type_id
    )

  df <- gt %>%
    extract_ft_data_from_gt(
      label = label,
      col_index = col_index,
      into_list = into_list
    ) %>%
    add_response_rate(
      label = label
    )

  return(df)
}
