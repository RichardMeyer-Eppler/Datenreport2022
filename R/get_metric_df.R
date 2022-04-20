#' Title
#'
#' @param df Data frame obtained by load_raw_studienfaelle
#'
#' @return
#' @export
#'
#' @examples
get_metric_df <- function(
  df
) {

  metric_image_path <- function(file_name) {
    system.file(
      "rmarkdown",
      "templates",
      "datenreport-2022",
      "skeleton",
      file_name,
      package = "RUBer"
    )
  }

  metrics_images <- purrr::map_chr(
    df$metrics_images,
    metric_image_path
  )

  metrics_text <- df[["metrics_text"]]

  df_metrics <- tibble::tribble(
    ~col1, ~col2, ~col3, ~col4, ~col5, ~col6,
    metrics_images[1], metrics_text[1], metrics_text[1], metrics_text[1], NA_character_, NA_character_,
    metrics_text[2], metrics_text[2], metrics_text[2], metrics_text[2], metrics_text[2], metrics_images[2],
    metrics_images[3], metrics_text[3], metrics_text[3], metrics_text[3], NA_character_, NA_character_,
    NA_character_, NA_character_, metrics_text[4], metrics_text[4], metrics_text[4], metrics_images[4],
    metrics_images[5], metrics_text[5], metrics_text[5], metrics_text[5], metrics_text[5], NA_character_,
    NA_character_, metrics_text[6], metrics_text[6], metrics_text[6], metrics_text[6], metrics_images[6]
  )

  return(df_metrics)
}


