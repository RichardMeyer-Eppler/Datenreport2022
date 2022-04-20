#' Replace German umlauts with ASCII escape sequence for unicode characters
#'
#' @param path Character, path to file
#'
#' @return Side effects
#' @export
#'
#' @examples
replace_umlauts <- function(
  path
) {
  text <- readLines(
    con = path,
    encoding = "UTF-8"
  ) %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "Ä"
      ),
      replace = "\\u00C4"
    )  %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "ä"
      ),
      replace = "\\u00E4"
    )  %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "Ü"
      ),
      replace = "\\u00DC"
    )  %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "ü"
      ),
      replace = "\\u00FC"
    )  %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "Ö"
      ),
      replace = "\\u00D6"
    ) %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "ö"
      ),
      replace = "\\u00F6"
    ) %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "’"
      ),
      replace = "\\u2019"
    ) %>%
    stringr::str_replace_all(
      pattern = stringr::fixed(
        "–"
      ),
      replace = "\\u2013"
    )

    writeLines(
      text,
      con = path
    )
}
