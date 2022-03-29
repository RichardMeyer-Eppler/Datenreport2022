library(Datenreport2022)
library(extrafont)
library(dplyr)

# Load R Environment for file paths
readRenviron(
  file.path(path.expand("~"), ".Renviron")
)

# File paths
folder <- Sys.getenv("DIR_INPUT_DWH")
file <- Sys.getenv("FILE_FIGURES")

# Base data ---------------------------------------------------------------

load(
  paste0(
    folder,
    file
  )
)

report_nr_m_ed <- RUBer::get_report_nr_by_id(
  df_figures,
  "M_ED"
)

report_nr_fgr <- RUBer::get_report_nr_by_id(
  df_figures,
  "FGR"
)

# Studiengänge ------------------------------------------------------------

anzahl_studiengaenge <- Datenreport2022::get_stg_df(
  Datenreport2022::load_raw_studienfaelle(
    path = paste0(
      Sys.getenv("DIR_INPUT_DWH"),
    #  Sys.getenv("DIR_GIT_DATA"),
      Sys.getenv("FILE_STUDENT_CASES")
    )
  ),
    report_nr_fgr
  ) %>%
  dplyr::pull(
    studiengang
  ) %>%
  dplyr::n_distinct(
    .
  )

# Berichte ------------------------------------------------------------

anzahl_berichte <- df_figures %>%
  dplyr::pull(
    report_nr
  ) %>%
  dplyr::n_distinct(
    .
  )

# Abbildungen ------------------------------------------------------------

anzahl_abbildungen <- df_figures %>%
  dplyr::distinct(
    report_nr,
    figure_count
  ) %>%
  nrow(
    .
  )

# Befragte ------------------------------------------------------------

anzahl_befragte <- Datenreport2022::get_stg_df(
  Datenreport2022::load_raw_studienfaelle(
    path = paste0(
      Sys.getenv("DIR_INPUT_DWH"),
      #Sys.getenv("DIR_GIT_DATA"),
      Sys.getenv("FILE_STUDENT_CASES")
    )
  ),
  report_nr_fgr
) %>%
  dplyr::group_by(
    befragung_typ_dtxt
  ) %>%
  dplyr::summarize(
    anzahl_befragte = sum(
      faelle
    )
  ) %>%
  dplyr::mutate(
    anzahl_befragte_dtxt = glue::glue(
      "{befragung_typ_dtxt} ({format(anzahl_befragte, big.mark = '.', decimal.mark = ',')} Fälle)"
    )
  )

# Fragen ------------------------------------------------------------

anzahl_fragen <- df_figures %>%
  dplyr::filter(
    !is.na(
      question_txt
    )
  ) %>%
  dplyr::pull(
    question_txt
  ) %>%
  dplyr::n_distinct(
    .
  )

# Items ------------------------------------------------------------

search_strings <- c(
  "[[:blank:]]\\(Bachelor 1-Fach\\)",
  "[[:blank:]]\\(Bachelor 2-Fächer\\)",
  "[[:blank:]]\\(Staatsexamen\\)",
  "[[:blank:]]\\(Master 1-Fach\\)",
  "[[:blank:]]\\(Master 2-Fächer\\)",
  "[[:blank:]]\\(Master of Education\\)",
  "[[:blank:]]\\(Magister Theologiae\\)"
)

patterns <- stringr::regex(
  paste0(
    search_strings,
    collapse = '|'
  ),
  ignore_case = TRUE
)

anzahl_items <- df_figures %>%
  dplyr::filter(
    figure_type_id == 3L,
    stringr::str_starts(
      string = .data$source_caption,
      pattern = "Informationsmanagement-System",
      negate = TRUE
    )
  ) %>%
  dplyr::mutate(
    facet_replaced = stringr::str_replace(
      string = facet,
      pattern = patterns,
      replacement = ""
    )
  ) %>%
  dplyr::pull(
    facet_replaced
  ) %>%
  dplyr::n_distinct(
    .
  )


# Antworten ---------------------------------------------------------------

anzahl_antworten <- df_figures %>%
  dplyr::filter(
    figure_type_id == 3L,
    !(report_nr %in% c(report_nr_fgr, report_nr_m_ed)),
    stringr::str_starts(
      string = .data$source_caption,
      pattern = "Informationsmanagement-System",
      negate = TRUE
    ),
    stringr::str_starts(
      string = .data$y,
      pattern = "FG ",
      negate = TRUE
    )
  ) %>%
  dplyr::summarize(
    anzahl_antworten = sum(
      as.integer(x)
    )
  ) %>%
  dplyr::pull(
    anzahl_antworten
  )

# Lehrförderungen ---------------------------------------------------------

foerderungen_path <- system.file(
  "rmarkdown",
  "templates",
  "datenreport-2022",
  "skeleton",
  "funded_projects.csv",
  package = "Datenreport2022"
)

df_foerderungen <- Datenreport2022::load_raw_funded_projects(
  path = foerderungen_path
)

anzahl_foerderungen <- nrow(
  df_foerderungen %>%
    dplyr::distinct(
      programm,
      projekttitel
    )
)

anzahl_antragssteller <- df_foerderungen %>%
  tidyr::separate_rows(
    antragsteller_innen_verantwortliche_personen,
    sep = ", "
  ) %>%
  dplyr::pull(
    antragsteller_innen_verantwortliche_personen
  ) %>%
  dplyr::n_distinct(
    .
  )

# Hochschulstatistik ------------------------------------------------------

df_studierende <- df_figures %>%
  dplyr::filter(
    figure_type_id == 1L,
    report_nr == report_nr_fgr,
    figure_count %in% c(1L),
    is.na(group)
  ) %>%
  dplyr::group_by(
    figure_count,
    fill_label
  ) %>%
  dplyr::slice_max(
    aggregation_sort_1,
    n = 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(
    fill_label,
    y
  )

df_absolvent_innen <- df_figures %>%
  dplyr::filter(
    figure_type_id == 1L,
    report_nr == report_nr_fgr,
    figure_count %in% c(2),
    is.na(group)
  ) %>%
  dplyr::group_by(
    figure_count,
    fill_label
  ) %>%
  dplyr::slice_max(
    aggregation_sort_1,
    n = 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(
    fill_label,
    y
  )

# Metrics -----------------------------------------------------------------

metrics <- tibble::lst(
  anzahl_abbildungen,
  anzahl_berichte,
  anzahl_fragen,
  anzahl_items,
  anzahl_studiengaenge,
  anzahl_befragte,
  anzahl_antworten,
  anzahl_foerderungen,
  anzahl_antragssteller,
  df_studierende,
  df_absolvent_innen
)

# metrics

# Metrics Text-------------------------------------------------------------

format_metric <- function(x) {
  format(
    x,
    big.mark = '.',
    decimal.mark = ','
  )
}

metrics_text <- c(
  glue::glue(
    "Insgesamt {format_metric(metrics$anzahl_berichte)} Datenreporte\n für {format_metric(metrics$anzahl_studiengaenge)} Studiengänge"
  ),
  glue::glue(
    "Drei Befragungen:\n{knitr::combine_words(metrics$anzahl_befragte$anzahl_befragte_dtxt, and = '\nund ', oxford_comma = FALSE)}"
  ),
  glue::glue(
    "Auswertung von {metrics$anzahl_fragen} Fragen\n und {metrics$anzahl_items} Items"
  ),
  glue::glue(
    "{format_metric(metrics$anzahl_antworten)} ausgewertete Antworten\nvisualisiert in {format_metric(metrics$anzahl_abbildungen)} Abbildungen"
  ),
  glue::glue(
    "{metrics$anzahl_foerderungen} Lehrförderungen verteilt auf\n {metrics$anzahl_antragssteller} Antragssteller:innen"
  ),
  glue::glue(
    "Hochschulstatistische Daten und Kohortenanalysen zu {format_metric(sum(as.numeric(metrics$df_studierende$y)))} Studienfällen und {format_metric(sum(as.numeric(metrics$df_absolvent_innen$y)))} Absolvent:innen"
  )
)

df_metrics <- tibble::tibble(
  metrics_text,
  metrics_images = c(
    "streamline-icon-pie-line-graph@48x48.png",
    "streamline-icon-team-meeting-message-men-question@48x48.png",
    "streamline-icon-list-numbers@48x48.png",
    "streamline-icon-analytics-bars-horizontal@48x48.png",
    "streamline-icon-user-female-teacher-math@48x48.png",
    "streamline-icon-people-man-graduate@48x48.png"
  ),
  image_attribution = c(
    '<a href="https://www.streamlinehq.com">Free Pie Line Graph PNG icon by Streamline</a>',
    '<a href="https://www.streamlinehq.com">Free Team Meeting Message Men Question PNG icon by Streamline</a>',
    '<a href="https://www.streamlinehq.com">Free List Numbers PNG icon by Streamline</a>',
    '<a href="https://www.streamlinehq.com">Free Analytics Bars Horizontal PNG icon by Streamline</a>',
    '<a href="https://www.streamlinehq.com">Free User Female Teacher Math PNG icon by Streamline</a>',
    '<a href="https://www.streamlinehq.com">Free People Man Graduate PNG icon by Streamline</a>'
  )
)

# df_metrics

readr::write_csv(
  df_metrics,
  file = Sys.getenv("FILE_METRICS")
)

fs::file_copy(
  path =  rep(
    Sys.getenv("FILE_METRICS"),
    2
  ),
  new_path = c(
    paste0(
      Sys.getenv("DIR_SKELETON"),
      Sys.getenv("FILE_METRICS")
    ),
    system.file(
      "rmarkdown",
      "templates",
      "datenreport-2022",
      "skeleton",
      Sys.getenv("FILE_METRICS"),
      package = "Datenreport2022"
    )
  ),
  overwrite = TRUE
)
