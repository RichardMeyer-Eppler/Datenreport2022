library(dplyr)
library(janitor)
library(readxl)
library(stringr)


# Load R Environment for file paths
readRenviron(
  file.path(
    path.expand("~"),
    ".Renviron"
  )
)

path_programme <- Sys.getenv("DIR_INPUT_DWH")
file_programme <- Sys.getenv("FILE_PROGRAMS")
path_out <- Sys.getenv("DIR_SKELETON")
file_out <- Sys.getenv("FILE_FUNDED_PROJECTS")

df_programme <- readxl::read_xlsx(
  path = paste0(
    path_programme,
    file_programme
  ),
  col_types = c(
    "date",
    "date",
    "text",
    "text",
    "text",
    "text",
    "text"
  )
) %>% 
  janitor::clean_names(
    .
  ) %>% 
  dplyr::mutate(
    projekttitel = stringr::str_replace_all(
      string = projekttitel,
      pattern = "\\n",
      replacement = " "
    ),
    projekttitel = stringr::str_replace_all(
      string = projekttitel,
      pattern = "\\r",
      replacement = " "
    ),
    projekttitel = stringr::str_squish(
      projekttitel
    )
)

# df_programme %>% 
#   dplyr::filter(
#     stringr::str_detect(
#       projekttitel,
#       pattern = "\\r"
#     )


df_bericht <- Datenreport2022::load_raw_reports(
  path = paste0(
    Sys.getenv("DIR_INPUT_DWH"),
    Sys.getenv("FILE_REPORTS")
  )
) %>% Datenreport2022::get_report_df(
  .
) %>% 
  dplyr::select(
    report_nr,
    report_type_id,
    fak_rub_id_3
  )

report_nr_m_ed <- RUBer::get_report_nr_by_id(
  df_bericht,
  "M_ED"
)

report_nr_fgr <- RUBer::get_report_nr_by_id(
  df_bericht,
  "FGR"
)

df_programs <- df_bericht %>% 
  dplyr::left_join(
    df_programme,
    by = c(
      "fak_rub_id_3" = "fak_rub_id_3"
    )
  ) %>%
  dplyr::filter(
    !report_type_id %in% c(
      "M_ED", "FGR"
    )
  ) %>%
  dplyr::bind_rows(
    df_programme %>%
      dplyr::mutate(
        report_nr = report_nr_fgr,
        .before = dplyr::everything()
      )
  ) %>%
  dplyr::bind_rows(
    df_programme %>%
      dplyr::mutate(
        report_nr = report_nr_m_ed,
        .before = dplyr::everything()
      ) %>%
      dplyr::filter(
        fak_rub_id_3 %in% c(
          01, 02, 03, 04, 05, 08, 09, 10, 15, 16, 17, 18, 19
        )
      )
  ) %>%
  dplyr::select(
    -(
      dplyr::starts_with(
        "fak"
      )
    )
  ) %>%
  dplyr::filter(
    !is.na(
      programm
    ) &
      !is.na(
        projekttitel
      )
  ) %>% 
  dplyr::select(
    report_nr,
    programm,
    projekttitel,
    antragsteller_innen_verantwortliche_personen,
    forderzeitraum_von,
    forderzeitraum_bis
  ) %>%
  dplyr::distinct() %>%
  dplyr::arrange(
    report_nr,
    programm,
    projekttitel,
    antragsteller_innen_verantwortliche_personen
  )

readr::write_csv(
  df_programs,
  file = paste0(
    path_out,
    file_out
  )
)