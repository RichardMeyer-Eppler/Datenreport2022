library(Datenreport2022)
library(dplyr)
library(purrr)
library(openxlsx)
library(RUBer)

# Load R Environment for file paths
readRenviron(
  file.path(path.expand("~"), ".Renviron")
)

load(
  paste0(
    Sys.getenv("DIR_INPUT_DWH"),
    Sys.getenv("FILE_FIGURES")
  )
)

all_list_items <- df_figures %>%
  dplyr::group_by(
    report_nr
  ) %>%
  dplyr::group_split(
    .keep = TRUE
  ) %>%
  purrr::map(
    .,
    Datenreport2022::get_item_df,
    cutoff = -1
  )

top8_list_items <- df_figures %>%
  dplyr::group_by(
    report_nr
  ) %>%
  dplyr::group_split(
    .keep = TRUE
  ) %>%
  purrr::map(
    .,
    Datenreport2022::get_item_df
  )

save(
  all_list_items,
  file = paste0(
    Sys.getenv("DIR_INPUT_DWH"),
    Sys.getenv("FILE_ITEMS_LIST")
  )
)

load(
  paste0(
    Sys.getenv("DIR_INPUT_DWH"),
    Sys.getenv("FILE_ITEMS_LIST")
  )
)

df_items <- purrr::map_dfr(
  all_list_items,
  dplyr::bind_rows
)

df_items_top_8 <- purrr::map_dfr(
  top8_list_items,
  dplyr::bind_rows
)

# RUBer::rub_table_item(
#   df_items
# )

df_report <- Datenreport2022::load_raw_reports(
  paste0(
    Sys.getenv("DIR_INPUT_DWH"),
    Sys.getenv("FILE_REPORTS")
  )
) %>% 
  Datenreport2022::get_report_df(
    .
  ) %>%
  dplyr::select(
    report_nr,
    report_author,
    fak_rub_dtxt_3,
    fgr_nrwbund_ltxt
  )

wrangle_data <- function(df){
  df_wrangled <- df %>%
    dplyr::left_join(
      df_report,
      by = c(
        "report_nr"
      )
    ) %>%
    dplyr::select(
      bericht_nr = report_nr,
      titel = report_author,
      fakultaet = fak_rub_dtxt_3,
      fgr_nrwbund = fgr_nrwbund_ltxt,
      abschluss = y,
      kapitel = heading,
      unterkapitel = subheading,
      abbildung = figure_caption,
      item = facet,
      mittelwert = mean,
      mittelwert_fgr = mean_fgr,
      abweichung_sd = distance
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(
          is.numeric
        ),
        round,
        digits = 2
      )
    )

  return(df_wrangled)
}

df_export_all <- wrangle_data(
  df_items
)

df_export_top_8 <- wrangle_data(
  df_items_top_8
)

# Export to Excel ---------------------------------------------------------

## Color codes
grey <- "#D7D7D7"
green_40 <- RUBer::get_RUB_colors("green_40")
blue <- RUBer::get_RUB_colors("blue")

## Header Styles
hs1 <- openxlsx::createStyle(
  fontColour = blue,
  fgFill = green_40,
  halign = "LEFT",
  textDecoration = "bold"
)

## Options for default styling (These are the defaults)
#options("openxlsx.borderColour" = grey)
#options("openxlsx.borderStyle" = "thin")
#options("openxlsx.borders " = "rows")
options("openxlsx.tableStyle" = "TableStyleLight1")
options("openxlsx.headerStyle" = hs1)

wb <- openxlsx::createWorkbook()

openxlsx::modifyBaseFont(
  wb,
  fontSize = 10,
  fontColour = blue,
  fontName = "Arial"
)

openxlsx::addWorksheet(
  wb,
  "top_8"
)

openxlsx::addWorksheet(
  wb,
  "alle_items"
)

openxlsx::writeDataTable(
  wb,
  "top_8",
  df_export_top_8
)


openxlsx::writeDataTable(
  wb,
  "alle_items",
  df_export_all
)

output_path <- paste0(
  Sys.getenv("DIR_INPUT_VORBEREITUNG"),
  Sys.getenv("FILE_ITEMS_GESAMTUEBERSICHT")
)

openxlsx::saveWorkbook(
  wb,
  output_path,
  overwrite = TRUE
)