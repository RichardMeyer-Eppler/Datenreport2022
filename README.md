
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Datenreport2022

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/Datenreport2022)](https://CRAN.R-project.org/package=Datenreport2022)
<!-- badges: end -->

Datenreport2022 provides data wrangling functions transforming the
incoming raw data to the format expected by the [RUBer
package](https://github.com/RichardMeyer-Eppler/RUBer) for parametric
reporting. The data is for the data reports 2022 used in the ninth
reporting cycle for teaching at the Ruhr-University Bochum.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("RichardMeyer-Eppler/Datenreport2022")
#> Using github PAT from envvar GITHUB_PAT
#> Skipping install of 'Datenreport2022' from a github remote, the SHA1 (65bea768) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Usage

``` r
library(Datenreport2022)

# Load R Environment for file paths
readRenviron(file.path(path.expand("~"), ".Renviron"))

# Load data for figure type 1
df_t1 <- Datenreport2022::load_raw_csv(
  path = Sys.getenv("REPORT_T1")
)

# Load data for figure type 3
df_t3 <- Datenreport2022::load_raw_csv(
  path = Sys.getenv("REPORT_T3")
)

# Bind data together
df <- dplyr::bind_rows(
  df_t1,
  df_t3
)
```

``` r
# Add additional columns
df_wrangled <- df %>%
    Datenreport2022::add_figure_height(
      .
    ) %>%
    Datenreport2022::split_figures(
      .
    ) %>%
    dplyr::arrange(
      figure_count,
      aggregation_sort_1,
      facet,
      wert_sort
    ) %>%
    dplyr::select(
      report_nr,
      figure_count,
      x,
      y,
      y_label,
      fill,
      fill_label,
      fill_reverse,
      facet,
      group,
      group_label,
      source_caption,
      question_txt,
      figure_type_id,
      figure_caption,
      heading,
      subheading,
      is_heading,
      is_subheading,
      report_author,
      report_title,
      file_name,
      figure_filter_flag,
      figure_height
    )
```

``` r
report_ids <- sort(unique(dplyr::filter(df_wrangled, figure_type_id == 3L) %>% dplyr::pull(report_nr)))

purrr::pwalk(
  list(
    report_nr = report_ids
  ),
  RUBer::render_report,
  p_df = df_wrangled,
  rmd_template = system.file(
    "rmarkdown",
    "templates",
    "datenreport-2022",
    "skeleton",
    "skeleton.Rmd",
    package = "Datenreport2022"
  ),
  date = format(Sys.Date(), format = "%B %Y")
)
```
