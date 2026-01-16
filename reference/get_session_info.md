# Get Reproducibility Tables

Creating tables used at the end of reports, for reproducibility. Most of
the information is based off of sessioninfo::session_info()

## Usage

``` r
get_session_info(libpath = FALSE)
```

## Arguments

- libpath:

  Show R package library path column in packages table

## Value

list of length two, containing dataframe of Software Session Information
and dataframe of Software Package Version Information

## Details

Both tables usually printing with `kable()` at the end of a report.

If any loaded packages have a `DataVersion` field then the Software
Package Version Information will contain a `data.version` column.

Full Name is found in Windows via the `net` command, and via ldap search
in Linux and MACs. The ldap search will only work on SCHARPs network at
Fred Hutching Cancer Research Center. If there is an error attempting to
get the Full Name, the system usernam will be displayed instead.

## Examples

``` r
my_session_info <- get_session_info()

library(dplyr)

# Simple HTML Display
kableExtra::kable(my_session_info$platform_table, 'html',
      caption = "Reproducibility Software Session Information") |>
      kableExtra::kable_styling()
#> <table class="table" style="margin-left: auto; margin-right: auto;">
#> <caption>Reproducibility Software Session Information</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> name </th>
#>    <th style="text-align:left;"> value </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> nodename </td>
#>    <td style="text-align:left;"> runnervmmtnos </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> version </td>
#>    <td style="text-align:left;"> R version 4.5.2 (2025-10-31) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> os </td>
#>    <td style="text-align:left;"> Ubuntu 24.04.3 LTS </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> system </td>
#>    <td style="text-align:left;"> x86_64, linux-gnu </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ui </td>
#>    <td style="text-align:left;"> X11 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> language </td>
#>    <td style="text-align:left;"> en </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> collate </td>
#>    <td style="text-align:left;"> C </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ctype </td>
#>    <td style="text-align:left;"> C.UTF-8 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> tz </td>
#>    <td style="text-align:left;"> UTC </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> date </td>
#>    <td style="text-align:left;"> 2026-01-16 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> pandoc </td>
#>    <td style="text-align:left;"> 3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown) </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> quarto </td>
#>    <td style="text-align:left;"> NA </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> repo </td>
#>    <td style="text-align:left;"> https://github.com/FredHutch/VISCfunctions </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> file name </td>
#>    <td style="text-align:left;"> No Input File Detected </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> location </td>
#>    <td style="text-align:left;"> No Input File Location Detected </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> user </td>
#>    <td style="text-align:left;"> runner </td>
#>   </tr>
#> </tbody>
#> </table>

kableExtra::kable(my_session_info$packages_table, 'html',
      caption = "Reproducibility Software Package Version Information") |>
      kableExtra::kable_styling()
#> <table class="table" style="margin-left: auto; margin-right: auto;">
#> <caption>Reproducibility Software Package Version Information</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> package </th>
#>    <th style="text-align:left;"> version </th>
#>    <th style="text-align:left;"> date </th>
#>    <th style="text-align:left;"> source </th>
#>    <th style="text-align:left;"> status </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> dplyr </td>
#>    <td style="text-align:left;"> 1.1.4 </td>
#>    <td style="text-align:left;"> 2023-11-17 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> attached </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> VISCfunctions </td>
#>    <td style="text-align:left;"> 1.3.1 </td>
#>    <td style="text-align:left;"> 2026-01-16 </td>
#>    <td style="text-align:left;"> local </td>
#>    <td style="text-align:left;"> attached </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> askpass </td>
#>    <td style="text-align:left;"> 1.2.1 </td>
#>    <td style="text-align:left;"> 2024-10-04 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> binom </td>
#>    <td style="text-align:left;"> 1.1-1.1 </td>
#>    <td style="text-align:left;"> 2022-05-02 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> bslib </td>
#>    <td style="text-align:left;"> 0.9.0 </td>
#>    <td style="text-align:left;"> 2025-01-30 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> cachem </td>
#>    <td style="text-align:left;"> 1.1.0 </td>
#>    <td style="text-align:left;"> 2024-05-16 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> cli </td>
#>    <td style="text-align:left;"> 3.6.5 </td>
#>    <td style="text-align:left;"> 2025-04-23 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> codetools </td>
#>    <td style="text-align:left;"> 0.2-20 </td>
#>    <td style="text-align:left;"> 2024-03-31 </td>
#>    <td style="text-align:left;"> CRAN (R 4.5.2) </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> coin </td>
#>    <td style="text-align:left;"> 1.4-3 </td>
#>    <td style="text-align:left;"> 2023-09-27 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> curl </td>
#>    <td style="text-align:left;"> 7.0.0 </td>
#>    <td style="text-align:left;"> 2025-08-19 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> desc </td>
#>    <td style="text-align:left;"> 1.4.3 </td>
#>    <td style="text-align:left;"> 2023-12-10 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> digest </td>
#>    <td style="text-align:left;"> 0.6.39 </td>
#>    <td style="text-align:left;"> 2025-11-19 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> downlit </td>
#>    <td style="text-align:left;"> 0.4.5 </td>
#>    <td style="text-align:left;"> 2025-11-14 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> evaluate </td>
#>    <td style="text-align:left;"> 1.0.5 </td>
#>    <td style="text-align:left;"> 2025-08-27 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> fansi </td>
#>    <td style="text-align:left;"> 1.0.7 </td>
#>    <td style="text-align:left;"> 2025-11-19 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> farver </td>
#>    <td style="text-align:left;"> 2.1.2 </td>
#>    <td style="text-align:left;"> 2024-05-13 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> fastmap </td>
#>    <td style="text-align:left;"> 1.2.0 </td>
#>    <td style="text-align:left;"> 2024-05-15 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> fontawesome </td>
#>    <td style="text-align:left;"> 0.5.3 </td>
#>    <td style="text-align:left;"> 2024-11-16 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> fs </td>
#>    <td style="text-align:left;"> 1.6.6 </td>
#>    <td style="text-align:left;"> 2025-04-12 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> generics </td>
#>    <td style="text-align:left;"> 0.1.4 </td>
#>    <td style="text-align:left;"> 2025-05-09 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ggplot2 </td>
#>    <td style="text-align:left;"> 4.0.1 </td>
#>    <td style="text-align:left;"> 2025-11-14 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> glue </td>
#>    <td style="text-align:left;"> 1.8.0 </td>
#>    <td style="text-align:left;"> 2024-09-30 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> gtable </td>
#>    <td style="text-align:left;"> 0.3.6 </td>
#>    <td style="text-align:left;"> 2024-10-25 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> htmltools </td>
#>    <td style="text-align:left;"> 0.5.9 </td>
#>    <td style="text-align:left;"> 2025-12-04 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> httr2 </td>
#>    <td style="text-align:left;"> 1.2.2 </td>
#>    <td style="text-align:left;"> 2025-12-08 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> jquerylib </td>
#>    <td style="text-align:left;"> 0.1.4 </td>
#>    <td style="text-align:left;"> 2021-04-26 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> jsonlite </td>
#>    <td style="text-align:left;"> 2.0.0 </td>
#>    <td style="text-align:left;"> 2025-03-27 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> kableExtra </td>
#>    <td style="text-align:left;"> 1.4.0 </td>
#>    <td style="text-align:left;"> 2024-01-24 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> knitr </td>
#>    <td style="text-align:left;"> 1.51 </td>
#>    <td style="text-align:left;"> 2025-12-20 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> labeling </td>
#>    <td style="text-align:left;"> 0.4.3 </td>
#>    <td style="text-align:left;"> 2023-08-29 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> lattice </td>
#>    <td style="text-align:left;"> 0.22-7 </td>
#>    <td style="text-align:left;"> 2025-04-02 </td>
#>    <td style="text-align:left;"> CRAN (R 4.5.2) </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> libcoin </td>
#>    <td style="text-align:left;"> 1.0-10 </td>
#>    <td style="text-align:left;"> 2023-09-27 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> lifecycle </td>
#>    <td style="text-align:left;"> 1.0.5 </td>
#>    <td style="text-align:left;"> 2026-01-08 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> magrittr </td>
#>    <td style="text-align:left;"> 2.0.4 </td>
#>    <td style="text-align:left;"> 2025-09-12 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> MASS </td>
#>    <td style="text-align:left;"> 7.3-65 </td>
#>    <td style="text-align:left;"> 2025-02-28 </td>
#>    <td style="text-align:left;"> CRAN (R 4.5.2) </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> Matrix </td>
#>    <td style="text-align:left;"> 1.7-4 </td>
#>    <td style="text-align:left;"> 2025-08-28 </td>
#>    <td style="text-align:left;"> CRAN (R 4.5.2) </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> matrixStats </td>
#>    <td style="text-align:left;"> 1.5.0 </td>
#>    <td style="text-align:left;"> 2025-01-07 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> memoise </td>
#>    <td style="text-align:left;"> 2.0.1 </td>
#>    <td style="text-align:left;"> 2021-11-26 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> modeltools </td>
#>    <td style="text-align:left;"> 0.2-24 </td>
#>    <td style="text-align:left;"> 2025-05-02 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> multcomp </td>
#>    <td style="text-align:left;"> 1.4-29 </td>
#>    <td style="text-align:left;"> 2025-10-20 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> mvtnorm </td>
#>    <td style="text-align:left;"> 1.3-3 </td>
#>    <td style="text-align:left;"> 2025-01-10 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> openssl </td>
#>    <td style="text-align:left;"> 2.3.4 </td>
#>    <td style="text-align:left;"> 2025-09-30 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> pillar </td>
#>    <td style="text-align:left;"> 1.11.1 </td>
#>    <td style="text-align:left;"> 2025-09-17 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> pkgconfig </td>
#>    <td style="text-align:left;"> 2.0.3 </td>
#>    <td style="text-align:left;"> 2019-09-22 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> pkgdown </td>
#>    <td style="text-align:left;"> 2.2.0 </td>
#>    <td style="text-align:left;"> 2025-11-06 </td>
#>    <td style="text-align:left;"> any (@2.2.0) </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> purrr </td>
#>    <td style="text-align:left;"> 1.2.1 </td>
#>    <td style="text-align:left;"> 2026-01-09 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> R6 </td>
#>    <td style="text-align:left;"> 2.6.1 </td>
#>    <td style="text-align:left;"> 2025-02-15 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> ragg </td>
#>    <td style="text-align:left;"> 1.5.0 </td>
#>    <td style="text-align:left;"> 2025-09-02 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> rappdirs </td>
#>    <td style="text-align:left;"> 0.3.3 </td>
#>    <td style="text-align:left;"> 2021-01-31 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> RColorBrewer </td>
#>    <td style="text-align:left;"> 1.1-3 </td>
#>    <td style="text-align:left;"> 2022-04-03 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> rlang </td>
#>    <td style="text-align:left;"> 1.1.7 </td>
#>    <td style="text-align:left;"> 2026-01-09 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> rmarkdown </td>
#>    <td style="text-align:left;"> 2.30 </td>
#>    <td style="text-align:left;"> 2025-09-28 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> rstudioapi </td>
#>    <td style="text-align:left;"> 0.17.1 </td>
#>    <td style="text-align:left;"> 2024-10-22 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> S7 </td>
#>    <td style="text-align:left;"> 0.2.1 </td>
#>    <td style="text-align:left;"> 2025-11-14 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> sandwich </td>
#>    <td style="text-align:left;"> 3.1-1 </td>
#>    <td style="text-align:left;"> 2024-09-15 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> sass </td>
#>    <td style="text-align:left;"> 0.4.10 </td>
#>    <td style="text-align:left;"> 2025-04-11 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> scales </td>
#>    <td style="text-align:left;"> 1.4.0 </td>
#>    <td style="text-align:left;"> 2025-04-24 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> sessioninfo </td>
#>    <td style="text-align:left;"> 1.2.3 </td>
#>    <td style="text-align:left;"> 2025-02-05 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> stringi </td>
#>    <td style="text-align:left;"> 1.8.7 </td>
#>    <td style="text-align:left;"> 2025-03-27 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> stringr </td>
#>    <td style="text-align:left;"> 1.6.0 </td>
#>    <td style="text-align:left;"> 2025-11-04 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> survival </td>
#>    <td style="text-align:left;"> 3.8-3 </td>
#>    <td style="text-align:left;"> 2024-12-17 </td>
#>    <td style="text-align:left;"> CRAN (R 4.5.2) </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> svglite </td>
#>    <td style="text-align:left;"> 2.2.2 </td>
#>    <td style="text-align:left;"> 2025-10-21 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> systemfonts </td>
#>    <td style="text-align:left;"> 1.3.1 </td>
#>    <td style="text-align:left;"> 2025-10-01 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> textshaping </td>
#>    <td style="text-align:left;"> 1.0.4 </td>
#>    <td style="text-align:left;"> 2025-10-10 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> TH.data </td>
#>    <td style="text-align:left;"> 1.1-5 </td>
#>    <td style="text-align:left;"> 2025-11-17 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> tibble </td>
#>    <td style="text-align:left;"> 3.3.1 </td>
#>    <td style="text-align:left;"> 2026-01-11 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> tidyr </td>
#>    <td style="text-align:left;"> 1.3.2 </td>
#>    <td style="text-align:left;"> 2025-12-19 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> tidyselect </td>
#>    <td style="text-align:left;"> 1.2.1 </td>
#>    <td style="text-align:left;"> 2024-03-11 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> utf8 </td>
#>    <td style="text-align:left;"> 1.2.6 </td>
#>    <td style="text-align:left;"> 2025-06-08 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> vctrs </td>
#>    <td style="text-align:left;"> 0.6.5 </td>
#>    <td style="text-align:left;"> 2023-12-01 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> viridisLite </td>
#>    <td style="text-align:left;"> 0.4.2 </td>
#>    <td style="text-align:left;"> 2023-05-02 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> whisker </td>
#>    <td style="text-align:left;"> 0.4.1 </td>
#>    <td style="text-align:left;"> 2022-12-05 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> withr </td>
#>    <td style="text-align:left;"> 3.0.2 </td>
#>    <td style="text-align:left;"> 2024-10-28 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> xfun </td>
#>    <td style="text-align:left;"> 0.55 </td>
#>    <td style="text-align:left;"> 2025-12-16 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> xml2 </td>
#>    <td style="text-align:left;"> 1.5.1 </td>
#>    <td style="text-align:left;"> 2025-12-01 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> yaml </td>
#>    <td style="text-align:left;"> 2.3.12 </td>
#>    <td style="text-align:left;"> 2025-12-10 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> zoo </td>
#>    <td style="text-align:left;"> 1.8-15 </td>
#>    <td style="text-align:left;"> 2025-12-15 </td>
#>    <td style="text-align:left;"> RSPM </td>
#>    <td style="text-align:left;"> loaded </td>
#>   </tr>
#> </tbody>
#> </table>


# Latex Display
kableExtra::kable(my_session_info$platform_table, 'latex', booktabs = TRUE,
      linesep = '', caption = "Reproducibility Software Session Information") |>
      kableExtra::kable_styling(font_size = 7)
#> \begin{table}
#> \centering
#> \caption{Reproducibility Software Session Information}
#> \centering
#> \fontsize{7}{9}\selectfont
#> \begin{tabular}[t]{ll}
#> \toprule
#> name & value\\
#> \midrule
#> nodename & runnervmmtnos\\
#> version & R version 4.5.2 (2025-10-31)\\
#> os & Ubuntu 24.04.3 LTS\\
#> system & x86\_64, linux-gnu\\
#> ui & X11\\
#> language & en\\
#> collate & C\\
#> ctype & C.UTF-8\\
#> tz & UTC\\
#> date & 2026-01-16\\
#> pandoc & 3.1.11 @ /opt/hostedtoolcache/pandoc/3.1.11/x64/ (via rmarkdown)\\
#> quarto & NA\\
#> repo & https://github.com/FredHutch/VISCfunctions\\
#> file name & No Input File Detected\\
#> location & No Input File Location Detected\\
#> user & runner\\
#> \bottomrule
#> \end{tabular}
#> \end{table}

kableExtra::kable(my_session_info$packages_table, 'latex', booktabs = TRUE,
      linesep = '', caption = "Reproducibility Software Package Version Information") |>
      kableExtra::kable_styling(font_size = 7)
#> \begin{table}
#> \centering
#> \caption{Reproducibility Software Package Version Information}
#> \centering
#> \fontsize{7}{9}\selectfont
#> \begin{tabular}[t]{lllll}
#> \toprule
#> package & version & date & source & status\\
#> \midrule
#> dplyr & 1.1.4 & 2023-11-17 & RSPM & attached\\
#> VISCfunctions & 1.3.1 & 2026-01-16 & local & attached\\
#> askpass & 1.2.1 & 2024-10-04 & RSPM & loaded\\
#> binom & 1.1-1.1 & 2022-05-02 & RSPM & loaded\\
#> bslib & 0.9.0 & 2025-01-30 & RSPM & loaded\\
#> cachem & 1.1.0 & 2024-05-16 & RSPM & loaded\\
#> cli & 3.6.5 & 2025-04-23 & RSPM & loaded\\
#> codetools & 0.2-20 & 2024-03-31 & CRAN (R 4.5.2) & loaded\\
#> coin & 1.4-3 & 2023-09-27 & RSPM & loaded\\
#> curl & 7.0.0 & 2025-08-19 & RSPM & loaded\\
#> desc & 1.4.3 & 2023-12-10 & RSPM & loaded\\
#> digest & 0.6.39 & 2025-11-19 & RSPM & loaded\\
#> downlit & 0.4.5 & 2025-11-14 & RSPM & loaded\\
#> evaluate & 1.0.5 & 2025-08-27 & RSPM & loaded\\
#> fansi & 1.0.7 & 2025-11-19 & RSPM & loaded\\
#> farver & 2.1.2 & 2024-05-13 & RSPM & loaded\\
#> fastmap & 1.2.0 & 2024-05-15 & RSPM & loaded\\
#> fontawesome & 0.5.3 & 2024-11-16 & RSPM & loaded\\
#> fs & 1.6.6 & 2025-04-12 & RSPM & loaded\\
#> generics & 0.1.4 & 2025-05-09 & RSPM & loaded\\
#> ggplot2 & 4.0.1 & 2025-11-14 & RSPM & loaded\\
#> glue & 1.8.0 & 2024-09-30 & RSPM & loaded\\
#> gtable & 0.3.6 & 2024-10-25 & RSPM & loaded\\
#> htmltools & 0.5.9 & 2025-12-04 & RSPM & loaded\\
#> httr2 & 1.2.2 & 2025-12-08 & RSPM & loaded\\
#> jquerylib & 0.1.4 & 2021-04-26 & RSPM & loaded\\
#> jsonlite & 2.0.0 & 2025-03-27 & RSPM & loaded\\
#> kableExtra & 1.4.0 & 2024-01-24 & RSPM & loaded\\
#> knitr & 1.51 & 2025-12-20 & RSPM & loaded\\
#> labeling & 0.4.3 & 2023-08-29 & RSPM & loaded\\
#> lattice & 0.22-7 & 2025-04-02 & CRAN (R 4.5.2) & loaded\\
#> libcoin & 1.0-10 & 2023-09-27 & RSPM & loaded\\
#> lifecycle & 1.0.5 & 2026-01-08 & RSPM & loaded\\
#> magrittr & 2.0.4 & 2025-09-12 & RSPM & loaded\\
#> MASS & 7.3-65 & 2025-02-28 & CRAN (R 4.5.2) & loaded\\
#> Matrix & 1.7-4 & 2025-08-28 & CRAN (R 4.5.2) & loaded\\
#> matrixStats & 1.5.0 & 2025-01-07 & RSPM & loaded\\
#> memoise & 2.0.1 & 2021-11-26 & RSPM & loaded\\
#> modeltools & 0.2-24 & 2025-05-02 & RSPM & loaded\\
#> multcomp & 1.4-29 & 2025-10-20 & RSPM & loaded\\
#> mvtnorm & 1.3-3 & 2025-01-10 & RSPM & loaded\\
#> openssl & 2.3.4 & 2025-09-30 & RSPM & loaded\\
#> pillar & 1.11.1 & 2025-09-17 & RSPM & loaded\\
#> pkgconfig & 2.0.3 & 2019-09-22 & RSPM & loaded\\
#> pkgdown & 2.2.0 & 2025-11-06 & any (@2.2.0) & loaded\\
#> purrr & 1.2.1 & 2026-01-09 & RSPM & loaded\\
#> R6 & 2.6.1 & 2025-02-15 & RSPM & loaded\\
#> ragg & 1.5.0 & 2025-09-02 & RSPM & loaded\\
#> rappdirs & 0.3.3 & 2021-01-31 & RSPM & loaded\\
#> RColorBrewer & 1.1-3 & 2022-04-03 & RSPM & loaded\\
#> rlang & 1.1.7 & 2026-01-09 & RSPM & loaded\\
#> rmarkdown & 2.30 & 2025-09-28 & RSPM & loaded\\
#> rstudioapi & 0.17.1 & 2024-10-22 & RSPM & loaded\\
#> S7 & 0.2.1 & 2025-11-14 & RSPM & loaded\\
#> sandwich & 3.1-1 & 2024-09-15 & RSPM & loaded\\
#> sass & 0.4.10 & 2025-04-11 & RSPM & loaded\\
#> scales & 1.4.0 & 2025-04-24 & RSPM & loaded\\
#> sessioninfo & 1.2.3 & 2025-02-05 & RSPM & loaded\\
#> stringi & 1.8.7 & 2025-03-27 & RSPM & loaded\\
#> stringr & 1.6.0 & 2025-11-04 & RSPM & loaded\\
#> survival & 3.8-3 & 2024-12-17 & CRAN (R 4.5.2) & loaded\\
#> svglite & 2.2.2 & 2025-10-21 & RSPM & loaded\\
#> systemfonts & 1.3.1 & 2025-10-01 & RSPM & loaded\\
#> textshaping & 1.0.4 & 2025-10-10 & RSPM & loaded\\
#> TH.data & 1.1-5 & 2025-11-17 & RSPM & loaded\\
#> tibble & 3.3.1 & 2026-01-11 & RSPM & loaded\\
#> tidyr & 1.3.2 & 2025-12-19 & RSPM & loaded\\
#> tidyselect & 1.2.1 & 2024-03-11 & RSPM & loaded\\
#> utf8 & 1.2.6 & 2025-06-08 & RSPM & loaded\\
#> vctrs & 0.6.5 & 2023-12-01 & RSPM & loaded\\
#> viridisLite & 0.4.2 & 2023-05-02 & RSPM & loaded\\
#> whisker & 0.4.1 & 2022-12-05 & RSPM & loaded\\
#> withr & 3.0.2 & 2024-10-28 & RSPM & loaded\\
#> xfun & 0.55 & 2025-12-16 & RSPM & loaded\\
#> xml2 & 1.5.1 & 2025-12-01 & RSPM & loaded\\
#> yaml & 2.3.12 & 2025-12-10 & RSPM & loaded\\
#> zoo & 1.8-15 & 2025-12-15 & RSPM & loaded\\
#> \bottomrule
#> \end{tabular}
#> \end{table}
```
