# Replace repeated rows in a data.frame with NA

This function is for replacing repeated rows in a data.frame into NA for
nice printing. This is not intended for use during processing.

## Usage

``` r
collapse_group_row(.data, ..., reorder_cols = TRUE)
```

## Arguments

- .data:

  a data.frame

- ...:

  Columns to use to identify which to rows to replace with NA's

- reorder_cols:

  if `TRUE` will order columns listed in `...` first

## Value

a data.frame where repeated rows in the columns identified by ... are
replaced with NA

## Examples

``` r
sample_df <- data.frame(
  x = c(1, 1, 1, 2, 2, 2, 2),
  y = c('test1', 'test1', 'test2', 'test1', 'test2', 'test2', 'test1'),
  z = c(1, 2, 3, 4, 5, 6, 7),
  outputVal = runif(7)
)

library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
options(knitr.kable.NA = '')
collapse_group_row(sample_df, x, y, z) |>
 kableExtra::kable() |>
 kableExtra::kable_styling()
#> <table class="table" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;">  </th>
#>    <th style="text-align:right;"> x </th>
#>    <th style="text-align:left;"> y </th>
#>    <th style="text-align:right;"> z </th>
#>    <th style="text-align:right;"> outputVal </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> 1 </td>
#>    <td style="text-align:right;"> 1 </td>
#>    <td style="text-align:left;"> test1 </td>
#>    <td style="text-align:right;"> 1 </td>
#>    <td style="text-align:right;"> 0.0073994 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> 2 </td>
#>    <td style="text-align:right;">  </td>
#>    <td style="text-align:left;">  </td>
#>    <td style="text-align:right;"> 2 </td>
#>    <td style="text-align:right;"> 0.4663935 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> 3 </td>
#>    <td style="text-align:right;">  </td>
#>    <td style="text-align:left;"> test2 </td>
#>    <td style="text-align:right;"> 3 </td>
#>    <td style="text-align:right;"> 0.4977774 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> 4 </td>
#>    <td style="text-align:right;"> 2 </td>
#>    <td style="text-align:left;"> test1 </td>
#>    <td style="text-align:right;"> 4 </td>
#>    <td style="text-align:right;"> 0.2897672 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> 7 </td>
#>    <td style="text-align:right;">  </td>
#>    <td style="text-align:left;">  </td>
#>    <td style="text-align:right;"> 7 </td>
#>    <td style="text-align:right;"> 0.8746007 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> 5 </td>
#>    <td style="text-align:right;">  </td>
#>    <td style="text-align:left;"> test2 </td>
#>    <td style="text-align:right;"> 5 </td>
#>    <td style="text-align:right;"> 0.7328820 </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;"> 6 </td>
#>    <td style="text-align:right;">  </td>
#>    <td style="text-align:left;">  </td>
#>    <td style="text-align:right;"> 6 </td>
#>    <td style="text-align:right;"> 0.7725215 </td>
#>   </tr>
#> </tbody>
#> </table>
```
