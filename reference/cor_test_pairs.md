# Correlation Testing for Multiple Endpoints/Terms

Takes a continuous variable and a categorical variable, and calculates
the Spearman, Pearson, or Kendall correlation estimate and p-value
between the categorical variable levels.

## Usage

``` r
cor_test_pairs(
  x,
  pair,
  id,
  method = c("spearman", "pearson", "kendall"),
  n_distinct_value = 3,
  digits = 3,
  trailing_zeros = TRUE,
  exact = TRUE,
  seed = 68954857,
  nresample = 10000,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  numeric vector (can include NA values)

- pair:

  categorical vector which contains the levels to compare

- id:

  vector which contains the id information

- method:

  character string indicating which correlation coefficient is to be
  used for the test ("pearson" (default), "kendall", or "spearman").

- n_distinct_value:

  number of distinct values in `x` each `pair` must contain to be
  compared. The value must be \>1, with a default of 3.

- digits:

  numeric value between 0 and 14 indicating the number of digits to
  round the correlation estimate. The default is set to 3.

- trailing_zeros:

  logical indicating if trailing zeros should be included in the
  descriptive statistics (i.e. 0.100 instead of 0.1). Note if set to
  `TRUE`, output is a character vector.

- exact:

  logical value indicating whether the "exact" method should be used.
  Ignored if `method = "pearson"` or if `method = "spearman"` and there
  are ties in `x` for either `pair`.

- seed:

  numeric value used to set the seed. Only used if `method = "spearman"`
  and there are ties in `x` for either `pair`.

- nresample:

  positive integer indicating the number of Monte Carlo replicates to
  used for the computation of the approximative reference distribution.
  Defaults is set to 10,000. Only used when `method = "spearman"` and
  there are ties in `x` for either `pair`.

- verbose:

  logical variable indicating whether warnings and messages should be
  displayed.

- ...:

  parameters passed to
  [`stats::cor.test`](https://rdrr.io/r/stats/cor.test.html) or
  `coin:spearman_test`

## Value

Returns a data frame of all possible pairwise correlations with pair
sizes greater than or equal to the minimum number of values in pair, as
set by `n_distinct_value`:

- `Correlation` - Comparisons made

- `NPairs` - number of non-missing pairs considered

- `Ties` - are ties present in either variable

- `CorrEst` - correlation estimates

- `CorrTest` - correlation test p value

## Details

The p value is calculated using the
[cor_test](https://fredhutch.github.io/VISCfunctions/reference/cor_test.md)
function (see documentation for method details)

If a pair has less than `n_distinct_value` non-missing values that pair
will be excluded from the comparisons. If a specific comparison has less
than `n_distinct_value` non-missing values to comparison the output will
return an estimate and the p-value set to NA.

## Examples

``` r
data_in <- data.frame(
  id = 1:10,
  x = c(-2, -1, 0, 1, 2,-2, -1, 0, 1, 2),
  y = c(4, 1, NA, 1, 4,-2, -1, 0, 1, 2),
  z = c(1, 2, 3, 4, NA,-2, -1, 0, 1, 2),
  v = c(rep(1,10)),
  aa = c(1:5,NA,NA,NA,NA,NA),
  bb = c(NA,NA,NA,NA,NA,1:5)
)
data_in_long <- tidyr::pivot_longer(data_in, -id)
cor_test_pairs(x = data_in_long$value,
                  pair = data_in_long$name,
                  id = data_in_long$id,
                  method = 'spearman')
#>    Correlation NPoints         Ties CorrEst   CorrTest
#> 1    aa and bb       0      no ties    <NA>         NA
#> 2     aa and x       5      no ties   1.000 0.01666667
#> 3     aa and y       4    ties in y    <NA>         NA
#> 4     aa and z       4      no ties   1.000 0.08333333
#> 5     bb and x       5      no ties   1.000 0.01666667
#> 6     bb and y       5      no ties   1.000 0.01666667
#> 7     bb and z       5      no ties   1.000 0.01666667
#> 8      x and y       9 ties in both   0.442 0.23650000
#> 9      x and z       9 ties in both   0.568 0.11550000
#> 10     y and z       8 ties in both   0.704 0.05970000


# Examples with Real World Data
library(dplyr)

# BAMA Assay Data Example
data(exampleData_BAMA)

## Antigen Correlation
exampleData_BAMA |>
filter(visitno != 0) |>
group_by(group, visitno) |>
 summarize(
   cor_test_pairs(x = magnitude, pair = antigen, id = pubID,
   method = 'spearman', n_distinct_value = 3, digits = 1, verbose = TRUE),
   .groups = 'drop'
 )
#> Warning: Returning more (or less) than 1 row per `summarise()` group was deprecated in
#> dplyr 1.1.0.
#> ℹ Please use `reframe()` instead.
#> ℹ When switching from `summarise()` to `reframe()`, remember that `reframe()`
#>   always returns an ungrouped data frame and adjust accordingly.
#> # A tibble: 84 × 7
#>    group visitno Correlation                      NPoints Ties  CorrEst CorrTest
#>    <int>   <dbl> <chr>                              <int> <chr> <chr>      <dbl>
#>  1     1       1 A1.con.env03 140 CF and A244 D1…       6 no t… 1.0      0.00278
#>  2     1       1 A1.con.env03 140 CF and B.63521…       6 no t… 0.9      0.0333 
#>  3     1       1 A1.con.env03 140 CF and B.MN V3…       6 no t… -0.4     0.419  
#>  4     1       1 A1.con.env03 140 CF and B.con.e…       6 no t… 0.9      0.0167 
#>  5     1       1 A1.con.env03 140 CF and gp41           6 no t… 0.8      0.103  
#>  6     1       1 A1.con.env03 140 CF and p24            6 no t… 0.3      0.564  
#>  7     1       1 A244 D11gp120_avi and B.63521_D…       6 no t… 0.9      0.0333 
#>  8     1       1 A244 D11gp120_avi and B.MN V3 g…       6 no t… -0.4     0.419  
#>  9     1       1 A244 D11gp120_avi and B.con.env…       6 no t… 0.9      0.0167 
#> 10     1       1 A244 D11gp120_avi and gp41             6 no t… 0.8      0.103  
#> # ℹ 74 more rows
```
