# Pairwise Testing for a Continuous Variable

Takes a continuous variable and performs pairwise testing (t-test or
wilcox test)

## Usage

``` r
pairwise_test_cont(
  x,
  group,
  paired = FALSE,
  id = NULL,
  method = c("wilcox", "t.test"),
  alternative = c("two.sided", "less", "greater"),
  sorted_group = NULL,
  num_needed_for_test = 3,
  log10_stats = FALSE,
  digits = 0,
  trailing_zeros = TRUE,
  sep_val = " vs. ",
  na_str_out = "---",
  verbose = FALSE
)
```

## Arguments

- x:

  numeric vector (can include NA values).

- group:

  categorical vector of group values.

- paired:

  a logical variable indicating whether to do a paired test.

- id:

  vector which contains the id information (so `x` values can be linked
  between groups). Only used and must be present when paired = TRUE.

- method:

  what test to run ("wilcox" or "t.test").

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "two.sided" (default), "greater" or "less". You can specify just the
  initial letter.

- sorted_group:

  a vector listing the group testing order from lowest to highest.

- num_needed_for_test:

  required sample size (per group) to perform test. Note at least 2
  distinct values per group are always needed for testing.

- log10_stats:

  specifies whether the summary statistics and p values should be
  calculated on log10 values. This could affect the median, mean, and p
  value.If TRUE, geometric mean is displayed as well as mean (sd)
  results on log10 x values (default is FALSE)

- digits:

  digits to round for magnitude descriptive statistics (default = 0).

- trailing_zeros:

  logical indicating if trailing zeros should be included in the
  descriptive statistics (i.e. 0.100 instead of 0.1). Note if set to
  TRUE, output is a character vector.

- sep_val:

  value to be pasted between the two measures. Default is ' vs. '.

- na_str_out:

  the character string in the output table that replaces missing values.

- verbose:

  a logical variable indicating if warnings and messages should be
  displayed.

## Value

Returns a data frame with all possible pairwise comparisons:

- `Comparison` - Comparisons made

- `SampleSizes` - number of samples per group

- `Median_Min_Max` - Median \[Min, Max\] per group

- `Mean_SD` - Mean(sd) per group (if `log10_stats` = FALSE)

- `Mean` - Geometric mean per group (if `log10_stats` = TRUE)

- `log_Mean_SD` - Mean(sd) per group on log10 `x` scale (if
  `log10_stats` = TRUE)

- `MagnitudeTest` - wilcox/t-test test p value

- `PerfectSeparation` - logical flag indicating perfect separation

Returns a data frame with all possible pairwise comparisons. Variables
include Comparison, SampleSizes, Median_Min_Max (group stats; median
\[min, max\]), Mean_SD (group stats; mean (sd)), MagnitudeTest
(wilcox/t-test p-value), PerfectSeparation (a logical flag indicating if
there is perfect separation).

## Details

Runs `wilcox_test()` in the coin package, with "exact" distribution.

If `sorted_group` is not specified then testing order based on factor
levels if `group` is a factor, and alphabetical order otherwise

`trailing_zeros` does not impact p-value column, which will be a numeric
column regardless.

If `paired = TRUE` the descriptive statistics are shown for observations
that have non-missing values for both groups.

## Examples

``` r
x_example <- c(NA, sample(1:50, 50), sample(51:99, 49), 1111,2222)
group_example <- c(rep(1:4,25),'a','a')

pairwise_test_cont(x_example,group_example, num_needed_for_test = 2)
#>    Comparison SampleSizes                   Median_Min_Max
#> 1     1 vs. 2   24 vs. 25        56 [4, 94] vs. 48 [3, 99]
#> 2     1 vs. 3   24 vs. 25        56 [4, 94] vs. 46 [1, 95]
#> 3     1 vs. 4   24 vs. 25        56 [4, 94] vs. 53 [2, 98]
#> 4     1 vs. a    24 vs. 2 56 [4, 94] vs. 1666 [1111, 2222]
#> 5     2 vs. 3   25 vs. 25        48 [3, 99] vs. 46 [1, 95]
#> 6     2 vs. 4   25 vs. 25        48 [3, 99] vs. 53 [2, 98]
#> 7     2 vs. a    25 vs. 2 48 [3, 99] vs. 1666 [1111, 2222]
#> 8     3 vs. 4   25 vs. 25        46 [1, 95] vs. 53 [2, 98]
#> 9     3 vs. a    25 vs. 2 46 [1, 95] vs. 1666 [1111, 2222]
#> 10    4 vs. a    25 vs. 2 53 [2, 98] vs. 1666 [1111, 2222]
#>                   Mean_SD MagnitudeTest PerfectSeparation
#> 1     50 (30) vs. 51 (30)   0.866238940             FALSE
#> 2     50 (30) vs. 46 (28)   0.585480654             FALSE
#> 3     50 (30) vs. 52 (29)   0.881854562             FALSE
#> 4  50 (30) vs. 1666 (786)   0.006153846              TRUE
#> 5     51 (30) vs. 46 (28)   0.616886231             FALSE
#> 6     51 (30) vs. 52 (29)   0.877748781             FALSE
#> 7  51 (30) vs. 1666 (786)   0.005698006              TRUE
#> 8     46 (28) vs. 52 (29)   0.500405306             FALSE
#> 9  46 (28) vs. 1666 (786)   0.005698006              TRUE
#> 10 52 (29) vs. 1666 (786)   0.005698006              TRUE

pairwise_test_cont(
x_example,group_example, alternative = "less",
  sorted_group = c(1:4, 'a'), num_needed_for_test = 2, , digits = 3)
#>    Comparison SampleSizes
#> 1       1 < 2   24 vs. 25
#> 2       1 < 3   24 vs. 25
#> 3       1 < 4   24 vs. 25
#> 4       1 < a    24 vs. 2
#> 5       2 < 3   25 vs. 25
#> 6       2 < 4   25 vs. 25
#> 7       2 < a    25 vs. 2
#> 8       3 < 4   25 vs. 25
#> 9       3 < a    25 vs. 2
#> 10      4 < a    25 vs. 2
#>                                              Median_Min_Max
#> 1         55.500 [4.000, 94.000] vs. 48.000 [3.000, 99.000]
#> 2         55.500 [4.000, 94.000] vs. 46.000 [1.000, 95.000]
#> 3         55.500 [4.000, 94.000] vs. 53.000 [2.000, 98.000]
#> 4  55.500 [4.000, 94.000] vs. 1666.500 [1111.000, 2222.000]
#> 5         48.000 [3.000, 99.000] vs. 46.000 [1.000, 95.000]
#> 6         48.000 [3.000, 99.000] vs. 53.000 [2.000, 98.000]
#> 7  48.000 [3.000, 99.000] vs. 1666.500 [1111.000, 2222.000]
#> 8         46.000 [1.000, 95.000] vs. 53.000 [2.000, 98.000]
#> 9  46.000 [1.000, 95.000] vs. 1666.500 [1111.000, 2222.000]
#> 10 53.000 [2.000, 98.000] vs. 1666.500 [1111.000, 2222.000]
#>                                   Mean_SD MagnitudeTest PerfectSeparation
#> 1     50.458 (30.041) vs. 51.080 (29.858)   0.433119470             FALSE
#> 2     50.458 (30.041) vs. 46.400 (27.618)   0.714054189             FALSE
#> 3     50.458 (30.041) vs. 52.080 (28.814)   0.440927281             FALSE
#> 4  50.458 (30.041) vs. 1666.500 (785.596)   0.003076923              TRUE
#> 5     51.080 (29.858) vs. 46.400 (27.618)   0.698307916             FALSE
#> 6     51.080 (29.858) vs. 52.080 (28.814)   0.438874391             FALSE
#> 7  51.080 (29.858) vs. 1666.500 (785.596)   0.002849003              TRUE
#> 8     46.400 (27.618) vs. 52.080 (28.814)   0.250202653             FALSE
#> 9  46.400 (27.618) vs. 1666.500 (785.596)   0.002849003              TRUE
#> 10 52.080 (28.814) vs. 1666.500 (785.596)   0.002849003              TRUE

# using log10 computations
pairwise_test_cont(
x_example,group_example, alternative = "less", log10_stats = TRUE,
  sorted_group = c(1:4, 'a'), num_needed_for_test = 2, digits = 3)
#>    Comparison SampleSizes
#> 1       1 < 2   24 vs. 25
#> 2       1 < 3   24 vs. 25
#> 3       1 < 4   24 vs. 25
#> 4       1 < a    24 vs. 2
#> 5       2 < 3   25 vs. 25
#> 6       2 < 4   25 vs. 25
#> 7       2 < a    25 vs. 2
#> 8       3 < 4   25 vs. 25
#> 9       3 < a    25 vs. 2
#> 10      4 < a    25 vs. 2
#>                                              Median_Min_Max                Mean
#> 1         55.118 [4.000, 94.000] vs. 48.000 [3.000, 99.000]   38.566 vs. 39.080
#> 2         55.118 [4.000, 94.000] vs. 46.000 [1.000, 95.000]   38.566 vs. 33.231
#> 3         55.118 [4.000, 94.000] vs. 53.000 [2.000, 98.000]   38.566 vs. 40.045
#> 4  55.118 [4.000, 94.000] vs. 1571.191 [1111.000, 2222.000] 38.566 vs. 1571.191
#> 5         48.000 [3.000, 99.000] vs. 46.000 [1.000, 95.000]   39.080 vs. 33.231
#> 6         48.000 [3.000, 99.000] vs. 53.000 [2.000, 98.000]   39.080 vs. 40.045
#> 7  48.000 [3.000, 99.000] vs. 1571.191 [1111.000, 2222.000] 39.080 vs. 1571.191
#> 8         46.000 [1.000, 95.000] vs. 53.000 [2.000, 98.000]   33.231 vs. 40.045
#> 9  46.000 [1.000, 95.000] vs. 1571.191 [1111.000, 2222.000] 33.231 vs. 1571.191
#> 10 53.000 [2.000, 98.000] vs. 1571.191 [1111.000, 2222.000] 40.045 vs. 1571.191
#>                        log_Mean_SD MagnitudeTest PerfectSeparation
#> 1  1.586 (0.375) vs. 1.592 (0.386)   0.433119470             FALSE
#> 2  1.586 (0.375) vs. 1.522 (0.466)   0.714054189             FALSE
#> 3  1.586 (0.375) vs. 1.603 (0.398)   0.440927281             FALSE
#> 4  1.586 (0.375) vs. 3.196 (0.213)   0.003076923              TRUE
#> 5  1.592 (0.386) vs. 1.522 (0.466)   0.698307916             FALSE
#> 6  1.592 (0.386) vs. 1.603 (0.398)   0.438874391             FALSE
#> 7  1.592 (0.386) vs. 3.196 (0.213)   0.002849003              TRUE
#> 8  1.522 (0.466) vs. 1.603 (0.398)   0.250202653             FALSE
#> 9  1.522 (0.466) vs. 3.196 (0.213)   0.002849003              TRUE
#> 10 1.603 (0.398) vs. 3.196 (0.213)   0.002849003              TRUE



# Examples with Real World Data
library(dplyr)

# BAMA Assay Data Example
data(exampleData_BAMA)

## Group Comparison
group_testing_tibble <- exampleData_BAMA |>
   group_by(antigen, visitno) |>
   reframe(pairwise_test_cont(x = magnitude,
                              group = group,
                              paired = FALSE,
                              method = 'wilcox',
                              alternative = "less",
                              sorted_group = c(1,2),
                              digits = 3,
                              num_needed_for_test = 3,
                              verbose = TRUE))


## Timepoint Comparison
timepoint_testing_dt <- exampleData_BAMA |>
                       group_by(antigen, group) |>
                       reframe(pairwise_test_cont(x = magnitude,
                                                  group = visitno,
                                                  paired = TRUE,
                                                  id = pubID,
                                                  method = 'wilcox',
                                                  sorted_group = c(0,1,2),
                                                  alternative = 'less',
                                                  num_needed_for_test = 3,
                                                  digits = 3,
                                                  trailing_zeros = TRUE,
                                                  sep_val = ' vs. ',
                                                  verbose = TRUE))


# ICS Assay Data Example
data(exampleData_ICS)

## Group Comparison
# using dplyr
exampleData_ICS |>
group_by(Stim, Parent, Population, Visit) |>
reframe(pairwise_test_cont(x = PercentCellNet,
                          group = Group,
                          paired = FALSE,
                          method = 'wilcox',
                          alternative = 'less',
                          sorted_group = c(1,2,3,4),
                          num_needed_for_test = 3,
                          digits = 4,
                          trailing_zeros = TRUE,
                          sep_val = ' vs. ',
                          verbose = TRUE))
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> # A tibble: 108 × 10
#>    Stim  Parent   Population Visit Comparison SampleSizes Median_Min_Max Mean_SD
#>    <chr> <chr>    <chr>      <int> <chr>      <chr>       <chr>          <chr>  
#>  1 GAG   CD4/TOTM IFNg           0 1 < 2      4 vs. 6     0.0000 [0.000… 0.0006…
#>  2 GAG   CD4/TOTM IFNg           0 1 < 3      4 vs. 2     0.0000 [0.000… 0.0006…
#>  3 GAG   CD4/TOTM IFNg           0 1 < 4      4 vs. 5     0.0000 [0.000… 0.0006…
#>  4 GAG   CD4/TOTM IFNg           0 2 < 3      6 vs. 2     0.0000 [0.000… 0.0014…
#>  5 GAG   CD4/TOTM IFNg           0 2 < 4      6 vs. 5     0.0000 [0.000… 0.0014…
#>  6 GAG   CD4/TOTM IFNg           0 3 < 4      2 vs. 5     0.0000 [0.000… 0.0000…
#>  7 GAG   CD4/TOTM IFNg           1 1 < 2      4 vs. 6     0.0078 [0.003… 0.0069…
#>  8 GAG   CD4/TOTM IFNg           1 1 < 3      4 vs. 2     0.0078 [0.003… 0.0069…
#>  9 GAG   CD4/TOTM IFNg           1 1 < 4      4 vs. 5     0.0078 [0.003… 0.0069…
#> 10 GAG   CD4/TOTM IFNg           1 2 < 3      6 vs. 2     0.0112 [0.002… 0.0176…
#> # ℹ 98 more rows
#> # ℹ 2 more variables: MagnitudeTest <dbl>, PerfectSeparation <lgl>

# Timepoint Comparison
timepoint_testing_dt <- exampleData_ICS |>
                       group_by(Stim, Parent, Population, Group) |>
                       reframe(pairwise_test_cont(x = PercentCellNet,
                                                  group = Visit,
                                                  paired = TRUE,
                                                  id = pubID,
                                                  method = 'wilcox',
                                                  sorted_group = c(0,1,2),
                                                  alternative = 'less',
                                                  num_needed_for_test = 3,
                                                  digits = 4,
                                                  trailing_zeros = TRUE,
                                                  sep_val = ' vs. ',
                                                  verbose = TRUE))
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)
#> x does not have at least 3 non missing per group, so no test run (MagnitudeTest=NA returned)

```
