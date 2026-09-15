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
#> 1     1 vs. 2   24 vs. 25        54 [4, 94] vs. 45 [3, 99]
#> 2     1 vs. 3   24 vs. 25        54 [4, 94] vs. 49 [5, 95]
#> 3     1 vs. 4   24 vs. 25        54 [4, 94] vs. 53 [1, 98]
#> 4     1 vs. a    24 vs. 2 54 [4, 94] vs. 1667 [1111, 2222]
#> 5     2 vs. 3   25 vs. 25        45 [3, 99] vs. 49 [5, 95]
#> 6     2 vs. 4   25 vs. 25        45 [3, 99] vs. 53 [1, 98]
#> 7     2 vs. a    25 vs. 2 45 [3, 99] vs. 1667 [1111, 2222]
#> 8     3 vs. 4   25 vs. 25        49 [5, 95] vs. 53 [1, 98]
#> 9     3 vs. a    25 vs. 2 49 [5, 95] vs. 1667 [1111, 2222]
#> 10    4 vs. a    25 vs. 2 53 [1, 98] vs. 1667 [1111, 2222]
#>                     Median_Quartiles                Mean_SD MagnitudeTest
#> 1        54 [30, 77] vs. 45 [27, 78]    52 (29) vs. 51 (30)   0.944709686
#> 2        54 [30, 77] vs. 49 [21, 66]    52 (29) vs. 47 (28)   0.506250091
#> 3        54 [30, 77] vs. 53 [30, 73]    52 (29) vs. 51 (30)   0.944709686
#> 4  54 [30, 77] vs. 1667 [1389, 1944] 52 (29) vs. 1667 (786)   0.006153846
#> 5        45 [27, 78] vs. 49 [21, 66]    51 (30) vs. 47 (28)   0.658147476
#> 6        45 [27, 78] vs. 53 [30, 73]    51 (30) vs. 51 (30)   0.908156495
#> 7  45 [27, 78] vs. 1667 [1389, 1944] 51 (30) vs. 1667 (786)   0.005698006
#> 8        49 [21, 66] vs. 53 [30, 73]    47 (28) vs. 51 (30)   0.630516700
#> 9  49 [21, 66] vs. 1667 [1389, 1944] 47 (28) vs. 1667 (786)   0.005698006
#> 10 53 [30, 73] vs. 1667 [1389, 1944] 51 (30) vs. 1667 (786)   0.005698006
#>    PerfectSeparation
#> 1              FALSE
#> 2              FALSE
#> 3              FALSE
#> 4               TRUE
#> 5              FALSE
#> 6              FALSE
#> 7               TRUE
#> 8              FALSE
#> 9               TRUE
#> 10              TRUE

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
#> 1         54.000 [4.000, 94.000] vs. 45.000 [3.000, 99.000]
#> 2         54.000 [4.000, 94.000] vs. 49.000 [5.000, 95.000]
#> 3         54.000 [4.000, 94.000] vs. 53.000 [1.000, 98.000]
#> 4  54.000 [4.000, 94.000] vs. 1666.500 [1111.000, 2222.000]
#> 5         45.000 [3.000, 99.000] vs. 49.000 [5.000, 95.000]
#> 6         45.000 [3.000, 99.000] vs. 53.000 [1.000, 98.000]
#> 7  45.000 [3.000, 99.000] vs. 1666.500 [1111.000, 2222.000]
#> 8         49.000 [5.000, 95.000] vs. 53.000 [1.000, 98.000]
#> 9  49.000 [5.000, 95.000] vs. 1666.500 [1111.000, 2222.000]
#> 10 53.000 [1.000, 98.000] vs. 1666.500 [1111.000, 2222.000]
#>                                             Median_Quartiles
#> 1        54.000 [29.750, 76.750] vs. 45.000 [27.000, 78.000]
#> 2        54.000 [29.750, 76.750] vs. 49.000 [21.000, 66.000]
#> 3        54.000 [29.750, 76.750] vs. 53.000 [30.000, 73.000]
#> 4  54.000 [29.750, 76.750] vs. 1666.500 [1388.750, 1944.250]
#> 5        45.000 [27.000, 78.000] vs. 49.000 [21.000, 66.000]
#> 6        45.000 [27.000, 78.000] vs. 53.000 [30.000, 73.000]
#> 7  45.000 [27.000, 78.000] vs. 1666.500 [1388.750, 1944.250]
#> 8        49.000 [21.000, 66.000] vs. 53.000 [30.000, 73.000]
#> 9  49.000 [21.000, 66.000] vs. 1666.500 [1388.750, 1944.250]
#> 10 53.000 [30.000, 73.000] vs. 1666.500 [1388.750, 1944.250]
#>                                   Mean_SD MagnitudeTest PerfectSeparation
#> 1     51.750 (28.659) vs. 50.520 (29.834)   0.535525529             FALSE
#> 2     51.750 (28.659) vs. 46.680 (27.907)   0.753197416             FALSE
#> 3     51.750 (28.659) vs. 51.120 (29.943)   0.535525529             FALSE
#> 4  51.750 (28.659) vs. 1666.500 (785.596)   0.003076923              TRUE
#> 5     50.520 (29.834) vs. 46.680 (27.907)   0.677864119             FALSE
#> 6     50.520 (29.834) vs. 51.120 (29.943)   0.454078248             FALSE
#> 7  50.520 (29.834) vs. 1666.500 (785.596)   0.002849003              TRUE
#> 8     46.680 (27.907) vs. 51.120 (29.943)   0.315258350             FALSE
#> 9  46.680 (27.907) vs. 1666.500 (785.596)   0.002849003              TRUE
#> 10 51.120 (29.943) vs. 1666.500 (785.596)   0.002849003              TRUE

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
#>                                              Median_Min_Max
#> 1         53.404 [4.000, 94.000] vs. 45.000 [3.000, 99.000]
#> 2         53.404 [4.000, 94.000] vs. 49.000 [5.000, 95.000]
#> 3         53.404 [4.000, 94.000] vs. 53.000 [1.000, 98.000]
#> 4  53.404 [4.000, 94.000] vs. 1571.191 [1111.000, 2222.000]
#> 5         45.000 [3.000, 99.000] vs. 49.000 [5.000, 95.000]
#> 6         45.000 [3.000, 99.000] vs. 53.000 [1.000, 98.000]
#> 7  45.000 [3.000, 99.000] vs. 1571.191 [1111.000, 2222.000]
#> 8         49.000 [5.000, 95.000] vs. 53.000 [1.000, 98.000]
#> 9  49.000 [5.000, 95.000] vs. 1571.191 [1111.000, 2222.000]
#> 10 53.000 [1.000, 98.000] vs. 1571.191 [1111.000, 2222.000]
#>                                             Median_Quartiles
#> 1        53.404 [29.750, 76.750] vs. 45.000 [27.000, 78.000]
#> 2        53.404 [29.750, 76.750] vs. 49.000 [21.000, 66.000]
#> 3        53.404 [29.750, 76.750] vs. 53.000 [30.000, 73.000]
#> 4  53.404 [29.750, 76.750] vs. 1571.191 [1388.750, 1944.250]
#> 5        45.000 [27.000, 78.000] vs. 49.000 [21.000, 66.000]
#> 6        45.000 [27.000, 78.000] vs. 53.000 [30.000, 73.000]
#> 7  45.000 [27.000, 78.000] vs. 1571.191 [1388.750, 1944.250]
#> 8        49.000 [21.000, 66.000] vs. 53.000 [30.000, 73.000]
#> 9  49.000 [21.000, 66.000] vs. 1571.191 [1388.750, 1944.250]
#> 10 53.000 [30.000, 73.000] vs. 1571.191 [1388.750, 1944.250]
#>                   Mean                     log_Mean_SD MagnitudeTest
#> 1    41.129 vs. 39.467 1.614 (0.349) vs. 1.596 (0.362)   0.535525529
#> 2    41.129 vs. 34.841 1.614 (0.349) vs. 1.542 (0.396)   0.753197416
#> 3    41.129 vs. 35.554 1.614 (0.349) vs. 1.551 (0.504)   0.535525529
#> 4  41.129 vs. 1571.191 1.614 (0.349) vs. 3.196 (0.213)   0.003076923
#> 5    39.467 vs. 34.841 1.596 (0.362) vs. 1.542 (0.396)   0.677864119
#> 6    39.467 vs. 35.554 1.596 (0.362) vs. 1.551 (0.504)   0.454078248
#> 7  39.467 vs. 1571.191 1.596 (0.362) vs. 3.196 (0.213)   0.002849003
#> 8    34.841 vs. 35.554 1.542 (0.396) vs. 1.551 (0.504)   0.315258350
#> 9  34.841 vs. 1571.191 1.542 (0.396) vs. 3.196 (0.213)   0.002849003
#> 10 35.554 vs. 1571.191 1.551 (0.504) vs. 3.196 (0.213)   0.002849003
#>    PerfectSeparation
#> 1              FALSE
#> 2              FALSE
#> 3              FALSE
#> 4               TRUE
#> 5              FALSE
#> 6              FALSE
#> 7               TRUE
#> 8              FALSE
#> 9               TRUE
#> 10              TRUE



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
#> # A tibble: 108 × 11
#>    Stim  Parent   Population Visit Comparison SampleSizes Median_Min_Max        
#>    <chr> <chr>    <chr>      <int> <chr>      <chr>       <chr>                 
#>  1 GAG   CD4/TOTM IFNg           0 1 < 2      4 vs. 6     0.0000 [0.0000, 0.002…
#>  2 GAG   CD4/TOTM IFNg           0 1 < 3      4 vs. 2     0.0000 [0.0000, 0.002…
#>  3 GAG   CD4/TOTM IFNg           0 1 < 4      4 vs. 5     0.0000 [0.0000, 0.002…
#>  4 GAG   CD4/TOTM IFNg           0 2 < 3      6 vs. 2     0.0000 [0.0000, 0.006…
#>  5 GAG   CD4/TOTM IFNg           0 2 < 4      6 vs. 5     0.0000 [0.0000, 0.006…
#>  6 GAG   CD4/TOTM IFNg           0 3 < 4      2 vs. 5     0.0000 [0.0000, 0.000…
#>  7 GAG   CD4/TOTM IFNg           1 1 < 2      4 vs. 6     0.0078 [0.0035, 0.008…
#>  8 GAG   CD4/TOTM IFNg           1 1 < 3      4 vs. 2     0.0078 [0.0035, 0.008…
#>  9 GAG   CD4/TOTM IFNg           1 1 < 4      4 vs. 5     0.0078 [0.0035, 0.008…
#> 10 GAG   CD4/TOTM IFNg           1 2 < 3      6 vs. 2     0.0112 [0.0027, 0.052…
#> # ℹ 98 more rows
#> # ℹ 4 more variables: Median_Quartiles <chr>, Mean_SD <chr>,
#> #   MagnitudeTest <dbl>, PerfectSeparation <lgl>

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
