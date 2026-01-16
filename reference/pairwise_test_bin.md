# Binary Variable Pairwise Testing

Takes a binary variable (e.g., response status) and performs pairwise
testing. Performs either Barnard, Fisher's, or Chi-sq test for unpaired
data and McNemar's test for paired data.

## Usage

``` r
pairwise_test_bin(
  x,
  group,
  id = NULL,
  method = c("barnard", "fisher", "chi.sq", "mcnemar"),
  barnard_method = c("z-pooled", "z-unpooled", "boschloo", "santner and snell", "csm",
    "csm approximate", "csm modified"),
  alternative = c("two.sided", "less", "greater"),
  sorted_group = NULL,
  num_needed_for_test = 3,
  conf_level = 0.95,
  digits = 1,
  trailing_zeros = TRUE,
  sep_val = " vs. ",
  na_str_out = "---",
  latex_output = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  numeric vector (0/1) or logical vector or (F/T) (can include NA
  values)

- group:

  categorical vector of group values.

- id:

  vector which contains the id information (so `x` values can be linked
  between groups). Only used and must be present when
  `method = 'mcnemar'`.

- method:

  what test to run, "barnard" (default), "fisher" ,"chi.sq" , or
  "mcnemar")

- barnard_method:

  indicates the Barnard method for finding tables as or more extreme
  than the observed table: must be either "Z-pooled", "Z-unpooled",
  "Santner and Snell", "Boschloo", "CSM", "CSM approximate", or "CSM
  modified". Only used when `method = 'barnard'`

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "two.sided" (default), "greater" or "less". You can specify just the
  initial letter. Only "two.sided" available for
  `method = 'chi.sq' or 'mcnemar'`

- sorted_group:

  a vector listing the group testing order from lowest to highest, if
  performing one sided tests

- num_needed_for_test:

  required sample size (per group) to perform test.

- conf_level:

  The level of confidence to be used in the confidence interval.

- digits:

  digits to round for descriptive statistics

- trailing_zeros:

  logical indicating if trailing zeros should be included in the
  descriptive statistics (i.e. 0.100 instead of 0.1). Note if set to
  TRUE, output is a character vector.

- sep_val:

  value to be pasted between the two measures. Default is ' vs. '.

- na_str_out:

  the character string in the output table that replaces missing values.

- latex_output:

  will this table be used for latex output (default is FALSE)

- verbose:

  a logical variable indicating if warnings and messages should be
  displayed

- ...:

  other parameters to pass to Exact::exact.test when running Barnard
  test

## Value

Returns a data frame with all possible pairwise comparisons. Variables
include Comparison, ResponseStats (group stats; number positive / number
= rate (Wilson CI Bounds)), ResponseTest (fisher/chisq p value),
PerfectSeparation (a logical flag indicating if one group if 0% and the
other 100%)

## Details

If all values of `x` are NA, the function will return NULL. This is to
allow for nice return when looping through function with dplyr
`group_by` and `group_modify`

For one sided tests if `sorted_group = NULL` than the factor level order
of `group` is respected, otherwise the levels will set to alphabetical
order (i.e. if `alternative = less` then testing a \< b ).

If planning on using the table in a latex document then set
`latex_output = TRUE`. This will set the `%` symbol to `\\%` in the
binary percentages

## Examples

``` r
set.seed(1)
x_example = c(NA,sample(0:1,50,replace = TRUE, prob = c(.75,.25)),
  sample(0:1,50,replace = TRUE, prob = c(.25,.75)),0,0,1,1)
group_example = c(rep(1,25),NA,rep(2,25),rep(3,25),rep(4,25),'a','a','b','b')

pairwise_test_bin(x_example,group_example, num_needed_for_test = 2)
#>    Comparison                                                 ResponseStats
#> 1     1 vs. 2   7/24 = 29.2% (14.9%, 49.2%) vs. 6/25 = 24.0% (11.5%, 43.4%)
#> 2     1 vs. 3  7/24 = 29.2% (14.9%, 49.2%) vs. 20/25 = 80.0% (60.9%, 91.1%)
#> 3     1 vs. 4  7/24 = 29.2% (14.9%, 49.2%) vs. 16/25 = 64.0% (44.5%, 79.8%)
#> 4     1 vs. a      7/24 = 29.2% (14.9%, 49.2%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 5     1 vs. b  7/24 = 29.2% (14.9%, 49.2%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 6     2 vs. 3  6/25 = 24.0% (11.5%, 43.4%) vs. 20/25 = 80.0% (60.9%, 91.1%)
#> 7     2 vs. 4  6/25 = 24.0% (11.5%, 43.4%) vs. 16/25 = 64.0% (44.5%, 79.8%)
#> 8     2 vs. a      6/25 = 24.0% (11.5%, 43.4%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 9     2 vs. b  6/25 = 24.0% (11.5%, 43.4%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 10    3 vs. 4 20/25 = 80.0% (60.9%, 91.1%) vs. 16/25 = 64.0% (44.5%, 79.8%)
#> 11    3 vs. a     20/25 = 80.0% (60.9%, 91.1%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 12    3 vs. b 20/25 = 80.0% (60.9%, 91.1%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 13    4 vs. a     16/25 = 64.0% (44.5%, 79.8%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 14    4 vs. b 16/25 = 64.0% (44.5%, 79.8%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 15    a vs. b     0/2 = 0.0% (0.0%, 65.8%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#>    ResponseTest PerfectSeparation
#> 1  7.681948e-01             FALSE
#> 2  3.858052e-04             FALSE
#> 3  1.666966e-02             FALSE
#> 4  5.716246e-01             FALSE
#> 5  6.895733e-02             FALSE
#> 6  9.064775e-05             FALSE
#> 7  4.983144e-03             FALSE
#> 8  7.702599e-01             FALSE
#> 9  6.623440e-02             FALSE
#> 10 2.344532e-01             FALSE
#> 11 2.929379e-02             FALSE
#> 12 7.151828e-01             FALSE
#> 13 1.074259e-01             FALSE
#> 14 4.735172e-01             FALSE
#> 15 1.250000e-01              TRUE

pairwise_test_bin(
x_example,group_example, alternative = "less",
  sorted_group = c(1:4, 'a','b'),num_needed_for_test = 2)
#>    Comparison                                                 ResponseStats
#> 1       1 < 2   7/24 = 29.2% (14.9%, 49.2%) vs. 6/25 = 24.0% (11.5%, 43.4%)
#> 2       1 < 3  7/24 = 29.2% (14.9%, 49.2%) vs. 20/25 = 80.0% (60.9%, 91.1%)
#> 3       1 < 4  7/24 = 29.2% (14.9%, 49.2%) vs. 16/25 = 64.0% (44.5%, 79.8%)
#> 4       1 < a      7/24 = 29.2% (14.9%, 49.2%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 5       1 < b  7/24 = 29.2% (14.9%, 49.2%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 6       2 < 3  6/25 = 24.0% (11.5%, 43.4%) vs. 20/25 = 80.0% (60.9%, 91.1%)
#> 7       2 < 4  6/25 = 24.0% (11.5%, 43.4%) vs. 16/25 = 64.0% (44.5%, 79.8%)
#> 8       2 < a      6/25 = 24.0% (11.5%, 43.4%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 9       2 < b  6/25 = 24.0% (11.5%, 43.4%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 10      3 < 4 20/25 = 80.0% (60.9%, 91.1%) vs. 16/25 = 64.0% (44.5%, 79.8%)
#> 11      3 < a     20/25 = 80.0% (60.9%, 91.1%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 12      3 < b 20/25 = 80.0% (60.9%, 91.1%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 13      4 < a     16/25 = 64.0% (44.5%, 79.8%) vs. 0/2 = 0.0% (0.0%, 65.8%)
#> 14      4 < b 16/25 = 64.0% (44.5%, 79.8%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#> 15      a < b     0/2 = 0.0% (0.0%, 65.8%) vs. 2/2 = 100.0% (34.2%, 100.0%)
#>    ResponseTest PerfectSeparation
#> 1  9.997601e-01             FALSE
#> 2  1.966412e-04             FALSE
#> 3  8.559910e-03             FALSE
#> 4  1.000000e+00             FALSE
#> 5  6.895733e-02             FALSE
#> 6  4.532387e-05             FALSE
#> 7  2.491572e-03             FALSE
#> 8  1.000000e+00             FALSE
#> 9  6.623440e-02             FALSE
#> 10 1.000000e+00             FALSE
#> 11 1.000000e+00             FALSE
#> 12 4.509185e-01             FALSE
#> 13 1.000000e+00             FALSE
#> 14 2.658420e-01             FALSE
#> 15 6.250000e-02              TRUE

# Examples with Real World Data
library(dplyr)

# BAMA Assay Data Example
data(exampleData_BAMA)

## Group Comparison
group_testing <- exampleData_BAMA |>
   group_by(antigen, visitno) |>
   group_modify(~ as.data.frame(
       pairwise_test_bin(x = .$response, group = .$group,
               method = 'barnard', alternative = 'less',
               num_needed_for_test = 3, digits = 1,
               trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#> "sorted_group" not specified so testing in following order: 
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "sorted_group" not specified so testing in following order: 
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "sorted_group" not specified so testing in following order: 
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "sorted_group" not specified so testing in following order: 
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "sorted_group" not specified so testing in following order: 
#> "sorted_group" not specified so testing in following order: 
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "sorted_group" not specified so testing in following order: 


## Timepoint Comparison
timepoint_testing <- exampleData_BAMA |>
   group_by(antigen, group) |>
   group_modify(~ as.data.frame(
       pairwise_test_bin(x = .$response, group = .$visitno, id = .$pubID,
               method = 'mcnemar', num_needed_for_test = 3, digits = 1,
               trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned

# ICS Assay Data Example
data(exampleData_ICS)

## Group Comparison
group_testing <- exampleData_ICS |>
   group_by(Stim, Parent, Population, Visit) |>
   group_modify(~ as.data.frame(
       pairwise_test_bin(x = .$response, group = .$Group , alternative = 'greater',
               method = 'barnard', num_needed_for_test = 3, digits = 1,
               trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "sorted_group" not specified so testing in following order: 
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)

## Timepoint Comparison
timepoint_testing <- exampleData_ICS |>
   group_by(Stim, Parent, Population, Group) |>
   group_modify(~ as.data.frame(
       pairwise_test_bin(x = .$response, group = .$Visit, id = .$pubID,
               method = 'mcnemar', num_needed_for_test = 3, digits = 1,
               trailing_zeros = TRUE, sep_val = ' vs. ', verbose = TRUE)))
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> x does not have at least 3 non missing values per group, so no test run (ResponseTest=NA returned)
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
#> "x" only has 1 distinct value when considering non-missing values of "y", so p=1 returned
```
