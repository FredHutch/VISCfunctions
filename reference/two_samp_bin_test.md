# Binary (Response) Variable Compared to Binary (Group) Variable Test (VISC)

Either Barnard, Fisher's, or Chi-sq test performed for unpaired data and
McNemar's test for paired data

## Usage

``` r
two_samp_bin_test(
  x,
  y,
  method = c("barnard", "fisher", "chi.sq", "mcnemar"),
  barnard_method = c("z-pooled", "z-unpooled", "boschloo", "santner and snell", "csm",
    "csm approximate", "csm modified"),
  alternative = c("two.sided", "less", "greater"),
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  vector with only 2 levels (can include NA values).

- y:

  vector with only 2 levels (can include NA values unless
  `method = 'mcnemar'`).

- method:

  what test to run, "barnard" (default), "fisher" , "chi.sq" , or
  "mcnemar")

- barnard_method:

  indicates the Barnard method for finding tables as or more extreme
  than the observed table: must be either "z-pooled", "z-unpooled",
  "santner and snell", "boschloo", "csm", "csm approximate", or "csm
  modified". Only used when `method = 'barnard'`

- alternative:

  a character string specifying the alternative hypothesis, must be one
  of "two.sided" (default), "greater" or "less". You can specify just
  the initial letter. Only "two.sided" available for
  `method = 'chi.sq' or 'mcnemar'`

- verbose:

  a logical variable indicating if warnings and messages should be
  displayed.

- ...:

  other parameters to pass to Exact::exact.test when running Barnard
  test

## Value

p-value for comparing x at the different levels of y.

## Details

For one sided tests if `y` is a factor variable the level order is
respected, otherwise the levels will set to alphabetical order (i.e. if
`alternative = less` then testing a \< b ).

If `method = 'mcnemar'` assumes the first observations of the first
group matches the first observation of the second group, and so on. Also
if `method = 'mcnemar'` then `y` must have the same number of samples
for each level.

If only one value of `x` than `p=1` is returned, however if only one
value of `y` than `p=NA` is returned. This is to match expactations
since normally y is a group variable and x is the outcome (i.e. if both
group response rates are 0\\ returned)

## Examples

``` r
set.seed(5432322)
outcome <- c(sample(0:1,10,replace = TRUE, prob = c(.75,.25)),
             sample(0:1,10,replace = TRUE, prob = c(.25,.75)))
grp <- c(rep('a', 10), rep('b', 10))
two_samp_bin_test(outcome, grp, method = 'barnard')
#> [1] 0.02277998
two_samp_bin_test(outcome, grp, 'fisher')
#> [1] 0.05727554
two_samp_bin_test(outcome, grp, 'chi.sq')
#> Warning: Chi-squared approximation may be incorrect
#> [1] 0.06076124
two_samp_bin_test(outcome, grp, 'mcnemar')
#> [1] 0.07363827
```
