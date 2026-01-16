# Continuous Variable Compared to Binary Variable Test (VISC)

Either Wilcox or T-Test Performed, for unpaired or paired data

## Usage

``` r
two_samp_cont_test(
  x,
  y,
  method = c("wilcox", "t.test"),
  paired = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  numeric vector (can include NA values).

- y:

  vector with only 2 levels (can include NA values unless
  `paired = TRUE`).

- method:

  what test to run (wilcox or t-test).

- paired:

  a logical indicating whether you want a paired test.

- verbose:

  a logical variable indicating if warnings and messages should be
  displayed.

- ...:

  parameters to pass to wilcox_test or t.test functions. For example the
  testing direction (`alternative`) in either call or the `var.equal` in
  the t.test function.

## Value

p-value for comparing x at the different levels of y.

## Details

Runs `wilcox_test()` in the coin package, with "exact" distribution and
mid-ranks ties method.

For one sided tests if `y` is a factor variable the level order is
respected, otherwise the levels will set to alphabetical order (i.e. if
`alternative = less` then testing a \< b ).

If `paired = TRUE` assumes the first observations of the first group
matches the first observation of the second group, and so on. Also if
`paired = TRUE` then `y` must have the same number of samples for each
level.

## Examples

``` r
set.seed(5432322)
outcome <- c(rnorm(10,0,3), rnorm(10,3,3))
grp <- c(rep('a', 10), rep('b', 10))
two_samp_cont_test(x = outcome, y = grp, method = 'wilcox', paired = FALSE)
#> [1] 0.01854338
two_samp_cont_test(x = outcome, y = grp, method = 'wilcox', paired = TRUE)
#> [1] 0.06445312
two_samp_cont_test(x = outcome, y = grp, method = 't', paired = FALSE)
#> [1] 0.01890551
two_samp_cont_test(x = outcome, y = grp, method = 't', paired = TRUE)
#> [1] 0.03394079
```
