# Correlation Test for Two Continuous Variables

This function is a wrapper for
[stats::cor.test](https://rdrr.io/r/stats/cor.test.html) function,
except if `method = "spearman"` is selected and there are ties in at
least one variable, in which case this is a wrapper for
coin::spearman_test employing the approximate method.

## Usage

``` r
cor_test(
  x,
  y,
  method = c("pearson", "kendall", "spearman"),
  seed = 68954857,
  nresample = 10000,
  exact = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  numeric vector (can include NA values).

- y:

  numeric vector (can include NA values).

- method:

  a character string indicating which correlation coefficient is to be
  used for the test. One of "pearson", "kendall", or "spearman", can be
  abbreviated to "p", "k", or "s".

- seed:

  seed (only used if `method = "spearman"`).

- nresample:

  a positive integer, the number of Monte Carlo replicates used for the
  computation of the approximative reference distribution. Defaults
  to 10000. (only used if `method = "spearman"`).

- exact:

  should exact method be used. Ignored if `method = "pearson"` or if
  `method = "spearman"` and there are ties in x or y.

- verbose:

  a logical variable indicating if warnings and messages should be
  displayed.

- ...:

  parameters passed to
  [stats::cor.test](https://rdrr.io/r/stats/cor.test.html) or
  coin::spearman_test

## Value

correlation estimate p value.

## Details

The three methods each estimate the association between paired samples
and compute a test of the value being zero. They use different measures
of association, all in the range \[-1, 1\] with 0 indicating no
association. These are sometimes referred to as tests of no correlation,
but that term is often confined to the default method.

If method is "pearson", the test statistic is based on Pearson's product
moment correlation coefficient cor(x, y) and follows a t distribution
with length(x)-2 degrees of freedom if the samples follow independent
normal distributions. If there are at least 4 complete pairs of
observation, an asymptotic confidence interval is given based on
Fisher's Z transform.

If method is "kendall" or "spearman", Kendall's tau or Spearman's rho
statistic is used to estimate a rank-based measure of association. These
tests may be used if the data do not necessarily come from a bivariate
normal distribution.

The preferred method for a Spearman test is using the exact method,
unless computation time is too high. This preferred method is obtained
though [stats::cor.test](https://rdrr.io/r/stats/cor.test.html) with
`exact = TRUE`. When there are ties in either variable there is no exact
method possible. Unfortunately if there are any ties the
[stats::cor.test](https://rdrr.io/r/stats/cor.test.html) function
switches to the asymptotic method, which is especially troubling with
small sample sizes. If there are ties `cor_test` will switch to the
approximate method available in the coin::spearman_test.

## Examples

``` r
set.seed(5432322)
x <- rnorm(20,0,3)
y <- x + rnorm(20,0,5)
cor_test(x,y, method = 'pearson')
#> [1] 0.003244366
cor_test(x,y, method = 'kendall')
#> [1] 0.0237345
cor_test(x,y, method = 'spearman')
#> [1] 0.01473093
# Adding ties
cor_test(c(x,x), c(y,y), method = 'spearman',
         seed = 1, nresample = 10000, verbose = TRUE)
#> Either "x" or "y" has ties, so using approximate method.
#> [1] 7e-04
```
