# Binomial confidence intervals

**\[stable\]**

Wrapper for
[binom::binom.confint](https://rdrr.io/pkg/binom/man/binom.confint.html)

## Usage

``` r
binom_ci(x, conf.level = 0.95, methods = "wilson", ...)
```

## Arguments

- x:

  vector of type integer (0/1) or logical (TRUE/FALSE)

- conf.level:

  confidence level (between 0 and 1). Default is 0.95.

- methods:

  which method to use to construct the interval. Any combination of
  c("exact", "ac", "asymptotic", "wilson", "prop.test", "bayes",
  "logit", "cloglog", "probit") is allowed or "all". Default is
  "wilson".

- ...:

  Additional arguments to be passed to
  [binom::binom.bayes](https://rdrr.io/pkg/binom/man/binom.bayes.html)

## Value

data.frame with with mean (`mean`), and bounds of confidence interval
(`lower`, `upper`)

Returns a data frame with the following columns:

- `method` - method(s) selected

- `x` - number of successes in the binomial experiment

- `n` - number of independent trials in the binomial experiment

- `mean` - success proportion mean

- `lower` - success proportion lower bound

- `upper` - success proportion upper bound

## Details

See
[binom::binom.confint](https://rdrr.io/pkg/binom/man/binom.confint.html)
for method details

## Examples

``` r
x <- c(rep(0, 500), rep(1, 500))
binom_ci(x, conf.level = .90, methods = 'all')
#>           method   x    n mean     lower     upper
#> 1  agresti-coull 500 1000  0.5 0.4740277 0.5259723
#> 2     asymptotic 500 1000  0.5 0.4739926 0.5260074
#> 3          bayes 500 1000  0.5 0.4740167 0.5259833
#> 4        cloglog 500 1000  0.5 0.4737084 0.5256940
#> 5          exact 500 1000  0.5 0.4735177 0.5264823
#> 6          logit 500 1000  0.5 0.4740160 0.5259840
#> 7         probit 500 1000  0.5 0.4740110 0.5259890
#> 8        profile 500 1000  0.5 0.4740102 0.5259898
#> 9            lrt 500 1000  0.5 0.4740164 0.5259836
#> 10     prop.test 500 1000  0.5 0.4690696 0.5309304
#> 11        wilson 500 1000  0.5 0.4740277 0.5259723
```
